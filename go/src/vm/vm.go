package vm

import (
	"fmt"

	"github.com/jabley/monkey/code"
	"github.com/jabley/monkey/compiler"
	"github.com/jabley/monkey/object"
)

// StackSize is the hard limit for how deep we can go
const StackSize = 2048

// GlobalsSize is the maximum number of global variables we can support. This is tied to how wide
// the operands of OpSetGlobal and OpGetGlobal are (currently `[]int{2}}`)
const GlobalsSize = 1 << 16

// MaxFrames limits how many frames we can execute
const MaxFrames = 1024

// Global singleton booleans to allow pointer comparison and minimise memory use/gc pressure.
var (
	True  = &object.Boolean{Value: true}
	False = &object.Boolean{Value: false}

	Null = &object.Null{}
)

// VM is responsible for executing bytecode. It will do the fetch, decode, and execute of instructions.
type VM struct {
	constants []object.Object
	globals   []object.Object

	stack []object.Object
	sp    int // aka stack pointer. Always points to the next value. Top of stack is `stack[sp-1]`

	frames      []*Frame
	framesIndex int
}

// New returns a new VM ready to execute the specified bytecode.
func New(bytecode *compiler.Bytecode) *VM {
	mainFn := &object.CompiledFunction{Instructions: bytecode.Instructions}
	mainClosure := &object.Closure{Fn: mainFn}
	mainFrame := NewFrame(mainClosure, 0)

	frames := make([]*Frame, MaxFrames)
	frames[0] = mainFrame

	return &VM{
		constants: bytecode.Constants,
		globals:   make([]object.Object, GlobalsSize),

		stack: make([]object.Object, StackSize),
		sp:    0,

		frames:      frames,
		framesIndex: 1,
	}
}

// NewWithGlobalsStore returns a new VM which can reuse globals from a previous compilation. This is
// needed by the REPL.
func NewWithGlobalsStore(bytecode *compiler.Bytecode, globals []object.Object) *VM {
	vm := New(bytecode)
	vm.globals = globals
	return vm
}

// LastPoppedStackElem returns the result of the last expression. This is to ensure that the
// expressions are removed from the stack and the stack doesn't grow in an unbounded fashion.
func (vm *VM) LastPoppedStackElem() object.Object {
	return vm.stack[vm.sp]
}

// StackTop returns the object.Object at the top of the stack, or nil if there isn't one.
func (vm *VM) StackTop() object.Object {
	if vm.sp == 0 {
		return nil
	}
	return vm.stack[vm.sp-1]
}

// Run does the fetch, decode, and execute cycle on the instructions
func (vm *VM) Run() error {
	var ip int
	var ins code.Instructions
	var op code.Opcode

	for vm.currentFrame().ip < len(vm.currentFrame().Instructions())-1 {
		vm.currentFrame().ip++

		ip = vm.currentFrame().ip
		ins = vm.currentFrame().Instructions()
		op = code.Opcode(ins[ip])

		switch op {
		case code.OpConstant:
			constIndex := code.ReadUint16(ins[ip+1:])
			vm.currentFrame().ip += 2

			err := vm.push(vm.constants[constIndex])
			if err != nil {
				return err
			}
		case code.OpAdd, code.OpSub, code.OpMul, code.OpDiv:
			err := vm.executeBinaryOperation(op)
			if err != nil {
				return err
			}
		case code.OpTrue:
			err := vm.push(True)
			if err != nil {
				return err
			}
		case code.OpFalse:
			err := vm.push(False)
			if err != nil {
				return err
			}
		case code.OpEqual, code.OpNotEqual, code.OpGreaterThan:
			err := vm.executeComparisonOperation(op)
			if err != nil {
				return err
			}
		case code.OpBang:
			if err := vm.executeBangOperator(); err != nil {
				return err
			}
		case code.OpMinus:
			if err := vm.executeMinusOperator(); err != nil {
				return err
			}
		case code.OpPop:
			vm.pop()
		case code.OpJump:
			pos := int(code.ReadUint16(ins[ip+1:]))
			vm.currentFrame().ip = pos - 1
		case code.OpJumpNotTruthy:
			pos := int(code.ReadUint16(ins[ip+1:]))
			vm.currentFrame().ip += 2

			condition := vm.pop()
			if !isTruthy(condition) {
				vm.currentFrame().ip = pos - 1
			}
		case code.OpNull:
			if err := vm.push(Null); err != nil {
				return err
			}
		case code.OpSetGlobal:
			globalIndex := code.ReadUint16(ins[ip+1:])
			vm.currentFrame().ip += 2

			vm.globals[globalIndex] = vm.pop()

		case code.OpGetGlobal:
			globalIndex := code.ReadUint16(ins[ip+1:])
			vm.currentFrame().ip += 2

			if err := vm.push(vm.globals[globalIndex]); err != nil {
				return err
			}

		case code.OpArray:
			numElements := int(code.ReadUint16(ins[ip+1:]))
			vm.currentFrame().ip += 2

			array := vm.buildArray(vm.sp-numElements, vm.sp)
			vm.sp = vm.sp - numElements

			if err := vm.push(array); err != nil {
				return err
			}

		case code.OpHash:
			numElements := int(code.ReadUint16(ins[ip+1:]))
			vm.currentFrame().ip += 2

			hash, err := vm.buildHash(vm.sp-numElements, vm.sp)
			if err != nil {
				return err
			}
			vm.sp = vm.sp - numElements

			if err := vm.push(hash); err != nil {
				return err
			}

		case code.OpIndex:
			index := vm.pop()
			left := vm.pop()

			if err := vm.executeIndexExpression(left, index); err != nil {
				return err
			}

		case code.OpCall:
			numArgs := code.ReadUint8(ins[ip+1:])
			vm.currentFrame().ip++

			if err := vm.executeCall(int(numArgs)); err != nil {
				return err
			}

		case code.OpReturnValue, code.OpReturn:
			if err := vm.returnValue(op == code.OpReturnValue); err != nil {
				return err
			}

		case code.OpSetLocal:
			localIndex := code.ReadUint8(ins[ip+1:])
			vm.currentFrame().ip++

			frame := vm.currentFrame()

			vm.stack[frame.basePointer+int(localIndex)] = vm.pop()

		case code.OpGetLocal:
			localIndex := code.ReadUint8(ins[ip+1:])
			vm.currentFrame().ip++

			frame := vm.currentFrame()

			if err := vm.push(vm.stack[frame.basePointer+int(localIndex)]); err != nil {
				return err
			}

		case code.OpGetBuiltIn:
			builtInIndex := code.ReadUint8(ins[ip+1:])
			vm.currentFrame().ip++

			definition := object.BuiltIns[builtInIndex]

			if err := vm.push(definition.BuiltIn); err != nil {
				return err
			}

		case code.OpClosure:
			constIndex := code.ReadUint16(ins[ip+1:])
			vm.currentFrame().ip += 2

			numFree := code.ReadUint8(ins[ip+3:])
			vm.currentFrame().ip++

			if err := vm.pushClosure(int(constIndex), int(numFree)); err != nil {
				return err
			}

		case code.OpGetFree:
			freeIndex := code.ReadUint8(ins[ip+1:])
			vm.currentFrame().ip++

			currentClosure := vm.currentFrame().cl
			if err := vm.push(currentClosure.Free[freeIndex]); err != nil {
				return err
			}
		}
	}

	return nil
}

func (vm *VM) buildArray(startIndex, endIndex int) object.Object {
	elements := make([]object.Object, endIndex-startIndex)

	for i := startIndex; i < endIndex; i++ {
		elements[i-startIndex] = vm.stack[i]
	}

	return &object.Array{Elements: elements}
}

func (vm *VM) buildHash(startIndex, endIndex int) (object.Object, error) {
	hashedPairs := make(map[object.HashKey]object.HashPair)

	for i := startIndex; i < endIndex; i += 2 {
		key := vm.stack[i]
		value := vm.stack[i+1]

		pair := object.HashPair{Key: key, Value: value}

		hashKey, ok := key.(object.Hashable)
		if !ok {
			return nil, fmt.Errorf("unusable as hashkey: %s", key.Type())
		}

		hashedPairs[hashKey.HashKey()] = pair
	}

	return &object.Hash{Pairs: hashedPairs}, nil
}

func (vm *VM) callBuiltIn(builtIn *object.BuiltIn, numArgs int) error {
	args := vm.stack[vm.sp-numArgs : vm.sp]

	result := builtIn.Fn(args...)
	vm.sp = vm.sp - numArgs - 1

	if result != nil {
		vm.push(result)
	} else {
		vm.push(Null)
	}

	return nil
}

func (vm *VM) callClosure(cl *object.Closure, numArgs int) error {
	fn := cl.Fn
	if numArgs != fn.NumParameters {
		return fmt.Errorf("wrong number of arguments: want=%d, got=%d", fn.NumParameters, numArgs)
	}

	frame := NewFrame(cl, vm.sp-numArgs)
	vm.pushFrame(frame)
	vm.sp = frame.basePointer + fn.NumLocals

	return nil
}

func (vm *VM) currentFrame() *Frame {
	return vm.frames[vm.framesIndex-1]
}

func (vm *VM) executeArrayIndex(array, index object.Object) error {
	arrayObject := array.(*object.Array)
	i := index.(*object.Integer).Value
	max := int64(len(arrayObject.Elements) - 1)

	// Bounds checking
	if i < 0 || i > max {
		return vm.push(Null)
	}

	return vm.push(arrayObject.Elements[i])
}

func (vm *VM) executeBangOperator() error {
	operand := vm.pop()

	switch operand {
	case True:
		return vm.push(False)
	case False:
		return vm.push(True)
	case Null:
		return vm.push(True)
	default:
		return vm.push(False)
	}
}

func (vm *VM) executeBinaryIntegerOperation(op code.Opcode, left, right object.Object) error {
	leftValue := left.(*object.Integer).Value
	rightValue := right.(*object.Integer).Value

	var result int64

	switch op {
	case code.OpAdd:
		result = leftValue + rightValue
	case code.OpSub:
		result = leftValue - rightValue
	case code.OpMul:
		result = leftValue * rightValue
	case code.OpDiv:
		result = leftValue / rightValue
	default:
		return fmt.Errorf("unknown integer operator: %d", op)
	}

	return vm.push(&object.Integer{Value: result})
}

func (vm *VM) executeBinaryOperation(op code.Opcode) error {
	right := vm.pop()
	left := vm.pop()

	ltype := left.Type()
	rtype := right.Type()

	if ltype == object.IntegerObj && rtype == object.IntegerObj {
		return vm.executeBinaryIntegerOperation(op, left, right)
	} else if ltype == object.StringObj && rtype == object.StringObj {
		return vm.executeBinaryStringOperation(op, left, right)
	}

	return fmt.Errorf("unsupported types for binary operation: %s %s", ltype, rtype)
}

func (vm *VM) executeBinaryStringOperation(op code.Opcode, left, right object.Object) error {
	leftValue := left.(*object.String).Value
	rightValue := right.(*object.String).Value

	var result string

	switch op {
	case code.OpAdd:
		result = leftValue + rightValue
	default:
		return fmt.Errorf("unknown string operator: %d", op)
	}

	return vm.push(&object.String{Value: result})
}

func (vm *VM) executeCall(numArgs int) error {
	callee := vm.stack[vm.sp-1-numArgs]

	switch callee := callee.(type) {
	case *object.Closure:
		return vm.callClosure(callee, numArgs)
	case *object.BuiltIn:
		return vm.callBuiltIn(callee, numArgs)
	default:
		return fmt.Errorf("calling non-function/non-built-in")
	}
}

func (vm *VM) executeComparisonOperation(op code.Opcode) error {
	right := vm.pop()
	left := vm.pop()

	if left.Type() == object.IntegerObj || right.Type() == object.IntegerObj {
		return vm.executeIntegerComparison(op, left, right)
	}

	switch op {
	case code.OpEqual:
		return vm.push(nativeBoolToBooleanObject(left == right))
	case code.OpNotEqual:
		return vm.push(nativeBoolToBooleanObject(left != right))
	default:
		return fmt.Errorf("unknown operator: %d (%s %s)", op, left.Type(), right.Type())
	}
}

func (vm *VM) executeHashIndex(hash, index object.Object) error {
	hashObject := hash.(*object.Hash)
	key, ok := index.(object.Hashable)
	if !ok {
		return fmt.Errorf("unusable as hashkey: %s", index.Type())
	}

	pair, ok := hashObject.Pairs[key.HashKey()]
	if !ok {
		return vm.push(Null)
	}

	return vm.push(pair.Value)
}

func (vm *VM) executeIndexExpression(left, index object.Object) error {
	switch {
	case left.Type() == object.ArrayObj && index.Type() == object.IntegerObj:
		return vm.executeArrayIndex(left, index)
	case left.Type() == object.HashObj:
		return vm.executeHashIndex(left, index)
	default:
		return fmt.Errorf("index operator not supported: %s", left.Type())
	}
}

func (vm *VM) executeIntegerComparison(op code.Opcode, left, right object.Object) error {
	lvalue := left.(*object.Integer).Value
	rvalue := right.(*object.Integer).Value

	switch op {
	case code.OpEqual:
		return vm.push(nativeBoolToBooleanObject(lvalue == rvalue))
	case code.OpNotEqual:
		return vm.push(nativeBoolToBooleanObject(lvalue != rvalue))
	case code.OpGreaterThan:
		return vm.push(nativeBoolToBooleanObject(lvalue > rvalue))
	default:
		return fmt.Errorf("unknown operator: %d", op)
	}
}

func (vm *VM) executeMinusOperator() error {
	operand := vm.pop()

	if operand.Type() != object.IntegerObj {
		return fmt.Errorf("unsupported type for negation: %s", operand.Type())
	}

	value := operand.(*object.Integer).Value
	return vm.push(&object.Integer{Value: -value})
}

func (vm *VM) pop() object.Object {
	o := vm.stack[vm.sp-1]
	vm.sp--
	return o
}

func (vm *VM) push(o object.Object) error {
	if vm.sp >= StackSize {
		return fmt.Errorf("Stack overflow")
	}

	vm.stack[vm.sp] = o
	vm.sp++

	return nil
}

func (vm *VM) popFrame() *Frame {
	vm.framesIndex--
	return vm.frames[vm.framesIndex]
}

func (vm *VM) pushClosure(constIndex, numFree int) error {
	constant := vm.constants[constIndex]
	function, ok := constant.(*object.CompiledFunction)
	if !ok {
		return fmt.Errorf("not a function: %+v", constant)
	}

	free := make([]object.Object, numFree)
	for i := 0; i < numFree; i++ {
		free[i] = vm.stack[vm.sp-numFree+i]
	}
	vm.sp = vm.sp - numFree

	closure := &object.Closure{Fn: function, Free: free}
	return vm.push(closure)
}

func (vm *VM) pushFrame(f *Frame) {
	vm.frames[vm.framesIndex] = f
	vm.framesIndex++
}

func (vm *VM) returnValue(hasReturnValue bool) error {
	var returnValue object.Object

	if hasReturnValue {
		returnValue = vm.pop()
	} else {
		returnValue = Null
	}

	// Pop the frame and update the stack pointer. The additional 1 means that we don't
	// need to pop the object.CompiledFunction too – just move the stack pointer.
	frame := vm.popFrame()
	vm.sp = frame.basePointer - 1

	return vm.push(returnValue)
}

func isTruthy(obj object.Object) bool {
	switch obj := obj.(type) {
	case *object.Boolean:
		return obj.Value
	case *object.Null:
		return false
	default:
		return true
	}
}

func nativeBoolToBooleanObject(b bool) *object.Boolean {
	if b {
		return True
	}
	return False
}
