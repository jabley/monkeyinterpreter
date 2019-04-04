package vm

import (
	"fmt"
	"monkey/code"
	"monkey/compiler"
	"monkey/object"
)

// StackSize is the hard limit for how deep we can go
const StackSize = 2048

// VM is responsible for executing bytecode. It will do the fetch, decode, and execute of instructions.
type VM struct {
	constants    []object.Object
	instructions code.Instructions

	stack []object.Object
	sp    int // aka stack pointer. Always points to the next value. Top of stack is `stack[sp-1]`
}

// New returns a new VM ready to execute the specified bytecode.
func New(bytecode *compiler.Bytecode) *VM {
	return &VM{
		instructions: bytecode.Instructions,
		constants:    bytecode.Constants,

		stack: make([]object.Object, StackSize),
		sp:    0,
	}
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
	for ip := 0; ip < len(vm.instructions); ip++ {
		op := code.Opcode(vm.instructions[ip])

		switch op {
		case code.OpConstant:
			constIndex := code.ReadUint16(vm.instructions[ip+1:])
			ip += 2

			err := vm.push(vm.constants[constIndex])
			if err != nil {
				return err
			}
		case code.OpAdd, code.OpSub, code.OpMul, code.OpDiv:
			err := vm.executeBinaryOperation(op)
			if err != nil {
				return err
			}
		case code.OpPop:
			vm.pop()
		}
	}

	return nil
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
	}

	return fmt.Errorf("unsupported types for binary operation: %s %s", ltype, rtype)
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
