package code

import (
	"encoding/binary"
	"fmt"
	"strings"
)

// Instructions is a stream of bytecode instructions.
type Instructions []byte

func (ins Instructions) String() string {
	var out strings.Builder

	i := 0

	for i < len(ins) {
		def, err := Lookup(ins[i])

		if err != nil {
			fmt.Fprintf(&out, "ERROR: %s\n", err)
			continue
		}

		operands, read := ReadOperands(def, ins[i+1:])
		fmt.Fprintf(&out, "%04d %s\n", i, ins.fmtInstruction(def, operands))

		i += 1 + read
	}

	return out.String()
}

func (ins Instructions) fmtInstruction(def *Definition, operands []int) string {
	operandCount := len(def.OperandWidths)

	if len(operands) != operandCount {
		return fmt.Sprintf("ERROR: operand len %d does not match defined %d\n ", len(operands), operandCount)
	}

	switch operandCount {
	case 0:
		return def.Name
	case 1:
		return fmt.Sprintf("%s %d", def.Name, operands[0])
	}

	return fmt.Sprintf("ERROR: unhandled operandCount for %s\n ", def.Name)

}

// Opcode is the first byte in an Instruction, followed by an optional number of operands.
type Opcode byte

// Opcodes
const (
	OpConstant Opcode = iota
	OpAdd
	OpSub
	OpMul
	OpDiv

	OpTrue
	OpFalse

	OpEqual
	OpNotEqual
	OpGreaterThan

	OpMinus
	OpBang

	OpJumpNotTruthy
	OpJump

	OpPop

	OpNull

	OpSetGlobal
	OpGetGlobal

	OpArray
	OpHash
	OpIndex

	OpCall        // tell the VM to start executing the *object.CompiledFunction sitting on top of the stack
	OpReturnValue // tell the VM to return the value on top of the stack to the calling context and to resume execution there
	OpReturn      // similar to OpReturnValue except there is no explicit return value to return but an implicit vm.Null

	OpSetLocal
	OpGetLocal
)

// Definition provides more context about each opcode
type Definition struct {
	Name          string
	OperandWidths []int
}

var definitions = map[Opcode]*Definition{
	OpConstant:      {"OpConstant", []int{2}},
	OpAdd:           {"OpAdd", []int{}},
	OpSub:           {"OpSub", []int{}},
	OpMul:           {"OpMul", []int{}},
	OpDiv:           {"OpDiv", []int{}},
	OpTrue:          {"OpTrue", []int{}},
	OpFalse:         {"OpFalse", []int{}},
	OpEqual:         {"OpEqual", []int{}},
	OpNotEqual:      {"OpNotEqual", []int{}},
	OpGreaterThan:   {"OpGreaterThan", []int{}},
	OpMinus:         {"OpMinus", []int{}},
	OpBang:          {"OpBang", []int{}},
	OpJumpNotTruthy: {"OpJumpNotTruthy", []int{2}},
	OpJump:          {"OpJump", []int{2}},
	OpPop:           {"OpPop", []int{}},
	OpNull:          {"OpNull", []int{}},
	OpSetGlobal:     {"OpSetGlobal", []int{2}},
	OpGetGlobal:     {"OpGetGlobal", []int{2}},
	OpArray:         {"OpArray", []int{2}}, // This limits an Array to only contain (1 << 16) -1 = 65535 elements in an array
	OpHash:          {"OpHash", []int{2}},
	OpIndex:         {"OpIndex", []int{}},
	OpCall:          {"OpCall", []int{1}},
	OpReturnValue:   {"OpReturnValue", []int{}},
	OpReturn:        {"OpReturn", []int{}},
	OpSetLocal:      {"OpSetLocal", []int{1}}, // This limits local bindings to only 1 << 8 == 256 per function.
	OpGetLocal:      {"OpGetLocal", []int{1}},
}

// Lookup returns the human-readable name of the opcode, or an error if the opcode isn't defined
func Lookup(op byte) (*Definition, error) {
	def, ok := definitions[Opcode(op)]
	if !ok {
		return nil, fmt.Errorf("opcode %d undefined", op)
	}
	return def, nil
}

// Make creates a single bytecode instruction of the opcode and an optional number of operands
func Make(op Opcode, operands ...int) []byte {
	def, err := Lookup(byte(op))
	if err != nil {
		return []byte{}
	}

	instructionLen := 1
	for _, w := range def.OperandWidths {
		instructionLen += w
	}

	instruction := make([]byte, instructionLen)
	instruction[0] = byte(op)
	offset := 1

	for i, o := range operands {
		width := def.OperandWidths[i]
		switch width {
		case 2:
			binary.BigEndian.PutUint16(instruction[offset:], uint16(o))
		case 1:
			instruction[offset] = byte(o)
		}
		offset += width
	}

	return instruction
}

// ReadOperands decodes the operands of a bytecode instruction. It is the inverse of Make
func ReadOperands(def *Definition, ins Instructions) ([]int, int) {
	operands := make([]int, len(def.OperandWidths))

	offset := 0

	for i, width := range def.OperandWidths {
		switch width {
		case 2:
			operands[i] = int(ReadUint16(ins[offset:]))
		case 1:
			operands[i] = int(ReadUint8(ins[offset:]))
		}

		offset += width
	}

	return operands, offset
}

// ReadUint16 reads a uint16 from a byte slice
func ReadUint16(ins []byte) uint16 {
	return binary.BigEndian.Uint16(ins)
}

// ReadUint8 reads a uint8 from the byte slice
func ReadUint8(ins []byte) uint8 {
	return uint8(ins[0])
}
