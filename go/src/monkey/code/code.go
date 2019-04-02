package code

import (
	"encoding/binary"
	"fmt"
)

// Instructions is a stream of bytecode instructions.
type Instructions []byte

// Opcode is the first byte in an Instruction, followed by an optional number of operands.
type Opcode byte

// Opcodes
const (
	OpConstant Opcode = iota
)

// Definition provides more context about each opcode
type Definition struct {
	Name          string
	OperandWidths []int
}

var definitions = map[Opcode]*Definition{
	OpConstant: {"OpConstant", []int{2}},
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
		}
		offset += width
	}

	return instruction
}
