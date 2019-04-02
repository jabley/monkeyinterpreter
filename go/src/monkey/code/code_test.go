package code

import (
	"fmt"
	"testing"
)

func TestMake(t *testing.T) {
	tests := []struct {
		op       Opcode
		operands []int
		expected []byte
	}{
		{OpConstant, []int{65534}, []byte{byte(OpConstant), 255, 254}},
	}

	for _, tt := range tests {
		t.Run(fmt.Sprintf(`%v`, tt.op), func(t *testing.T) {
			instruction := Make(tt.op, tt.operands...)

			if len(instruction) != len(tt.expected) {
				t.Fatalf("instruction has wrong length. want=%d, got=%d", len(tt.expected), len(instruction))
			}

			for i, b := range tt.expected {
				if instruction[i] != tt.expected[i] {
					t.Fatalf("wrong byte at pos %d. want=%d, got=%d", i, b, instruction[i])
				}
			}
		})
	}
}
