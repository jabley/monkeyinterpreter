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

func TestInstructionsString(t *testing.T) {
	instructions := []Instructions{
		Make(OpConstant, 1),
		Make(OpConstant, 2),
		Make(OpConstant, 65535),
	}

	expected := `0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535
`
	concatted := Instructions{}

	for _, ins := range instructions {
		concatted = append(concatted, ins...)
	}

	if concatted.String() != expected {
		t.Errorf("instructions wrongly formatted. want=%q, got=%q", expected, concatted.String())
	}
}
