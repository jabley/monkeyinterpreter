package compiler

import (
	"fmt"
	"monkey/ast"
	"monkey/code"
	"monkey/lexer"
	"monkey/object"
	"monkey/parser"
	"testing"
)

type compilerTestCase struct {
	input                string
	expectedConstants    []interface{}
	expectedInstructions []code.Instructions
}

func TestIntegerArithmetic(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "1 + 2",
			expectedConstants: []interface{}{1, 2},
			expectedInstructions: []code.Instructions{
				code.Make(code.OpConstant, 0),
				code.Make(code.OpConstant, 1),
				code.Make(code.OpAdd),
			},
		},
	}

	runCompilerTests(t, tests)
}

func runCompilerTests(t *testing.T, tests []compilerTestCase) {
	t.Helper()

	for _, tt := range tests {
		t.Run(fmt.Sprintf(`%v`, tt.input), func(t *testing.T) {
			program := parse(tt.input)

			compiler := New()
			err := compiler.Compile(program)
			if err != nil {
				t.Fatalf("compiler error: %s", err)
			}

			bytecode := compiler.Bytecode()

			testInstructions(t, tt.expectedInstructions, bytecode.Instructions)
			testConstants(t, tt.expectedConstants, bytecode.Constants)
		})
	}
}

func testInstructions(t *testing.T, expected []code.Instructions, actual code.Instructions) {
	t.Helper()

	concatted := concatInstructions(expected)

	if len(actual) != len(concatted) {
		t.Fatalf("wrong instructions length. want=%q, got=%q", concatted, actual)
	}

	for i, ins := range concatted {
		if actual[i] != ins {
			t.Fatalf("wrong instruction at %d. want=%q, got=%q", i, ins, actual[i])
		}
	}
}

func testConstants(t *testing.T, expected []interface{}, actual []object.Object) {
	t.Helper()

	if len(expected) != len(actual) {
		t.Fatalf("wrong number of constants. got=%d, want=%d", len(actual), len(expected))
	}

	for i, constant := range expected {
		switch constant := constant.(type) {
		case int:
			testIntegerObject(t, int64(constant), actual[i])
		}
	}
}

func testIntegerObject(t *testing.T, expected int64, actual object.Object) {
	t.Helper()

	result, ok := actual.(*object.Integer)
	if !ok {
		t.Fatalf("object is not Integer. got=%T (%+v)", actual, actual)
	}

	if result.Value != expected {
		t.Fatalf("object has the wrong value. got=%d, want=%d", result.Value, expected)
	}
}

func concatInstructions(s []code.Instructions) code.Instructions {
	out := code.Instructions{}

	for _, ins := range s {
		out = append(out, ins...)
	}

	return out
}

func parse(input string) *ast.Program {
	l := lexer.New(input)
	p := parser.New(l)
	return p.ParseProgram()
}