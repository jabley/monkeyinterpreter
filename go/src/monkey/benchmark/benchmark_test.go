package benchmark

import (
	"monkey/ast"
	"monkey/compiler"
	"monkey/evaluator"
	"monkey/lexer"
	"monkey/object"
	"monkey/parser"
	"monkey/vm"
	"testing"
)

func BenchmarkEvaluator(b *testing.B) {
	program := parse()
	for i := 0; i < b.N; i++ {
		env := object.NewEnvironment()
		result := evaluator.Eval(program, env)
		testIntegerObject(b, int64(9227465), result)
	}
}

func BenchmarkVM(b *testing.B) {
	program := parse()
	for i := 0; i < b.N; i++ {
		comp := compiler.New()
		err := comp.Compile(program)
		if err != nil {
			b.Fatalf("compiler error: %s", err)
		}
		machine := vm.New(comp.Bytecode())
		if err := machine.Run(); err != nil {
			b.Fatalf("%v", err)
		}
		result := machine.LastPoppedStackElem()
		testIntegerObject(b, int64(9227465), result)
	}
}

func parse() *ast.Program {
	input := `
	let fibonacci = fn(x) {
		if (x == 0) {
			0
		} else {
			if (x == 1) {
				return 1;
			} else {
				fibonacci(x - 1) + fibonacci(x - 2);
			}
		}
	};
	fibonacci(35); 
	`
	l := lexer.New(input)
	p := parser.New(l)

	return p.ParseProgram()
}

func testIntegerObject(t *testing.B, expected int64, actual object.Object) {
	t.Helper()

	result, ok := actual.(*object.Integer)
	if !ok {
		t.Fatalf("object is not Integer. got=%T (%+v)", actual, actual)
	}

	if result.Value != expected {
		t.Fatalf("object has the wrong value. got=%d, want=%d", result.Value, expected)
	}
}
