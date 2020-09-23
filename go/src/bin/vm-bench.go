package main

import (
	"github.com/jabley/monkey/ast"
	"github.com/jabley/monkey/compiler"
	"github.com/jabley/monkey/lexer"
	"github.com/jabley/monkey/parser"
	"github.com/jabley/monkey/vm"

	"fmt"
	"log"
)

func main() {
	program := parse()
	comp := compiler.New()
	err := comp.Compile(program)
	if err != nil {
		log.Fatalf("compiler error: %s", err)
	}
	machine := vm.New(comp.Bytecode())
	if err := machine.Run(); err != nil {
		log.Fatalf("%v", err)
	}
	result := machine.LastPoppedStackElem()
	fmt.Printf("%s\n", result.Inspect())
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
