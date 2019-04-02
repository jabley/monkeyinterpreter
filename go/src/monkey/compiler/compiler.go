package compiler

import (
	"monkey/ast"
	"monkey/code"
	"monkey/object"
)

// Compiler is responsible for compiling an AST into bytecode.
type Compiler struct {
	instructions code.Instructions
	constants    []object.Object
}

// New creates a new Compiler ready for use
func New() *Compiler {
	return &Compiler{
		instructions: code.Instructions{},
		constants:    []object.Object{},
	}
}

// Compile compiles an AST into bytecode
func (c *Compiler) Compile(node ast.Node) error {
	return nil
}

// Bytecode returns the Bytecode representing the compiled *ast.Node
func (c *Compiler) Bytecode() *Bytecode {
	return &Bytecode{
		Instructions: c.instructions,
		Constants:    c.constants,
	}
}

// Bytecode is a compiled representation of an *ast.Node
type Bytecode struct {
	Instructions code.Instructions
	Constants    []object.Object
}
