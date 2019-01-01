package object

import (
	"fmt"
	"monkey/ast"
	"strings"
)

// Type is the enum for different types of object in our object system.
type Type string

// The different types of object supported.
const (
	BooleanObj     = "BOOLEAN"
	BuiltInObj     = "BUILTIN"
	ErrorObj       = "ERROR"
	FunctionObj    = "FUNCTION"
	IntegerObj     = "INTEGER"
	NullObj        = "NULL"
	ReturnValueObj = "RETURN_VALUE"
	StringObj      = "STRING"
)

// Object is the common interface for our object system.
type Object interface {
	Type() Type
	Inspect() string
}

// Boolean is the boolean type in Monkey.
type Boolean struct {
	Value bool
}

// Inspect implementation of the Object interface
func (b *Boolean) Inspect() string {
	return fmt.Sprintf("%t", b.Value)
}

// Type implementation of the Object interface
func (b *Boolean) Type() Type {
	return BooleanObj
}

// BuiltInFunction is core functions built-in to the Monkey language.
type BuiltInFunction func(args ...Object) Object

// BuiltIn is the built-in function type in Monkey.
type BuiltIn struct {
	Fn BuiltInFunction
}

// Inspect implementation of the Object interface
func (b *BuiltIn) Inspect() string {
	return "builtin function"
}

// Type implementation of the Object interface
func (b *BuiltIn) Type() Type {
	return BuiltInObj
}

// Error is the error type in Monkey.
type Error struct {
	Message string
}

// Inspect implementation of the Object interface
func (e *Error) Inspect() string {
	return "ERROR: " + e.Message
}

// Type implementation of the Object interface
func (e *Error) Type() Type {
	return ErrorObj
}

// Function is the function type in Monkey. First class functions!
type Function struct {
	Parameters []*ast.Identifier
	Body       *ast.BlockStatement
	Env        *Environment
}

// Inspect implementation of the Object interface
func (f *Function) Inspect() string {
	var sb strings.Builder

	params := []string{}

	for _, p := range f.Parameters {
		params = append(params, p.String())
	}

	sb.WriteString("fn")
	sb.WriteString("(")
	sb.WriteString(strings.Join(params, ", "))
	sb.WriteString(") \n")
	sb.WriteString(f.Body.String())
	sb.WriteString("\n}")

	return sb.String()
}

// Type implementation of the Object interface
func (f *Function) Type() Type {
	return FunctionObj
}

// Integer is the integer type in Monkey.
type Integer struct {
	Value int64
}

// Inspect implementation of the Object interface
func (i *Integer) Inspect() string {
	return fmt.Sprintf("%d", i.Value)
}

// Type implementation of the Object interface
func (i *Integer) Type() Type {
	return IntegerObj
}

// Null is the Null type in Monkey.
type Null struct{}

// Inspect implementation of the Object interface
func (n *Null) Inspect() string {
	return "null"
}

// Type implementation of the Object interface
func (n *Null) Type() Type {
	return NullObj
}

// ReturnValue is the wrapper for return values in Monkey.
type ReturnValue struct {
	Value Object
}

// Inspect implementation of the Object interface
func (rv *ReturnValue) Inspect() string {
	return rv.Value.Inspect()
}

// Type implementation of the Object interface
func (rv *ReturnValue) Type() Type {
	return ReturnValueObj
}

// String is the wrapper for strings in Monkey.
type String struct {
	Value string
}

// Inspect implementation of the Object interface
func (s *String) Inspect() string {
	return s.Value
}

// Type implementation of the Object interface
func (s *String) Type() Type {
	return StringObj
}
