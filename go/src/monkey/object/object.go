package object

import (
	"fmt"
	"hash/fnv"
	"monkey/ast"
	"monkey/code"
	"strings"
)

// Type is the enum for different types of object in our object system.
type Type string

// The different types of object supported.
const (
	ArrayObj            = "ARRAY"
	BooleanObj          = "BOOLEAN"
	BuiltInObj          = "BUILTIN"
	ClosureObj          = "CLOSURE"
	CompiledFunctionObj = "COMPILED_FUNCTION"
	ErrorObj            = "ERROR"
	FunctionObj         = "FUNCTION"
	HashObj             = "HASH"
	IntegerObj          = "INTEGER"
	MacroObj            = "MACRO"
	NullObj             = "NULL"
	QuoteObj            = "QUOTE"
	ReturnValueObj      = "RETURN_VALUE"
	StringObj           = "STRING"
)

// Hashable is the interface which objects implement if they can go in a HashLiteral.
type Hashable interface {
	HashKey() HashKey
}

// Object is the common interface for our object system.
type Object interface {
	Type() Type
	Inspect() string
}

// Array is the array type in Monkey.
type Array struct {
	Elements []Object
}

// Inspect implementation of the Object interface
func (a *Array) Inspect() string {
	var out strings.Builder

	elements := []string{}

	for _, el := range a.Elements {
		elements = append(elements, el.Inspect())
	}

	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")

	return out.String()
}

// Type implementation of the Object interface
func (a *Array) Type() Type {
	return ArrayObj
}

// Boolean is the boolean type in Monkey.
type Boolean struct {
	Value bool
}

// HashKey implementation of the Hashable interface
func (b *Boolean) HashKey() HashKey {
	var value uint64

	if b.Value {
		value = 1
	} else {
		value = 0
	}

	return HashKey{Type: b.Type(), Value: value}
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

// Closure wraps a CompiledFunction and any free variables that are referenced. See
// https://en.wikipedia.org/wiki/Free_variables_and_bound_variables
type Closure struct {
	Fn   *CompiledFunction
	Free []Object
}

// Inspect implementation of the Object interface
func (c *Closure) Inspect() string {
	return fmt.Sprintf("Closure[%p]", c)
}

// Type implementation of the Object interface
func (c *Closure) Type() Type {
	return BuiltInObj
}

// CompiledFunction is a a function object that holds bytecode instead of AST nodes
type CompiledFunction struct {
	Instructions  code.Instructions
	NumLocals     int
	NumParameters int
}

// Inspect implementation of the Object interface
func (cf *CompiledFunction) Inspect() string {
	return fmt.Sprintf("CompiledFunction[%p]", cf)
}

// Type implementation of the Object interface
func (cf *CompiledFunction) Type() Type {
	return CompiledFunctionObj
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

// Hash is the hash type in Monkey.
type Hash struct {
	Pairs map[HashKey]HashPair
}

// Inspect implementation of the Object interface
func (h *Hash) Inspect() string {
	var out strings.Builder

	pairs := []string{}

	for _, pair := range h.Pairs {
		pairs = append(pairs, fmt.Sprintf("%s: %s", pair.Key.Inspect(), pair.Value.Inspect()))
	}

	out.WriteString("{")
	out.WriteString(strings.Join(pairs, ", "))
	out.WriteString("}")

	return out.String()
}

// Type implementation of the Object interface
func (h *Hash) Type() Type {
	return HashObj
}

// HashKey allows for hashing of data types in Monkey.
type HashKey struct {
	Type  Type
	Value uint64
}

// HashPair keeps track of the key and value in a hash
type HashPair struct {
	Key   Object
	Value Object
}

// Integer is the integer type in Monkey.
type Integer struct {
	Value int64
}

// HashKey implementation of the Hashable interface
func (i *Integer) HashKey() HashKey {
	return HashKey{Type: i.Type(), Value: uint64(i.Value)}
}

// Inspect implementation of the Object interface
func (i *Integer) Inspect() string {
	return fmt.Sprintf("%d", i.Value)
}

// Type implementation of the Object interface
func (i *Integer) Type() Type {
	return IntegerObj
}

// Macro is the macro type in Monkey.
type Macro struct {
	Parameters []*ast.Identifier
	Body       *ast.BlockStatement
	Env        *Environment
}

// Inspect implementation of the Object interface
func (m *Macro) Inspect() string {
	var out strings.Builder

	params := []string{}
	for _, p := range m.Parameters {
		params = append(params, p.String())
	}

	out.WriteString("macro")
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") {\n")
	out.WriteString(m.Body.String())
	out.WriteString("\n}")

	return out.String()
}

// Type implementation of the Object interface
func (m *Macro) Type() Type {
	return MacroObj
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

// Quote is the beginnings of a macro system for Monkey, a la Scheme.
type Quote struct {
	Node ast.Node
}

// Inspect implementation of the Object interface
func (q *Quote) Inspect() string {
	return "QUOTE(" + q.Node.String() + ")"
}

// Type implementation of the Object interface
func (q *Quote) Type() Type {
	return QuoteObj
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

// HashKey implementation of the Hashable interface
func (s *String) HashKey() HashKey {
	h := fnv.New64a()
	h.Write([]byte(s.Value))

	return HashKey{Type: s.Type(), Value: h.Sum64()}
}

// Inspect implementation of the Object interface
func (s *String) Inspect() string {
	return s.Value
}

// Type implementation of the Object interface
func (s *String) Type() Type {
	return StringObj
}
