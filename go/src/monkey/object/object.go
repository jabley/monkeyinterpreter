package object

import "fmt"

// Type is the enum for different types of object in our object system.
type Type string

// The different types of object supported.
const (
	BooleanObj     = "BOOLEAN"
	ErrorObj       = "ERROR"
	IntegerObj     = "INTEGER"
	NullObj        = "NULL"
	ReturnValueObj = "RETURN_VALUE"
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
