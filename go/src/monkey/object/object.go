package object

import "fmt"

// Type is the enum for different types of object in our object system.
type Type string

// The different types of object supported.
const (
	IntegerObj = "INTEGER"
)

// Object is the common interface for our object system.
type Object interface {
	Type() Type
	Inspect() string
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
