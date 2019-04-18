package vm

import (
	"monkey/code"
	"monkey/object"
)

// Frame represents call-relevant information – the instructions and the instruction pointer.
// It is short for "call frame" or "stack frame", and is sometimes called an "activation record" in
// the literature
type Frame struct {
	fn *object.CompiledFunction
	ip int
}

// NewFrame returns a Frame which encapsulates the specified compiled function
func NewFrame(fn *object.CompiledFunction) *Frame {
	return &Frame{
		fn: fn,
		ip: -1,
	}
}

// Instructions returns the instructions for this Frame
func (f *Frame) Instructions() code.Instructions {
	return f.fn.Instructions
}
