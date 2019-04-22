package vm

import (
	"monkey/code"
	"monkey/object"
)

// Frame represents call-relevant information – the instructions and the instruction pointer.
// It is short for "call frame" or "stack frame", and is sometimes called an "activation record" in
// the literature
type Frame struct {
	cl          *object.Closure
	ip          int
	basePointer int
}

// NewFrame returns a Frame which encapsulates the specified compiled function
func NewFrame(cl *object.Closure, basePointer int) *Frame {
	return &Frame{
		cl:          cl,
		ip:          -1,
		basePointer: basePointer,
	}
}

// Instructions returns the instructions for this Frame
func (f *Frame) Instructions() code.Instructions {
	return f.cl.Fn.Instructions
}
