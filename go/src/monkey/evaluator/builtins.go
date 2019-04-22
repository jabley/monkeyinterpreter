package evaluator

import (
	"monkey/object"
)

var builtins = map[string]*object.BuiltIn{
	"len":   object.GetBuiltInByName("len"),
	"puts":  object.GetBuiltInByName("puts"),
	"first": object.GetBuiltInByName("first"),
	"last":  object.GetBuiltInByName("last"),
	"rest":  object.GetBuiltInByName("rest"),
	"push": &object.BuiltIn{
		Fn: func(args ...object.Object) object.Object {
			if len(args) != 2 {
				return newError("wrong number of arguments. got=%d, want=2",
					len(args))
			}
			if args[0].Type() != object.ArrayObj {
				return newError("argument to `push` must be ARRAY, got %s",
					args[0].Type())
			}

			arr := args[0].(*object.Array)
			length := len(arr.Elements)

			newElements := make([]object.Object, length+1, length+1)
			copy(newElements, arr.Elements)
			newElements[length] = args[1]

			return &object.Array{Elements: newElements}
		},
	},
}
