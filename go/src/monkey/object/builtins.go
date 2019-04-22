package object

import "fmt"

// BuiltIns is the set of builtin functions usable by both the VM and evaluator
var BuiltIns = []struct {
	Name    string
	BuiltIn *BuiltIn
}{
	{
		"len",
		&BuiltIn{
			Fn: func(args ...Object) Object {
				if len(args) != 1 {
					return newError("wrong number of arguments. got=%d, want=1", len(args))
				}

				switch arg := args[0].(type) {
				case *Array:
					return &Integer{Value: int64(len(arg.Elements))}
				case *String:
					return &Integer{Value: int64(len(arg.Value))}
				default:
					return newError("argument to `len` not supported, got %s", args[0].Type())
				}
			},
		},
	},
	{
		"puts",
		&BuiltIn{
			Fn: func(args ...Object) Object {
				for _, arg := range args {
					fmt.Println(arg.Inspect())
				}

				return nil
			},
		},
	},
}

// GetBuiltInByName returns the named BuiltIn, or nil if there is no such BuiltIn
func GetBuiltInByName(name string) *BuiltIn {
	for _, def := range BuiltIns {
		if def.Name == name {
			return def.BuiltIn
		}
	}
	return nil
}

func newError(format string, a ...interface{}) *Error {
	return &Error{Message: fmt.Sprintf(format, a...)}
}
