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
	"push":  object.GetBuiltInByName("push"),
}
