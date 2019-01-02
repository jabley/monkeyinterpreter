package ast

import (
	"fmt"
	"monkey/token"
	"reflect"
	"testing"
)

func TestLetString(t *testing.T) {
	program := &Program{
		Statements: []Statement{
			&LetStatement{
				Token: token.Token{Type: token.Let, Literal: "let"},
				Name: &Identifier{
					Token: token.Token{Type: token.Ident, Literal: "myVar"},
					Value: "myVar",
				},
				Value: &Identifier{
					Token: token.Token{Type: token.Ident, Literal: "anotherVar"},
					Value: "anotherVar",
				},
			},
		},
	}

	if program.String() != "let myVar = anotherVar;" {
		t.Errorf("program.String() wrong. got=%q", program.String())
	}
}

func TestReturnString(t *testing.T) {
	program := &Program{
		Statements: []Statement{
			&ReturnStatement{
				Token: token.Token{Type: token.Return, Literal: "return"},
				ReturnValue: &Identifier{
					Token: token.Token{Type: token.Ident, Literal: "anotherVar"},
					Value: "anotherVar",
				},
			},
		},
	}

	if program.String() != "return anotherVar;" {
		t.Errorf("program.String() wrong. got=%q", program.String())
	}
}

func TestInfixString(t *testing.T) {
	program := &Program{
		Statements: []Statement{
			&ExpressionStatement{
				Expression: &InfixExpression{
					Token: token.Token{Type: token.Plus, Literal: "+"},
					Left: &IntegerLiteral{
						Token: token.Token{Type: token.Ident, Literal: "5"},
						Value: 5,
					},
					Operator: "+",
					Right: &IntegerLiteral{
						Token: token.Token{Type: token.Ident, Literal: "10"},
						Value: 10,
					},
				},
			},
		},
	}

	if program.String() != "(5 + 10)" {
		t.Errorf("program.String() wrong. got=%q", program.String())
	}
}

func TestPrefixString(t *testing.T) {
	program := &Program{
		Statements: []Statement{
			&ExpressionStatement{
				Expression: &PrefixExpression{
					Token:    token.Token{Type: token.Plus, Literal: "+"},
					Operator: "+",
					Right: &IntegerLiteral{
						Token: token.Token{Type: token.Ident, Literal: "10"},
						Value: 10,
					},
				},
			},
		},
	}

	if program.String() != "(+10)" {
		t.Errorf("program.String() wrong. got=%q", program.String())
	}
}

func TestIfString(t *testing.T) {
	program := &Program{
		Statements: []Statement{
			&ExpressionStatement{
				Expression: &IfExpression{
					Token: token.Token{Type: token.Ident, Literal: "if"},
					Condition: &Identifier{
						Token: token.Token{Type: token.Ident, Literal: "foo"},
						Value: "foo",
					},
					Consequence: &BlockStatement{
						Statements: []Statement{
							&ExpressionStatement{
								Expression: &Identifier{
									Token: token.Token{Type: token.Ident, Literal: "bar"},
									Value: "bar",
								},
							},
						},
					},
				},
			},
		},
	}

	if program.String() != "if (foo) { bar }" {
		t.Errorf("program.String() wrong. got=%q", program.String())
	}
}

func TestIfElseString(t *testing.T) {
	program := &Program{
		Statements: []Statement{
			&ExpressionStatement{
				Expression: &IfExpression{
					Token: token.Token{Type: token.Ident, Literal: "if"},
					Condition: &Identifier{
						Token: token.Token{Type: token.Ident, Literal: "foo"},
						Value: "foo",
					},
					Consequence: &BlockStatement{
						Statements: []Statement{
							&ExpressionStatement{
								Expression: &Identifier{
									Token: token.Token{Type: token.Ident, Literal: "bar"},
									Value: "bar",
								},
							},
						},
					},
					Alternative: &BlockStatement{
						Statements: []Statement{
							&ExpressionStatement{
								Expression: &Identifier{
									Token: token.Token{Type: token.Ident, Literal: "baz"},
									Value: "baz",
								},
							},
						},
					},
				},
			},
		},
	}

	if program.String() != "if (foo) { bar } else { baz }" {
		t.Errorf("program.String() wrong. got=%q", program.String())
	}
}

func TestFunctionString(t *testing.T) {
	program := &Program{
		Statements: []Statement{
			&ExpressionStatement{
				Expression: &FunctionLiteral{
					Token: token.Token{Type: token.Function, Literal: "fn"},
					Parameters: []*Identifier{
						&Identifier{
							Token: token.Token{Type: token.Ident, Literal: "x"},
							Value: "x",
						},
					},
					Body: &BlockStatement{
						Statements: []Statement{
							&ExpressionStatement{
								Expression: &InfixExpression{
									Token: token.Token{Type: token.Plus, Literal: "+"},
									Left: &Identifier{
										Token: token.Token{Type: token.Ident, Literal: "x"},
										Value: "x",
									},
									Operator: "+",
									Right: &IntegerLiteral{
										Token: token.Token{Type: token.Ident, Literal: "2"},
										Value: 2,
									},
								},
							},
						},
					},
				},
			},
		},
	}

	if program.String() != "fn(x) { (x + 2) }" {
		t.Errorf("program.String() wrong. got=%q", program.String())
	}
}

func TestCallString(t *testing.T) {
	program := &Program{
		Statements: []Statement{
			&ExpressionStatement{
				Expression: &CallExpression{
					Token: token.Token{Type: token.LParen, Literal: "("},
					Arguments: []Expression{
						&Identifier{
							Token: token.Token{Type: token.Ident, Literal: "x"},
							Value: "x",
						},
						&Identifier{
							Token: token.Token{Type: token.Ident, Literal: "y"},
							Value: "y",
						},
					},
					Function: &Identifier{
						Token: token.Token{Type: token.Ident, Literal: "foobar"},
						Value: "foobar",
					},
				},
			},
		},
	}

	if program.String() != "foobar(x, y)" {
		t.Errorf("program.String() wrong. got=%q", program.String())
	}
}

func TestModify(t *testing.T) {
	one := func() Expression { return &IntegerLiteral{Value: 1} }
	two := func() Expression { return &IntegerLiteral{Value: 2} }

	turnOneIntoTwo := func(node Node) Node {
		integer, ok := node.(*IntegerLiteral)
		if !ok {
			return node
		}

		if integer.Value != 1 {
			return node
		}

		integer.Value = 2
		return integer
	}

	tests := []struct {
		input    Node
		expected Node
	}{
		{
			one(),
			two(),
		},
		{
			&Program{
				Statements: []Statement{
					&ExpressionStatement{Expression: one()},
				},
			},
			&Program{
				Statements: []Statement{
					&ExpressionStatement{Expression: two()},
				},
			},
		},
		{
			&InfixExpression{Left: one(), Operator: "+", Right: two()},
			&InfixExpression{Left: two(), Operator: "+", Right: two()},
		},
		{
			&InfixExpression{Left: two(), Operator: "+", Right: one()},
			&InfixExpression{Left: two(), Operator: "+", Right: two()},
		},
	}

	for _, tt := range tests {
		t.Run(fmt.Sprintf("%v", tt.input), func(t *testing.T) {
			modified := Modify(tt.input, turnOneIntoTwo)

			equal := reflect.DeepEqual(modified, tt.expected)
			if !equal {
				t.Errorf("not equal. got=%#v, want=%#v",
					modified, tt.expected)
			}
		})
	}
}
