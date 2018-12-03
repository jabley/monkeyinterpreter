package ast

import (
	"monkey/token"
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
