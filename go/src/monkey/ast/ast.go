package ast

// Node is the basic interface for a item in the Abstract Syntax Tree
type Node interface {
	TokenLiteral() string
}

// Statement is a type of Node that does not return a value
type Statement interface {
	Node
	statementNode()
}

// Expression is a type of Node that returns a value
type Expression interface {
	Node
	expressionNode()
}

// Program is a Monkey programme. Every valid Program is a series of Statements.
type Program struct {
	Statements []Statement
}

// TokenLiteral returns the result of the root Node
func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}
