package ast

import "monkey/token"

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

// LetStatement is a `let x = foo;` statement
type LetStatement struct {
	Token token.Token // the token.Let token
	Name  *Identifier
	Value Expression
}

func (ls *LetStatement) statementNode() {

}

// TokenLiteral Node implementation
func (ls *LetStatement) TokenLiteral() string {
	return ls.Token.Literal
}

// Identifier is an Expression. Identifier in LetStatement isn't an Expression
// because it doesn't produce a value. But Identifiers in other parts might be;
// eg a function name.
type Identifier struct {
	Token token.Token // the token.Identifier token
	Value string
}

func (i *Identifier) expressionNode() {

}

// TokenLiteral Node implementation
func (i *Identifier) TokenLiteral() string {
	return i.Token.Literal
}

// ReturnStatement is a `return foo;` statement
type ReturnStatement struct {
	Token       token.Token // the 'return' token
	ReturnValue Expression
}

func (rs *ReturnStatement) statementNode() {}

// TokenLiteral Node implementation
func (rs *ReturnStatement) TokenLiteral() string {
	return rs.Token.Literal
}
