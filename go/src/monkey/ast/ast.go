package ast

import (
	"bytes"
	"monkey/token"
)

// Node is the basic interface for a item in the Abstract Syntax Tree
type Node interface {
	String() string
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

// String Node implementation
func (p *Program) String() string {
	var out bytes.Buffer

	for _, s := range p.Statements {
		out.WriteString(s.String())
	}

	return out.String()
}

// TokenLiteral returns the result of the root Node
func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

// BlockStatement allows for blocks of code (as part of an `if` expression, for example)
type BlockStatement struct {
	Token      token.Token // the `{` token
	Statements []Statement
}

// String Node implementation
func (bs *BlockStatement) String() string {
	var out bytes.Buffer

	for _, s := range bs.Statements {
		out.WriteString(s.String())
	}

	return out.String()
}

// TokenLiteral returns the result of the root Node
func (bs *BlockStatement) TokenLiteral() string {
	return bs.Token.Literal
}

func (bs *BlockStatement) expressionNode() {

}

// Boolean adds support for literal booleans
type Boolean struct {
	Token token.Token
	Value bool
}

// String Node implementation
func (b *Boolean) String() string {
	return b.TokenLiteral()
}

// TokenLiteral returns the result of the root Node
func (b *Boolean) TokenLiteral() string {
	return b.Token.Literal
}

func (b *Boolean) expressionNode() {

}

// IfExpression adds support for `if (<condition>) <consequence? [else <alternative>]`
type IfExpression struct {
	Token       token.Token // The `if` keyword token
	Condition   Expression
	Consequence *BlockStatement
	Alternative *BlockStatement
}

// String Node implementation
func (ie *IfExpression) String() string {
	var out bytes.Buffer

	out.WriteString("if")
	out.WriteString(ie.Condition.String())
	out.WriteString(" ")
	out.WriteString(ie.Consequence.String())

	if ie.Alternative != nil {
		out.WriteString("else ")
		out.WriteString(ie.Alternative.String())
	}

	return out.String()
}

// TokenLiteral returns the result of the root Node
func (ie *IfExpression) TokenLiteral() string {
	return ie.Token.Literal
}

func (ie *IfExpression) expressionNode() {

}

// IntegerLiteral is a type of Node that is an int
type IntegerLiteral struct {
	Token token.Token
	Value int64
}

// String Node implementation
func (il *IntegerLiteral) String() string {
	return il.Token.Literal
}

// TokenLiteral returns the result of the root Node
func (il *IntegerLiteral) TokenLiteral() string {
	return il.Token.Literal
}

func (il *IntegerLiteral) expressionNode() {

}

// LetStatement is a `let x = foo;` statement
type LetStatement struct {
	Token token.Token // the token.Let token
	Name  *Identifier
	Value Expression
}

func (ls *LetStatement) statementNode() {

}

// String Node implementation
func (ls *LetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.String())
	out.WriteString(" = ")

	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}

	out.WriteString(";")

	return out.String()
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

// String Node implementation
func (i *Identifier) String() string {
	return i.Value
}

// TokenLiteral Node implementation
func (i *Identifier) TokenLiteral() string {
	return i.Token.Literal
}

// InfixExpression is expressions involving binary operators like `X+Y`, `X*Y` etc
type InfixExpression struct {
	Token    token.Token // The operator token, e.g. +
	Left     Expression
	Operator string
	Right    Expression
}

// String Node implementation
func (ie *InfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")

	return out.String()
}

// TokenLiteral Node implementation
func (ie *InfixExpression) TokenLiteral() string {
	return ie.Token.Literal
}
func (ie *InfixExpression) expressionNode() {}

// PrefixExpression is a Node for `!X` or `-X`
type PrefixExpression struct {
	Right    Expression
	Operator string
	Token    token.Token
}

// String Node implementation
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")

	return out.String()
}

// TokenLiteral Node implementation
func (pe *PrefixExpression) TokenLiteral() string {
	return pe.Token.Literal
}

func (pe *PrefixExpression) expressionNode() {

}

// ReturnStatement is a `return foo;` statement
type ReturnStatement struct {
	Token       token.Token // the 'return' token
	ReturnValue Expression
}

func (rs *ReturnStatement) statementNode() {}

// String Node implementation
func (rs *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString(rs.TokenLiteral() + " ")

	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.String())
	}

	out.WriteString(";")

	return out.String()
}

// TokenLiteral Node implementation
func (rs *ReturnStatement) TokenLiteral() string {
	return rs.Token.Literal
}

// ExpressionStatement is a general-purpose expression
type ExpressionStatement struct {
	Token      token.Token // the first token of the expression
	Expression Expression
}

// String Node implementation
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

// TokenLiteral Node implementation
func (es *ExpressionStatement) TokenLiteral() string {
	return es.Token.Literal
}

func (es *ExpressionStatement) statementNode() {

}
