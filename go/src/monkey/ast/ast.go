package ast

import (
	"monkey/token"
	"strings"
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
	var out strings.Builder

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

// ArrayLiteral allows for array support.
type ArrayLiteral struct {
	Token    token.Token
	Elements []Expression
}

// String Node implementation
func (al *ArrayLiteral) String() string {
	var out strings.Builder

	elements := []string{}

	for _, el := range al.Elements {
		elements = append(elements, el.String())
	}

	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")

	return out.String()
}

// TokenLiteral returns the result of the root Node
func (al *ArrayLiteral) TokenLiteral() string {
	return al.Token.Literal
}

func (al *ArrayLiteral) expressionNode() {

}

// BlockStatement allows for blocks of code (as part of an `if` expression, for example)
type BlockStatement struct {
	Token      token.Token // the `{` token
	Statements []Statement
}

// String Node implementation
func (bs *BlockStatement) String() string {
	var out strings.Builder

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

// CallExpression adds support for `<expression>([<expression> [, <expression>] ])`
type CallExpression struct {
	Token     token.Token // The '(' token
	Function  Expression  // Identifier or FunctionLiteral
	Arguments []Expression
}

// String Node implementation
func (ce *CallExpression) String() string {
	var out strings.Builder

	args := []string{}
	for _, a := range ce.Arguments {
		args = append(args, a.String())
	}

	out.WriteString(ce.Function.String())
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")

	return out.String()
}

// TokenLiteral returns the result of the root Node
func (ce *CallExpression) TokenLiteral() string { return ce.Token.Literal }

func (ce *CallExpression) expressionNode() {}

// FunctionLiteral adds support for `fn <parameters> <block statement>`
type FunctionLiteral struct {
	Token      token.Token // The `fn` token
	Parameters []*Identifier
	Body       *BlockStatement
}

// String Node implementation
func (fl *FunctionLiteral) String() string {
	var out strings.Builder

	params := make([]string, len(fl.Parameters))

	for i, p := range fl.Parameters {
		params[i] = p.String()
	}

	out.WriteString(fl.TokenLiteral())
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") { ")
	out.WriteString(fl.Body.String())
	out.WriteString(" }")

	return out.String()
}

// TokenLiteral returns the result of the root Node
func (fl *FunctionLiteral) TokenLiteral() string {
	return fl.Token.Literal
}

func (fl *FunctionLiteral) expressionNode() {}

// IfExpression adds support for `if (<condition>) <consequence? [else <alternative>]`
type IfExpression struct {
	Token       token.Token // The `if` keyword token
	Condition   Expression
	Consequence *BlockStatement
	Alternative *BlockStatement
}

// String Node implementation
func (ie *IfExpression) String() string {
	var out strings.Builder

	out.WriteString("if (")
	out.WriteString(ie.Condition.String())
	out.WriteString(") { ")
	out.WriteString(ie.Consequence.String())

	if ie.Alternative != nil {
		out.WriteString(" } else { ")
		out.WriteString(ie.Alternative.String())
	}

	out.WriteString(" }")

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
	var out strings.Builder

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

// HashLiteral is the node for a literal hash `{key: value [, ... ]}`
type HashLiteral struct {
	Token token.Token
	Pairs map[Expression]Expression
}

// String Node implementation
func (hl *HashLiteral) String() string {
	var out strings.Builder

	pairs := []string{}

	for key, value := range hl.Pairs {
		pairs = append(pairs, key.String()+": "+value.String())
	}

	out.WriteString("{")
	out.WriteString(strings.Join(pairs, ", "))
	out.WriteString("}")

	return out.String()
}

// TokenLiteral Node implementation
func (hl *HashLiteral) TokenLiteral() string {
	return hl.Token.Literal
}

func (hl *HashLiteral) expressionNode() {

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

// IndexExpression represents an array index expression `[exp]`
type IndexExpression struct {
	Token token.Token // The [ token
	Left  Expression
	Index Expression
}

// String Node implementation
func (ie *IndexExpression) String() string {
	var out strings.Builder

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString("[")
	out.WriteString(ie.Index.String())
	out.WriteString("])")

	return out.String()
}

// TokenLiteral Node implementation
func (ie *IndexExpression) TokenLiteral() string {
	return ie.Token.Literal
}

func (ie *IndexExpression) expressionNode() {

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
	var out strings.Builder

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
	var out strings.Builder

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
	var out strings.Builder

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

// StringLiteral is a type of Node that is a string
type StringLiteral struct {
	Token token.Token
	Value string
}

func (s *StringLiteral) expressionNode() {}

// String Node implementation
func (s *StringLiteral) String() string {
	return s.Token.Literal
}

// TokenLiteral Node implementation
func (s *StringLiteral) TokenLiteral() string {
	return s.Token.Literal
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
