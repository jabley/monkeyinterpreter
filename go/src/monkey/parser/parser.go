package parser

import (
	"monkey/ast"
	"monkey/lexer"
	"monkey/token"
)

// Parser is the parser for Monkey programs
type Parser struct {
	l         *lexer.Lexer
	curToken  token.Token
	peekToken token.Token
}

// New creates a new Parser which wraps the provided Lexer
func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l}

	// Read 2 tokens so that curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

// ParseProgram return the result of complete parsing
func (p *Parser) ParseProgram() *ast.Program {
	return nil
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}
