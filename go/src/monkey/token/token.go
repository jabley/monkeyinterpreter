package token

// Type is alias for supported tokens
type Type string

// Type enums for our language
const (
	Illegal = "ILLEGAL"
	EOF     = "EOF"

	// identifier + literals
	Ident  = "IDENT" // add, foobar, x, y, ...
	Int    = "INT"   // 1343456
	String = "STRING"

	// Operators
	Assign   = "="
	Plus     = "+"
	Minus    = "-"
	Bang     = "!"
	Asterisk = "*"
	Slash    = "/"

	Lt    = "<"
	Gt    = ">"
	Eq    = "=="
	NotEq = "!="

	// Delimiters
	Comma     = ","
	SemiColon = ";"

	LParen = "("
	RParen = ")"
	LBrace = "{"
	RBrace = "}"

	// Keywords
	Function = "FUNCTION"
	Let      = "LET"
	If       = "IF"
	Else     = "ELSE"
	True     = "TRUE"
	False    = "FALSE"
	Return   = "RETURN"
)

// Token is the atom returned by the Lexer.
type Token struct {
	Type    Type
	Literal string
}

var keywords = map[string]Type{
	"fn":     Function,
	"let":    Let,
	"if":     If,
	"else":   Else,
	"true":   True,
	"false":  False,
	"return": Return,
}

// LookupIdent checks whether the indentifier is a keyword.
func LookupIdent(ident string) Type {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return Ident
}
