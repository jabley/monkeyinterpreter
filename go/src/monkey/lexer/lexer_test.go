package lexer

import (
	"testing"

	"monkey/token"
)

type expectedToken struct {
	expectedType    token.Type
	expectedLiteral string
}

func TestNextToken(t *testing.T) {
	testLexing(t, `=+(){},;`, []expectedToken{
		{token.Assign, "="},
		{token.Plus, "+"},
		{token.LParen, "("},
		{token.RParen, ")"},
		{token.LBrace, "{"},
		{token.RBrace, "}"},
		{token.Comma, ","},
		{token.SemiColon, ";"},
		{token.EOF, ""},
	})
}

func TestNextTokenOnMonkey(t *testing.T) {
	input := `let five = 5;

let ten = 10;

let add = fn( x, y) {
	x + y;
};

let result = add( five, ten);

!-/* 5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;

10 != 9;
"foobar"
"foo bar"
[1, 2];
{"foo": "bar"}
macro(x, y) { x + y; };
`

	testLexing(t, input, []expectedToken{
		{token.Let, "let"},
		{token.Ident, "five"},
		{token.Assign, "="},
		{token.Int, "5"},
		{token.SemiColon, ";"},
		{token.Let, "let"},
		{token.Ident, "ten"},
		{token.Assign, "="},
		{token.Int, "10"},
		{token.SemiColon, ";"},
		{token.Let, "let"},
		{token.Ident, "add"},
		{token.Assign, "="},
		{token.Function, "fn"},
		{token.LParen, "("},
		{token.Ident, "x"},
		{token.Comma, ","},
		{token.Ident, "y"},
		{token.RParen, ")"},
		{token.LBrace, "{"},
		{token.Ident, "x"},
		{token.Plus, "+"},
		{token.Ident, "y"},
		{token.SemiColon, ";"},
		{token.RBrace, "}"},
		{token.SemiColon, ";"},
		{token.Let, "let"},
		{token.Ident, "result"},
		{token.Assign, "="},
		{token.Ident, "add"},
		{token.LParen, "("},
		{token.Ident, "five"},
		{token.Comma, ","},
		{token.Ident, "ten"},
		{token.RParen, ")"},
		{token.SemiColon, ";"},
		{token.Bang, "!"},
		{token.Minus, "-"},
		{token.Slash, "/"},
		{token.Asterisk, "*"},
		{token.Int, "5"},
		{token.SemiColon, ";"},
		{token.Int, "5"},
		{token.Lt, "<"},
		{token.Int, "10"},
		{token.Gt, ">"},
		{token.Int, "5"},
		{token.SemiColon, ";"},
		{token.If, "if"},
		{token.LParen, "("},
		{token.Int, "5"},
		{token.Lt, "<"},
		{token.Int, "10"},
		{token.RParen, ")"},
		{token.LBrace, "{"},
		{token.Return, "return"},
		{token.True, "true"},
		{token.SemiColon, ";"},
		{token.RBrace, "}"},
		{token.Else, "else"},
		{token.LBrace, "{"},
		{token.Return, "return"},
		{token.False, "false"},
		{token.SemiColon, ";"},
		{token.RBrace, "}"},
		{token.Int, "10"},
		{token.Eq, "=="},
		{token.Int, "10"},
		{token.SemiColon, ";"},
		{token.Int, "10"},
		{token.NotEq, "!="},
		{token.Int, "9"},
		{token.SemiColon, ";"},
		{token.String, "foobar"},
		{token.String, "foo bar"},
		{token.LBracket, "["},
		{token.Int, "1"},
		{token.Comma, ","},
		{token.Int, "2"},
		{token.RBracket, "]"},
		{token.SemiColon, ";"},
		{token.LBrace, "{"},
		{token.String, "foo"},
		{token.Colon, ":"},
		{token.String, "bar"},
		{token.RBrace, "}"},
		{token.Macro, "macro"},
		{token.LParen, "("},
		{token.Ident, "x"},
		{token.Comma, ","},
		{token.Ident, "y"},
		{token.RParen, ")"},
		{token.LBrace, "{"},
		{token.Ident, "x"},
		{token.Plus, "+"},
		{token.Ident, "y"},
		{token.SemiColon, ";"},
		{token.RBrace, "}"},
		{token.SemiColon, ";"},
		{token.EOF, ""},
	})
}

func testLexing(t *testing.T, input string, expectedTokens []expectedToken) {
	t.Helper()
	l := New(input)

	for i, tt := range expectedTokens {
		tok := l.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf(" tests[%d] - tokentype wrong. expected =% q, got =% q", i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf(" tests[%d] - literal wrong. expected =% q, got =% q", i, tt.expectedLiteral, tok.Literal)
		}
	}
}
