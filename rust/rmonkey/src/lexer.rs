use std::iter::Peekable;
use std::str::Chars;

use crate::token;
use crate::token::Token;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn peek_char(&self) -> Option<char> {
        self.input.clone().next()
    }

    fn peek_if<F>(&mut self, mut predicate: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        match self.peek_char() {
            Some(ch) => predicate(ch),
            None => false,
        }
    }

    fn read_identifier(&mut self, first: char) -> String {
        let mut ident = String::new();
        ident.push(first);

        while self.peek_if(is_id_continue) {
            ident.push(self.read_char().unwrap());
        }

        ident
    }

    fn read_number(&mut self, first: char) -> i64 {
        let mut number = String::new();
        number.push(first);

        while self.peek_if(|c| c.is_numeric()) {
            number.push(self.read_char().unwrap());
        }

        number.parse().unwrap()
    }

    fn read_string(&mut self) -> String {
        let mut res = String::new();

        while self.peek_if(|c| c != '"') {
            res.push(self.read_char().unwrap());
        }

        // Consume the last '""
        self.read_char();

        res
    }

    fn skip_whitespace(&mut self) {
        while self.peek_if(|c| c.is_whitespace()) {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.read_char() {
            Some('=') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            Some('+') => Token::Plus,
            Some('!') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::Ne
                } else {
                    Token::Bang
                }
            }
            Some('-') => Token::Minus,
            Some('*') => Token::Asterisk,
            Some('/') => Token::Slash,
            Some('<') => Token::Lt,
            Some('>') => Token::Gt,
            Some('(') => Token::OpenParen,
            Some(')') => Token::CloseParen,
            Some('{') => Token::OpenBrace,
            Some('}') => Token::CloseBrace,
            Some('[') => Token::OpenBracket,
            Some(']') => Token::CloseBracket,
            Some(',') => Token::Comma,
            Some(';') => Token::SemiColon,
            Some(':') => Token::Colon,
            Some('"') => Token::String(self.read_string()),
            Some(c) => {
                if is_id_start(c) {
                    token::lookup_ident(&self.read_identifier(c))
                } else if c.is_numeric() {
                    Token::Int(self.read_number(c))
                } else {
                    Token::Illegal(c)
                }
            }
            None => Token::Eof,
        }
    }
}

fn is_id_start(c: char) -> bool {
    c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_'
}

fn is_id_continue(c: char) -> bool {
    is_id_start(c) || c.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    fn test_lexing(input: &str, expected_tokens: Vec<Token>) {
        let mut l = Lexer::new(input);

        for expected in expected_tokens {
            let actual = l.next_token();

            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_next_token() {
        test_lexing(
            "=+(){},;",
            vec![
                Token::Assign,
                Token::Plus,
                Token::OpenParen,
                Token::CloseParen,
                Token::OpenBrace,
                Token::CloseBrace,
                Token::Comma,
                Token::SemiColon,
                Token::Eof,
            ],
        );
    }

    #[test]
    fn test_next_token_on_monkey() {
        test_lexing(
            r#"let five = 5;

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
[1, 2]
{"foo": "bar"}
"#,
            vec![
                Token::Let,
                Token::Ident("five".to_owned()),
                Token::Assign,
                Token::Int(5),
                Token::SemiColon,
                Token::Let,
                Token::Ident("ten".to_owned()),
                Token::Assign,
                Token::Int(10),
                Token::SemiColon,
                Token::Let,
                Token::Ident("add".to_owned()),
                Token::Assign,
                Token::Function,
                Token::OpenParen,
                Token::Ident("x".to_owned()),
                Token::Comma,
                Token::Ident("y".to_owned()),
                Token::CloseParen,
                Token::OpenBrace,
                Token::Ident("x".to_owned()),
                Token::Plus,
                Token::Ident("y".to_owned()),
                Token::SemiColon,
                Token::CloseBrace,
                Token::SemiColon,
                Token::Let,
                Token::Ident("result".to_owned()),
                Token::Assign,
                Token::Ident("add".to_owned()),
                Token::OpenParen,
                Token::Ident("five".to_owned()),
                Token::Comma,
                Token::Ident("ten".to_owned()),
                Token::CloseParen,
                Token::SemiColon,
                Token::Bang,
                Token::Minus,
                Token::Slash,
                Token::Asterisk,
                Token::Int(5),
                Token::SemiColon,
                Token::Int(5),
                Token::Lt,
                Token::Int(10),
                Token::Gt,
                Token::Int(5),
                Token::SemiColon,
                Token::If,
                Token::OpenParen,
                Token::Int(5),
                Token::Lt,
                Token::Int(10),
                Token::CloseParen,
                Token::OpenBrace,
                Token::Return,
                Token::True,
                Token::SemiColon,
                Token::CloseBrace,
                Token::Else,
                Token::OpenBrace,
                Token::Return,
                Token::False,
                Token::SemiColon,
                Token::CloseBrace,
                Token::Int(10),
                Token::Eq,
                Token::Int(10),
                Token::SemiColon,
                Token::Int(10),
                Token::Ne,
                Token::Int(9),
                Token::SemiColon,
                Token::String("foobar".to_owned()),
                Token::String("foo bar".to_owned()),
                Token::OpenBracket,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::CloseBracket,
                Token::OpenBrace,
                Token::String("foo".to_owned()),
                Token::Colon,
                Token::String("bar".to_owned()),
                Token::CloseBrace,
                Token::Eof,
            ],
        );
    }
}
