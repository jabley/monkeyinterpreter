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
            Some(',') => Token::Comma,
            Some(';') => Token::SemiColon,
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
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'
}

fn is_id_continue(c: char) -> bool {
    is_id_start(c) || ('0' <= c && c <= '9')
}

#[cfg(test)]
mod tests {
    use crate::*;

    fn test_lexing(input: &str, expected_tokens: Vec<token::Token>) {
        let mut l = lexer::Lexer::new(input);

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
                token::Token::Assign,
                token::Token::Plus,
                token::Token::OpenParen,
                token::Token::CloseParen,
                token::Token::OpenBrace,
                token::Token::CloseBrace,
                token::Token::Comma,
                token::Token::SemiColon,
                token::Token::Eof,
            ],
        );
    }

    #[test]
    fn test_next_token_on_monkey() {
        test_lexing(
            "let five = 5;

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
",
            vec![
                token::Token::Let,
                token::Token::Ident("five".to_owned()),
                token::Token::Assign,
                token::Token::Int(5),
                token::Token::SemiColon,
                token::Token::Let,
                token::Token::Ident("ten".to_owned()),
                token::Token::Assign,
                token::Token::Int(10),
                token::Token::SemiColon,
                token::Token::Let,
                token::Token::Ident("add".to_owned()),
                token::Token::Assign,
                token::Token::Function,
                token::Token::OpenParen,
                token::Token::Ident("x".to_owned()),
                token::Token::Comma,
                token::Token::Ident("y".to_owned()),
                token::Token::CloseParen,
                token::Token::OpenBrace,
                token::Token::Ident("x".to_owned()),
                token::Token::Plus,
                token::Token::Ident("y".to_owned()),
                token::Token::SemiColon,
                token::Token::CloseBrace,
                token::Token::SemiColon,
                token::Token::Let,
                token::Token::Ident("result".to_owned()),
                token::Token::Assign,
                token::Token::Ident("add".to_owned()),
                token::Token::OpenParen,
                token::Token::Ident("five".to_owned()),
                token::Token::Comma,
                token::Token::Ident("ten".to_owned()),
                token::Token::CloseParen,
                token::Token::SemiColon,
                token::Token::Bang,
                token::Token::Minus,
                token::Token::Slash,
                token::Token::Asterisk,
                token::Token::Int(5),
                token::Token::SemiColon,
                token::Token::Int(5),
                token::Token::Lt,
                token::Token::Int(10),
                token::Token::Gt,
                token::Token::Int(5),
                token::Token::SemiColon,
                token::Token::If,
                token::Token::OpenParen,
                token::Token::Int(5),
                token::Token::Lt,
                token::Token::Int(10),
                token::Token::CloseParen,
                token::Token::OpenBrace,
                token::Token::Return,
                token::Token::True,
                token::Token::SemiColon,
                token::Token::CloseBrace,
                token::Token::Else,
                token::Token::OpenBrace,
                token::Token::Return,
                token::Token::False,
                token::Token::SemiColon,
                token::Token::CloseBrace,
                token::Token::Int(10),
                token::Token::Eq,
                token::Token::Int(10),
                token::Token::SemiColon,
                token::Token::Int(10),
                token::Token::Ne,
                token::Token::Int(9),
                token::Token::SemiColon,
                token::Token::Eof,
            ],
        );
    }
}
