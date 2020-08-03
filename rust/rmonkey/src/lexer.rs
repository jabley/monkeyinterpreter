use std::iter::Peekable;
use std::str::Chars;

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

    pub fn next_token(&mut self) -> Token {
        match self.read_char() {
            Some('=') => Token::Assign,
            Some('+') => Token::Plus,
            Some('(') => Token::OpenParen,
            Some(')') => Token::CloseParen,
            Some('{') => Token::OpenBrace,
            Some('}') => Token::CloseBrace,
            Some(',') => Token::Comma,
            Some(';') => Token::SemiColon,
            _ => Token::Eof,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let mut l = lexer::Lexer::new(input);

        let expected_tokens = vec![
            token::Token::Assign,
            token::Token::Plus,
            token::Token::OpenParen,
            token::Token::CloseParen,
            token::Token::OpenBrace,
            token::Token::CloseBrace,
            token::Token::Comma,
            token::Token::SemiColon,
            token::Token::Eof,
        ];

        for expected in expected_tokens {
            let actual = l.next_token();

            assert_eq!(expected, actual);
        }
    }
}
