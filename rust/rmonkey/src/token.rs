use std::fmt;

/// Enum representing common lexeme types.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Illegal(char),
    Eof,

    Ident(String),
    Int(i64),

    /// "="
    Assign,
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "!"
    Bang,
    /// "*"
    Asterisk,
    /// "/"
    Slash,
    /// "<"
    Lt,
    /// ">"
    Gt,
    /// "=="
    Eq,
    /// "!="
    Ne,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// ","
    Comma,
    /// ";"
    SemiColon,

    // Keywords
    /// "fn"
    Function,
    /// "let"
    Let,
    /// "if"
    If,
    /// "else"
    Else,
    /// "true"
    True,
    /// "false"
    False,
    /// "return"
    Return,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Token::Illegal(c) => write!(f, "ILLEGAL '{}'", c),
            Token::Eof => write!(f, "EOF"),

            Token::Ident(ident) => write!(f, "{}", ident),
            Token::Int(int) => write!(f, "{}", int),

            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),

            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),

            Token::Eq => write!(f, "="),
            Token::Ne => write!(f, "!="),

            Token::Comma => write!(f, ","),
            Token::SemiColon => write!(f, ";"),

            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),

            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
        }
    }
}

pub fn lookup_ident(ident: &str) -> Token {
    keyword_to_token(ident).unwrap_or_else(|| Token::Ident(ident.to_owned()))
}

fn keyword_to_token(keyword: &str) -> Option<Token> {
    match keyword {
        "fn" => Some(Token::Function),
        "let" => Some(Token::Let),
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        "true" => Some(Token::True),
        "false" => Some(Token::False),
        "return" => Some(Token::Return),
        _ => None,
    }
}
