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
