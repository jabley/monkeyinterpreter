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

    // Keyworlds
    /// "fn"
    Function,
    /// "let"
    Let,
}

pub fn lookup_ident(ident: &str) -> Token {
    keyword_to_token(ident).unwrap_or_else(|| Token::Ident(ident.to_owned()))
}

fn keyword_to_token(keyword: &str) -> Option<Token> {
    match keyword {
        "fn" => Some(Token::Function),
        "let" => Some(Token::Let),
        _ => None,
    }
}
