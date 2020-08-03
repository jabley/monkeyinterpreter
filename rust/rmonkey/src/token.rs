/// Enum representing common lexeme types.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Illegal,
    Eof,

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
}
