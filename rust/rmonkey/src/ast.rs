#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Identifier(String),
    Integer(i64),
    Infix(InfixOperator, Box<Expression>, Box<Expression>),
    Prefix(PrefixOperator, Box<Expression>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum PrefixOperator {
    Bang,
    Minus,
}

#[derive(Debug, Eq, PartialEq)]
pub enum InfixOperator {
    Eq,
    NotEq,
    Lt,
    Gt,
    Plus,
    Minus,
    Asterisk,
    Slash,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Let(String),
    Return,
    Expression(Expression),
}

pub struct Program {
    pub statements: Vec<Statement>,
}
