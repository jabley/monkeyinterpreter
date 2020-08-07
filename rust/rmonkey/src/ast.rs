#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Identifier(String),
    Integer(i64),
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
