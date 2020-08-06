#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Let(String),
    Return,
}

pub struct Program {
    pub statements: Vec<Statement>,
}
