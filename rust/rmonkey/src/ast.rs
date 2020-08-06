#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Let(String),
}

pub struct Program {
    pub statements: Vec<Statement>,
}
