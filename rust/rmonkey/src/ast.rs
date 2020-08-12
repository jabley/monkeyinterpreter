use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    ArrayLiteral(Vec<Expression>),
    Boolean(bool),
    Call(Box<Expression>, Vec<Expression>),
    FunctionLiteral(Vec<String>, BlockStatement),
    Identifier(String),
    If(Box<Expression>, BlockStatement, Option<BlockStatement>),
    IndexExpression(Box<Expression>, Box<Expression>),
    IntegerLiteral(i64),
    Infix(InfixOperator, Box<Expression>, Box<Expression>),
    Prefix(PrefixOperator, Box<Expression>),
    StringLiteral(String),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::Call(function, parameters) => {
                write!(f, "{}({})", function, comma_separated(parameters),)
            }
            Expression::FunctionLiteral(parameters, body) => {
                write!(f, "fn({}) {}", parameters.join(", "), body)
            }
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::If(condition, consequence, alternative) => {
                write!(f, "if {} {}", condition, consequence)?;
                if let Some(alt) = alternative {
                    write!(f, " else {}", alt)?;
                }
                Ok(())
            }
            Expression::IntegerLiteral(value) => write!(f, "{}", value),
            Expression::Prefix(operator, exp) => write!(f, "({}{})", operator, exp),
            Expression::Infix(operator, left, right) => {
                write!(f, "({} {} {})", left, operator, right)
            }
            Expression::StringLiteral(s) => write!(f, "\"{}\"", s),
            Expression::ArrayLiteral(elements) => write!(f, "[{}]", comma_separated(elements)),
            Expression::IndexExpression(left, index) => write!(f, "{}[{}]", left, index),
        }
    }
}

fn comma_separated(values: &[Expression]) -> String {
    values
        .iter()
        .map(|a| a.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PrefixOperator {
    Bang,
    Minus,
}

impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrefixOperator::Bang => write!(f, "!"),
            PrefixOperator::Minus => write!(f, "-"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
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

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InfixOperator::Eq => write!(f, "=="),
            InfixOperator::NotEq => write!(f, "!="),
            InfixOperator::Lt => write!(f, "<"),
            InfixOperator::Gt => write!(f, ">"),
            InfixOperator::Plus => write!(f, "+"),
            InfixOperator::Minus => write!(f, "-"),
            InfixOperator::Asterisk => write!(f, "*"),
            InfixOperator::Slash => write!(f, "/"),
        }
    }
}

/// BlockStatement allows for blocks of code, as part of an if expression, for example.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{{ {} }}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    Let(String, Expression),
    Return(Option<Expression>),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(ident, value) => write!(f, "let {} = {};", ident, value),
            Statement::Return(None) => write!(f, "return;"),
            Statement::Return(Some(value)) => write!(f, "return {};", value),
            Statement::Expression(exp) => write!(f, "{};", exp),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}
