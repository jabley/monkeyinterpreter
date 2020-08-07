use std::fmt;

#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Boolean(bool),
    FunctionLiteral(Vec<String>, BlockStatement),
    Identifier(String),
    If(Box<Expression>, BlockStatement, Option<BlockStatement>),
    IntegerLiteral(i64),
    Infix(InfixOperator, Box<Expression>, Box<Expression>),
    Prefix(PrefixOperator, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Boolean(b) => write!(f, "{}", b),
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
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
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
#[derive(Debug, Eq, PartialEq)]
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

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Let(String),
    Return,
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(ident) => write!(f, "let {};", ident),
            Statement::Return => write!(f, "return;"),
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
