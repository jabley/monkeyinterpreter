use indexmap::IndexMap;
use std::{fmt, hash::Hash};

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression {
    ArrayLiteral(Vec<Expression>),
    Boolean(bool),
    Call(Box<Expression>, Vec<Expression>),
    FunctionLiteral(Vec<String>, BlockStatement),
    HashLiteral(HashLiteral),
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
            Expression::HashLiteral(hash) => write!(f, "{}", hash),
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

#[derive(Debug, Default, Eq)]
pub struct HashLiteral {
    pub pairs: IndexMap<Expression, Expression>,
}

impl HashLiteral {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, k: Expression, v: Expression) -> Option<Expression> {
        self.pairs.insert(k, v)
    }
}

impl Hash for HashLiteral {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in &self.pairs {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl PartialEq for HashLiteral {
    fn eq(&self, other: &Self) -> bool {
        if other.pairs.len() != self.pairs.len() {
            return false;
        }

        for (k, my_v) in &self.pairs {
            match other.pairs.get_key_value(k) {
                Some((_, other_v)) => {
                    if other_v != my_v {
                        return false; // values don't match
                    }
                }
                _ => return false, // No such element for that key
            }
        }

        true
    }
}

impl Clone for HashLiteral {
    fn clone(&self) -> Self {
        let mut res = HashLiteral::new();

        for (k, v) in &self.pairs {
            res.pairs.insert(k.clone(), v.clone());
        }

        res
    }
}

impl fmt::Display for HashLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.pairs
                .iter()
                .map(|p| p.0.to_string() + ": " + &p.1.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
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

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
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
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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

#[cfg(test)]
mod tests {
    use super::{Expression, HashLiteral};

    #[test]
    fn equality() {
        let mut populated_hash = HashLiteral::new();
        populated_hash.insert(
            Expression::StringLiteral("hello".to_owned()),
            Expression::IntegerLiteral(17),
        );
        populated_hash.insert(
            Expression::StringLiteral("world".to_owned()),
            Expression::IntegerLiteral(19),
        );

        assert_expressions_equal(vec![
            Expression::IntegerLiteral(37),
            Expression::ArrayLiteral(vec![]),
            Expression::ArrayLiteral(vec![Expression::Boolean(true), Expression::Boolean(false)]),
            Expression::ArrayLiteral(vec![Expression::Boolean(true), Expression::Boolean(false)]),
            Expression::HashLiteral(HashLiteral::new()),
            Expression::HashLiteral(populated_hash),
        ]);
    }

    fn assert_expressions_equal(tests: Vec<Expression>) {
        for exp in tests {
            assert_eq!(exp, exp.clone());
        }
    }

    #[test]
    fn inequality() {
        let first =
            Expression::ArrayLiteral(vec![Expression::Boolean(true), Expression::Boolean(false)]);
        let second =
            Expression::ArrayLiteral(vec![Expression::Boolean(false), Expression::Boolean(false)]);

        assert_ne!(first, second);
    }
}
