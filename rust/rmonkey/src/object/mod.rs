use crate::ast::{BlockStatement, InfixOperator, PrefixOperator};
pub use crate::object::environment::Environment;
use std::fmt;

pub mod builtins;
pub mod environment;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Function(Vec<String>, BlockStatement, Environment),
    String(String),
    BuiltIn(BuiltIn),
    Array(Vec<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Integer(v) => write!(f, "{}", v),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Return(obj) => write!(f, "{}", obj),
            Object::Function(parameters, body, _) => {
                write!(f, "fn({}) {{\n{}\n}}", parameters.join(", "), body)
            }
            Object::String(s) => write!(f, "{}", s),
            Object::BuiltIn(_) => write!(f, "built-in function"),
            Object::Array(elements) => write!(
                f,
                "[{}]",
                elements
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Null => false,
            Object::Boolean(b) => *b,
            _ => true,
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            Object::Boolean(_) => "BOOLEAN",
            Object::Integer(_) => "INTEGER",
            Object::Null => "NULL",
            Object::Return(_) => "RETURN",
            Object::Function(_, _, _) => "FUNCTION",
            Object::String(_) => "STRING",
            Object::BuiltIn(_) => "BUILTIN",
            Object::Array(_) => "ARRAY",
        }
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum HashKey {
    Boolean(bool),
    Integer(i64),
    String(String),
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HashKey::Boolean(b) => write!(f, "{}", b),
            HashKey::Integer(i) => write!(f, "{}", i),
            HashKey::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

impl HashKey {
    pub fn from_object(obj: Object) -> Result<HashKey, EvalError> {
        match obj {
            Object::Boolean(b) => Ok(HashKey::Boolean(b)),
            Object::Integer(i) => Ok(HashKey::Integer(i)),
            Object::String(s) => Ok(HashKey::String(s)),
            _ => Err(EvalError::UnusableHashKey(obj)),
        }
    }
}

pub type EvalResult = std::result::Result<Object, EvalError>;
pub type BuiltIn = fn(Vec<Object>) -> EvalResult;

pub enum EvalError {
    IdentifierNotFound(String),
    NotCallable(Object),
    UnsupportedArguments(String, Vec<Object>),
    UnsupportedInfixOperator(InfixOperator, Object, Object),
    UnsupportedIndexOperator(Object, Object),
    UnsupportedPrefixOperator(PrefixOperator, Object),
    UnusableHashKey(Object),
    TypeMismatch(InfixOperator, Object, Object),
    WrongArgumentCount { expected: usize, given: usize },
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::UnsupportedPrefixOperator(operator, obj) => {
                write!(f, "Unknown operator: {}{}", operator, obj.type_name())
            }
            EvalError::TypeMismatch(operator, left, right) => write!(
                f,
                "Type mismatch: {} {} {}",
                left.type_name(),
                operator,
                right.type_name()
            ),
            EvalError::UnsupportedInfixOperator(operator, left, right) => write!(
                f,
                "Unknown operator: {} {} {}",
                left.type_name(),
                operator,
                right.type_name()
            ),
            EvalError::IdentifierNotFound(name) => write!(f, "Identifier not found: {}", name),
            EvalError::NotCallable(obj) => write!(f, "Not a function: {}", obj),
            EvalError::UnsupportedArguments(function, args) => write!(
                f,
                "argument to `{}` not supported, got {}",
                function,
                args.iter()
                    .map(|a| a.type_name())
                    .collect::<Vec<&str>>()
                    .join(", ")
            ),
            EvalError::WrongArgumentCount { expected, given } => write!(
                f,
                "wrong number of arguments. got={}, want={}",
                given, expected
            ),
            EvalError::UnsupportedIndexOperator(l, i) => {
                write!(f, "index operator not supported: {}[{}]", l, i)
            }
            EvalError::UnusableHashKey(obj) => {
                write!(f, "Unusable as hash key: {}", obj.type_name())
            }
        }
    }
}

pub fn assert_argument_count(expected: usize, arguments: &[Object]) -> Result<(), EvalError> {
    if arguments.len() != expected {
        return Err(EvalError::WrongArgumentCount {
            expected,
            given: arguments.len(),
        });
    }
    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn equality() {
        assert_eq!(Object::Integer(1), Object::Integer(1));
        assert_eq!(
            Object::String("hello".to_string()),
            Object::String("hello".to_string())
        );
        assert_ne!(Object::Integer(1), Object::Integer(2));
        assert_ne!(
            Object::String("hello".to_string()),
            Object::String("world".to_string())
        );
        assert_ne!(Object::String("1".to_string()), Object::Integer(1));
    }
}
