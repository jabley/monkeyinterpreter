use crate::ast::{BlockStatement, InfixOperator, PrefixOperator};
use crate::code::{Instructions, InstructionsFns};
use crate::object::builtins::BuiltIn;
pub use crate::object::environment::Environment;
use indexmap::IndexMap;
use std::fmt::Formatter;
use std::rc::Rc;
use std::{fmt, hash::Hash};

pub mod builtins;
pub mod environment;

#[derive(Clone, Debug, Eq)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    /// Closure wraps a CompiledFunction and any free variables that are referenced. See
    /// https://en.wikipedia.org/wiki/Free_variables_and_bound_variables
    Closure(Rc<Closure>),
    CompiledFunction(Rc<CompiledFunction>),
    Return(Rc<Object>),
    Function(Vec<String>, BlockStatement, Environment),
    String(String),
    BuiltIn(BuiltIn),
    Array(Rc<Vec<Rc<Object>>>),
    Hash(Rc<IndexMap<HashKey, Rc<Object>>>),
}

impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Object::Null => 37.hash(state),
            Object::Integer(i) => i.hash(state),
            Object::Boolean(b) => b.hash(state),
            Object::CompiledFunction(_compiled_function) => panic!("Can't hash a CompiledFunction"),
            Object::Return(_o) => panic!("Can't hash a Return"),
            Object::Function(_parameters, _body, _env) => panic!("Can't hash a Function"),
            Object::String(s) => s.hash(state),
            Object::BuiltIn(_func) => panic!("Can't hash a BuiltIn"),
            Object::Array(_elements) => panic!("Can't hash an Array"),
            Object::Hash(_map) => panic!("Can't hash a Hash"),
            Object::Closure(_closure) => panic!("Can't hash a Closure"),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Null, Object::Null) => true,
            (Object::Integer(first), Object::Integer(second)) => first == second,
            (Object::Boolean(first), Object::Boolean(second)) => first == second,
            (Object::Return(first), Object::Return(second)) => first == second,
            (
                Object::Function(first_params, first_body, first_env),
                Object::Function(second_params, second_body, second_env),
            ) => {
                first_params == second_params
                    && first_body == second_body
                    && first_env == second_env
            }
            (Object::String(first), Object::String(second)) => first == second,
            (Object::BuiltIn(first), Object::BuiltIn(second)) => first == second,
            (Object::Array(first), Object::Array(second)) => first == second,
            (Object::Hash(first), Object::Hash(second)) => first == second,
            _ => false,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Integer(v) => write!(f, "{}", v),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::CompiledFunction(compiled_function) => {
                write!(f, "{}", compiled_function.to_string())
            }
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
            Object::Hash(map) => write!(
                f,
                "{{{}}}",
                map.iter()
                    .map(|(k, v)| k.to_string() + ": " + &v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Object::Closure(closure) => write!(f, "{}", closure),
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
            Object::CompiledFunction(_) => "COMPILED_FUNCTION",
            Object::Integer(_) => "INTEGER",
            Object::Null => "NULL",
            Object::Return(_) => "RETURN",
            Object::Function(_, _, _) => "FUNCTION",
            Object::String(_) => "STRING",
            Object::BuiltIn(_) => "BUILTIN",
            Object::Array(_) => "ARRAY",
            Object::Hash(_) => "HASH",
            Object::Closure(_) => "CLOSURE",
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct Closure {
    pub func: Rc<CompiledFunction>,
    pub free: Vec<Rc<Object>>,
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}]",
            self.func,
            self.free
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct CompiledFunction {
    pub instructions: Instructions,
    pub num_locals: usize,
    pub num_parameters: usize,
}

impl fmt::Display for CompiledFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.instructions.to_string())
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
    pub fn from_object(obj: &Object) -> Result<HashKey, EvalError> {
        match obj {
            Object::Boolean(b) => Ok(HashKey::Boolean(*b)),
            Object::Integer(i) => Ok(HashKey::Integer(*i)),
            Object::String(s) => Ok(HashKey::String(s.to_owned())),
            _ => Err(EvalError::UnusableHashKey(Rc::new(obj.clone()))),
        }
    }
}

pub type EvalResult = std::result::Result<Rc<Object>, EvalError>;
pub type BuiltInFunction = fn(&Vec<Rc<Object>>) -> EvalResult;

#[derive(Debug, PartialEq)]
pub enum EvalError {
    IdentifierNotFound(String),
    NotCallable(Object),
    UnsupportedArguments(String, Vec<Rc<Object>>),
    UnsupportedInfixOperator(InfixOperator, Object, Object),
    UnsupportedIndexOperator(Rc<Object>, Rc<Object>),
    UnsupportedPrefixOperator(PrefixOperator, Rc<Object>),
    UnusableHashKey(Rc<Object>),
    TypeMismatch(InfixOperator, Rc<Object>, Rc<Object>),
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

pub fn assert_argument_count(expected: usize, arguments: &[Rc<Object>]) -> Result<(), EvalError> {
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
