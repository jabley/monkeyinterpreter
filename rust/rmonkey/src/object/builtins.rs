use super::{assert_argument_count, EvalError, EvalResult, Object};
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;
use std::slice::Iter;

#[repr(u8)]
#[derive(PartialOrd, PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub enum BuiltIn {
    Len,
    Puts,
    First,
    Last,
    Rest,
    Push,
}

impl fmt::Display for BuiltIn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BuiltIn::Len => write!(f, "len"),
            BuiltIn::Puts => write!(f, "puts"),
            BuiltIn::First => write!(f, "first"),
            BuiltIn::Last => write!(f, "last"),
            BuiltIn::Rest => write!(f, "rest"),
            BuiltIn::Push => write!(f, "push"),
        }
    }
}

impl BuiltIn {
    pub fn iterator() -> Iter<'static, BuiltIn> {
        static BUILTINS: [BuiltIn; 6] = [
            BuiltIn::Len,
            BuiltIn::Puts,
            BuiltIn::First,
            BuiltIn::Last,
            BuiltIn::Rest,
            BuiltIn::Push,
        ];
        BUILTINS.iter()
    }

    pub fn lookup(name: &str) -> Option<Object> {
        match name {
            "len" => Some(Object::BuiltIn(BuiltIn::Len)),
            "puts" => Some(Object::BuiltIn(BuiltIn::Puts)),
            "first" => Some(Object::BuiltIn(BuiltIn::First)),
            "last" => Some(Object::BuiltIn(BuiltIn::Last)),
            "rest" => Some(Object::BuiltIn(BuiltIn::Rest)),
            "push" => Some(Object::BuiltIn(BuiltIn::Push)),
            _ => None,
        }
    }

    pub fn apply(&self, args: &[Rc<Object>]) -> EvalResult {
        match self {
            BuiltIn::Len => len(args),
            BuiltIn::Puts => puts(args),
            BuiltIn::First => first(args),
            BuiltIn::Last => last(args),
            BuiltIn::Rest => rest(args),
            BuiltIn::Push => push(args),
        }
    }
}

fn len(args: &[Rc<Object>]) -> EvalResult {
    assert_argument_count(1, &args)?;

    match &*args[0] {
        Object::String(s) => Ok(Rc::new(Object::Integer(s.len() as i64))),
        Object::Array(elements) => Ok(Rc::new(Object::Integer(elements.len() as i64))),
        _ => Err(EvalError::UnsupportedArguments(
            "len".to_string(),
            args.to_vec(),
        )),
    }
}

fn first(args: &[Rc<Object>]) -> EvalResult {
    assert_argument_count(1, &args)?;

    match &*args[0] {
        Object::Array(elements) => Ok(elements
            .first()
            .cloned()
            .unwrap_or_else(|| Rc::new(Object::Null))),
        _ => Err(EvalError::UnsupportedArguments(
            "first".to_string(),
            args.to_vec(),
        )),
    }
}

fn last(args: &[Rc<Object>]) -> EvalResult {
    assert_argument_count(1, &args)?;

    match &*args[0] {
        Object::Array(elements) => Ok(elements
            .last()
            .cloned()
            .unwrap_or_else(|| Rc::new(Object::Null))),
        _ => Err(EvalError::UnsupportedArguments(
            "last".to_string(),
            args.to_vec(),
        )),
    }
}

fn rest(args: &[Rc<Object>]) -> EvalResult {
    assert_argument_count(1, &args)?;

    match &*args[0] {
        Object::Array(elements) => Ok(if elements.is_empty() {
            Rc::new(Object::Null)
        } else {
            Rc::new(Object::Array(Rc::new(elements[1..].to_owned())))
        }),
        _ => Err(EvalError::UnsupportedArguments(
            "rest".to_string(),
            args.to_vec(),
        )),
    }
}

fn push(args: &[Rc<Object>]) -> EvalResult {
    assert_argument_count(2, &args)?;

    let array = &*Rc::clone(args.first().unwrap());
    let obj = Rc::clone(args.last().unwrap());

    match array {
        Object::Array(elements) => {
            let mut new_elements = elements.to_vec();
            new_elements.push(obj);
            Ok(Rc::new(Object::Array(Rc::new(new_elements.to_vec()))))
        }
        _ => Err(EvalError::UnsupportedArguments(
            "push".to_string(),
            args.to_vec(),
        )),
    }
}

fn puts(args: &[Rc<Object>]) -> EvalResult {
    for arg in args {
        println!("{}", arg);
    }

    Ok(Rc::new(Object::Null))
}
