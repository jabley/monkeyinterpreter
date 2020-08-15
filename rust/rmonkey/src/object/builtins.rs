use super::{assert_argument_count, EvalError, EvalResult, Object};

pub fn lookup(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::BuiltIn(len)),
        "first" => Some(Object::BuiltIn(first)),
        "last" => Some(Object::BuiltIn(last)),
        "rest" => Some(Object::BuiltIn(rest)),
        "push" => Some(Object::BuiltIn(push)),
        "puts" => Some(Object::BuiltIn(puts)),
        _ => None,
    }
}

fn len(args: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &args)?;

    match &args[0] {
        Object::String(value) => Ok(Object::Integer(value.len() as i64)),
        Object::Array(elements) => Ok(Object::Integer(elements.len() as i64)),
        _ => Err(EvalError::UnsupportedArguments("len".to_string(), args)),
    }
}

fn first(args: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &args)?;

    match &args[0] {
        Object::Array(elements) => Ok(elements.first().cloned().unwrap_or(Object::Null)),
        _ => Err(EvalError::UnsupportedArguments("first".to_string(), args)),
    }
}

fn last(args: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &args)?;

    match &args[0] {
        Object::Array(elements) => Ok(elements.last().cloned().unwrap_or(Object::Null)),
        _ => Err(EvalError::UnsupportedArguments("last".to_string(), args)),
    }
}

fn rest(args: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &args)?;

    match &args[0] {
        Object::Array(elements) => Ok(if elements.is_empty() {
            Object::Null
        } else {
            Object::Array(elements[1..].to_vec())
        }),
        _ => Err(EvalError::UnsupportedArguments("rest".to_string(), args)),
    }
}

fn push(args: Vec<Object>) -> EvalResult {
    assert_argument_count(2, &args)?;

    match (&args[0], &args[1]) {
        (Object::Array(elements), value) => {
            let mut new_elements = elements.clone();
            new_elements.push(value.clone());
            Ok(Object::Array(new_elements))
        }
        _ => Err(EvalError::UnsupportedArguments("push".to_string(), args)),
    }
}

fn puts(args: Vec<Object>) -> EvalResult {
    for arg in args {
        println!("{}", arg);
    }

    Ok(Object::Null)
}
