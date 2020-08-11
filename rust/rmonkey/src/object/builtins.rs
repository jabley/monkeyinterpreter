use super::{assert_argument_count, EvalError, EvalResult, Object};

pub fn lookup(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::BuiltIn(len)),
        _ => None,
    }
}

fn len(args: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &args)?;

    match &args[0] {
        Object::String(value) => Ok(Object::Integer(value.len() as i64)),
        _ => Err(EvalError::UnsupportedArguments("len".to_string(), args)),
    }
}
