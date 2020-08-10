use std::fmt;

#[derive(Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Integer(v) => write!(f, "{}", v),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Return(obj) => write!(f, "{}", obj),
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
        }
    }
}
