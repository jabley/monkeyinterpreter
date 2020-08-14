use crate::object::Object;
use std::collections::HashMap;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn extend(outer: &Self) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(Box::new(outer.clone())),
        }
    }

    pub fn set(&mut self, key: &str, value: Object) {
        self.store.insert(key.to_string(), value);
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        match self.store.get(key) {
            Some(value) => Some(value.clone()),
            None => self.outer.as_ref().and_then(|outer| outer.get(key)),
        }
    }
}
