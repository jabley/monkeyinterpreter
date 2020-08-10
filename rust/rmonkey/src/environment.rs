use crate::object::Object;
use std::collections::HashMap;

#[derive(Default)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn set(&mut self, key: &str, value: Object) {
        self.store.insert(key.to_string(), value);
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        match self.store.get(key) {
            Some(value) => Some(value.clone()),
            None => None,
        }
    }
}
