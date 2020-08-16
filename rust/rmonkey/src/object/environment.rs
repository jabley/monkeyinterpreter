use crate::object::Object;
use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

#[derive(Default, Debug, Clone, Eq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn extend(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(Rc::clone(&outer)),
        }
    }

    pub fn set(&mut self, key: &str, value: Object) {
        self.store.insert(key.to_string(), value);
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        match self.store.get(key) {
            Some(value) => Some(value.clone()),
            None => self
                .outer
                .as_ref()
                .and_then(|outer| outer.borrow().get(key)),
        }
    }
}

impl Hash for Environment {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in &self.store {
            k.hash(state);
            v.hash(state);
        }

        if let Some(env) = (&self.outer).as_ref() {
            env.borrow().hash(state)
        }
    }
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        if other.store.len() != self.store.len() {
            return false;
        }

        match (self.outer.as_ref(), other.outer.as_ref()) {
            (Some(us), Some(them)) => us.eq(&them),
            (None, None) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Environment;
    use crate::object::Object;
    use std::{cell::RefCell, rc::Rc};

    #[test]
    fn recursive_lookup() {
        let outer = Rc::new(RefCell::new(Environment::new()));
        let enclosed = Environment::extend(Rc::clone(&outer));

        outer.borrow_mut().set("fib", Object::Integer(1));

        match enclosed.get("fib") {
            Some(Object::Integer(1)) => {}
            _ => assert!(false),
        }
    }
}
