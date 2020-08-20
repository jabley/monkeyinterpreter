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

pub struct Environment2 {
    head: Link,
}

type Link = Option<Rc<RefCell<Context>>>;

struct Context {
    store: HashMap<String, Object>,
    next: Link,
}

impl Environment2 {
    pub fn new() -> Self {
        Environment2 {
            head: Some(Context::new()),
        }
    }

    pub fn extend(&mut self) -> Self {
        let head = Context::new();
        head.borrow_mut().next = Some(Rc::clone(self.head.as_ref().unwrap()));
        Environment2 { head: Some(head) }
    }

    pub fn set(&mut self, key: &str, value: Object) {
        self.head
            .as_ref()
            .map(|node| node.borrow_mut().store.insert(key.to_string(), value));
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        let foo = self.head.as_ref();

        match foo.unwrap().borrow().store.get(key) {
            Some(value) => Some(value.clone()),
            None => foo.and_then(|node| {
                node.borrow()
                    .next
                    .as_ref()
                    .and_then(|outer| match outer.borrow().store.get(key) {
                        Some(value) => Some(value.clone()),
                        _ => None,
                    })
            }),
        }
    }
}

impl Context {
    fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Context {
            store: HashMap::new(),
            next: None,
        }))
    }
}

impl Drop for Environment2 {
    fn drop(&mut self) {
        let mut head = self.head.take();
        while let Some(node) = head {
            if let Ok(node) = Rc::try_unwrap(node) {
                head = node.borrow_mut().next.take();
            } else {
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Environment, Environment2};
    use crate::object::Object;
    use std::{cell::RefCell, rc::Rc};

    #[test]
    fn recursive_lookup() {
        let outer = Rc::new(RefCell::new(Environment::new()));
        let enclosed = Environment::extend(Rc::clone(&outer));

        match enclosed.get("fib") {
            None => {}
            _ => assert!(false, "Value hasn't been put into outer yet"),
        }

        outer.borrow_mut().set("fib", Object::Integer(1));

        match enclosed.get("fib") {
            Some(Object::Integer(1)) => {}
            _ => assert!(false, "Value should be accessible from the outer reference"),
        }
    }

    #[test]
    fn hide_internals_usage() {
        let mut outer = Environment2::new();
        let enclosed = outer.extend();

        match enclosed.get("fib") {
            None => {}
            _ => assert!(false, "Value hasn't been put into outer yet"),
        }

        outer.set("fib", Object::Integer(1));

        match enclosed.get("fib") {
            Some(Object::Integer(1)) => {}
            _ => assert!(false, "Value should be accessible from the outer reference"),
        }
    }
}
