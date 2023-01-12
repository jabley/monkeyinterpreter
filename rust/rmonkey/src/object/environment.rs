use crate::object::Object;
use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

#[derive(Default, Debug, Clone, Eq)]
pub struct Environment3 {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment3>>>,
}

impl Environment3 {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn extend(outer: Rc<RefCell<Environment3>>) -> Self {
        Environment3 {
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

impl Hash for Environment3 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in &self.store {
            k.hash(state);
            v.hash(state);
        }

        if let Some(env) = self.outer.as_ref() {
            env.borrow().hash(state)
        }
    }
}

impl PartialEq for Environment3 {
    fn eq(&self, other: &Self) -> bool {
        if !self.store.eq(&other.store) {
            return false;
        }

        match (self.outer.as_ref(), other.outer.as_ref()) {
            (Some(us), Some(them)) => us.eq(them),
            (None, None) => true,
            _ => false,
        }
    }
}

impl Drop for Environment3 {
    fn drop(&mut self) {
        let mut outer = self.outer.take();

        while let Some(node) = outer {
            if let Ok(node) = Rc::try_unwrap(node) {
                outer = node.borrow_mut().outer.take();
            } else {
                break;
            }
        }
    }
}

#[derive(Debug, Clone, Eq)]
pub struct Environment {
    context: Link,
}

type Link = Option<Rc<RefCell<Context>>>;

#[derive(Debug, Eq)]
struct Context {
    store: HashMap<String, Rc<Object>>,
    next: Link,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            context: Some(Context::new()),
        }
    }

    pub fn extend(&self) -> Self {
        let context = Context::new();
        context.borrow_mut().next = Some(Rc::clone(self.context.as_ref().unwrap()));
        Environment {
            context: Some(context),
        }
    }

    pub fn set(&mut self, key: &str, value: Rc<Object>) {
        self.context
            .as_ref()
            .map(|node| node.borrow_mut().store.insert(key.to_string(), value));
    }

    pub fn get(&self, key: &str) -> Option<Rc<Object>> {
        let context_ref = self.context.as_ref();

        match context_ref.unwrap().borrow().store.get(key) {
            Some(value) => Some(value.clone()),
            None => context_ref.and_then(|node| {
                node.borrow()
                    .next
                    .as_ref()
                    .and_then(|outer| outer.borrow().store.get(key).cloned())
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

impl Drop for Environment {
    fn drop(&mut self) {
        let mut context = self.context.take();

        while let Some(node) = context {
            if let Ok(node) = Rc::try_unwrap(node) {
                context = node.borrow_mut().next.take();
            } else {
                break;
            }
        }
    }
}

impl PartialEq for Context {
    fn eq(&self, other: &Self) -> bool {
        if !self.store.eq(&other.store) {
            return false;
        }

        match (self.next.as_ref(), other.next.as_ref()) {
            (Some(us), Some(them)) => us.eq(them),
            (None, None) => true,
            _ => false,
        }
    }
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        self.context.eq(&other.context)
    }
}

impl Hash for Environment {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if let Some(h) = &self.context {
            h.borrow().hash(state);
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Environment {
            context: Some(Context::new()),
        }
    }
}

impl Hash for Context {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in &self.store {
            k.hash(state);
            v.hash(state);
        }

        if let Some(c) = &self.next {
            c.borrow().hash(state);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Environment, Environment3};
    use crate::object::Object;
    use std::{cell::RefCell, rc::Rc};

    #[test]
    fn recursive_lookup() {
        let outer = Rc::new(RefCell::new(Environment3::new()));
        let enclosed = Environment3::extend(Rc::clone(&outer));

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
        let mut outer = Environment::new();
        let enclosed = outer.extend();

        match enclosed.get("fib") {
            None => {}
            _ => assert!(false, "Value hasn't been put into outer yet"),
        }

        outer.set("fib", Rc::new(Object::Integer(1)));

        match enclosed.get("fib") {
            Some(value) => assert_eq!(&*value, &Object::Integer(1)),
            _ => assert!(false, "Value should be accessible from the outer reference"),
        }
    }
}
