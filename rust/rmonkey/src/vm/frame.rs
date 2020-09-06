use crate::code::Instructions;
use crate::object::Object;

/// Frame represents call-relevant information – the instructions and the instruction pointer.
/// It is short for "call frame" or "stack frame", and is sometimes called an "activation record" in
/// the literature
pub struct Frame {
    closure: Object,
    pub ip: usize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(closure: Object, base_pointer: usize) -> Self {
        match closure {
            Object::Closure(_, _) => Frame {
                closure,
                ip: 0,
                base_pointer,
            },
            _ => panic!("Object not supported {}", closure.type_name()),
        }
    }

    pub fn instructions(&self) -> Instructions {
        match &self.closure {
            Object::Closure(compiled_function, _) => match compiled_function.as_ref() {
                Object::CompiledFunction(instructions, _, _) => return instructions.clone(),
                _ => {}
            },
            _ => {}
        }
        panic!("Instructions not supported {}", self.closure.type_name());
    }
}
