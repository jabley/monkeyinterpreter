use crate::code::Instructions;
use crate::object::Object;

/// Frame represents call-relevant information – the instructions and the instruction pointer.
/// It is short for "call frame" or "stack frame", and is sometimes called an "activation record" in
/// the literature
pub struct Frame {
    pub closure: Object,
    pub ip: usize,
    pub base_pointer: usize,
    pub instructions: Instructions,
}

impl Frame {
    pub fn new(closure: Object, base_pointer: usize) -> Self {
        if let Object::Closure(compiled_function, _) = &closure {
            if let Object::CompiledFunction(instructions, _, _) = compiled_function.as_ref() {
                return Frame {
                    closure: closure.clone(),
                    ip: 0,
                    base_pointer,
                    instructions: instructions.clone(),
                }
            }
        }
        panic!("Object not supported {}", closure.type_name())
    }
}
