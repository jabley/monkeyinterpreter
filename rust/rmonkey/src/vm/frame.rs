use crate::code::Instructions;
use crate::object::Object;

/// Frame represents call-relevant information – the instructions and the instruction pointer.
/// It is short for "call frame" or "stack frame", and is sometimes called an "activation record" in
/// the literature
pub struct Frame {
    func: Object,
    pub ip: usize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(func: Object, base_pointer: usize) -> Self {
        match func {
            Object::CompiledFunction(_, _, _) => Frame {
                func,
                ip: 0,
                base_pointer,
            },
            _ => panic!("Object not supported {}", func.type_name()),
        }
    }

    pub fn instructions(&self) -> Instructions {
        if let Object::CompiledFunction(instructions, _, _) = &self.func {
            instructions.clone()
        } else {
            panic!("Instructions not supported {}", self.func.type_name());
        }
    }
}
