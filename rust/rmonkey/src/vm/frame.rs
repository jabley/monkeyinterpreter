use crate::code::Instructions;
use crate::object::Object;

/// Frame represents call-relevant information – the instructions and the instruction pointer.
/// It is short for "call frame" or "stack frame", and is sometimes called an "activation record" in
/// the literature
pub struct Frame {
    func: Object,
    pub ip: usize,
}

impl Frame {
    pub fn new(func: Object) -> Self {
        match func {
            Object::CompiledFunction(_) => Frame { func, ip: 0 },
            _ => panic!("Object not supported {}", func.type_name()),
        }
    }

    pub fn instructions(&self) -> Instructions {
        if let Object::CompiledFunction(instructions) = &self.func {
            instructions.clone()
        } else {
            panic!("Instructions not supported {}", self.func.type_name());
        }
    }
}
