use crate::code::Instructions;
use crate::object::Object;

/// Frame represents call-relevant information – the instructions and the instruction pointer.
/// It is short for "call frame" or "stack frame", and is sometimes called an "activation record" in
/// the literature
pub struct Frame {
    pub free: Vec<Object>,
    pub ip: usize,
    pub base_pointer: usize,
    pub instructions: Instructions,
}

impl Frame {
    pub fn new(instructions: Instructions, base_pointer: usize, free: Vec<Object>) -> Self {
        Frame {
            free,
            ip: 0,
            base_pointer,
            instructions,
        }
    }
}
