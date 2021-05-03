use crate::code::Instructions;
use crate::object::Closure;
use std::rc::Rc;

/// Frame represents call-relevant information – the instructions and the instruction pointer.
/// It is short for "call frame" or "stack frame", and is sometimes called an "activation record" in
/// the literature
pub struct Frame {
    pub cl: Rc<Closure>,
    pub ip: usize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn get_instructions(&self) -> &Instructions {
        &self.cl.func.instructions
    }
}
