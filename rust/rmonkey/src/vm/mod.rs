use crate::ast::InfixOperator;
use crate::code::{Instructions, Op};
use crate::compiler::Bytecode;
use crate::object::Object;
use byteorder::{BigEndian, ByteOrder};
use std::{error, fmt};

#[derive(Debug)]
pub enum VMError {
    StackOverflow(),
    StackEmpty(),
    TypeMismatch(InfixOperator, Object, Object),
}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VMError::StackOverflow() => write!(f, "StackOverflow"),
            VMError::StackEmpty() => write!(f, "Stack empty"),
            VMError::TypeMismatch(operator, left, right) => write!(
                f,
                "Type mismatch: {} {} {}",
                left.type_name(),
                operator,
                right.type_name()
            ),
        }
    }
}

impl error::Error for VMError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            VMError::StackOverflow() => None,
            VMError::StackEmpty() => None,
            VMError::TypeMismatch(_, _, _) => None,
        }
    }
}

/// STACK_SIZE is how deep we can go
const STACK_SIZE: usize = 2048;

/// VM is responsible for executing bytecode. It will do the fetch/decode/execute loop for instructions.
pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
    sp: usize, // aka stack pointer. Always points to the next value. Top of stack is `stack[sp-1]`
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        let mut stack = Vec::with_capacity(STACK_SIZE);
        stack.resize(STACK_SIZE, Object::Null);

        VM {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: stack,
            sp: 0,
        }
    }

    pub fn run(&mut self) -> Result<Object, VMError> {
        let mut ip = 0;

        while ip < self.instructions.len() {
            let op_code = self.instructions[ip];
            match Op::lookup_op(op_code) {
                Some(Op::Constant) => {
                    let const_index =
                        BigEndian::read_u16(&self.instructions[ip + 1..ip + 3]) as usize;
                    ip += 2;

                    self.push(self.constants[const_index].clone())?;
                }
                Some(Op::Add) => {
                    let right = self.pop()?;
                    let left = self.pop()?;

                    match (&left, &right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.push(Object::Integer(l + r))?
                        }
                        _ => return Err(VMError::TypeMismatch(InfixOperator::Plus, left, right)),
                    }
                }
                Some(Op::Pop) => {
                    self.pop()?;
                }
                _ => todo!("Unhandled op code {}", op_code),
            }
            ip += 1;
        }

        Ok(self.last_popped_stack_elem())
    }

    fn last_popped_stack_elem(&self) -> Object {
        self.stack[self.sp].clone()
    }

    fn push(&mut self, obj: Object) -> Result<(), VMError> {
        if self.sp >= STACK_SIZE {
            return Err(VMError::StackOverflow());
        }

        self.stack[self.sp] = obj;
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Result<Object, VMError> {
        if self.sp == 0 {
            Err(VMError::StackEmpty())
        } else {
            self.sp -= 1;
            Ok(self.stack[self.sp].clone())
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{ast::Program, compiler::Compiler, lexer::Lexer, object::Object, parser::Parser};
    use std::error;

    #[test]
    fn integer_arithmetic() {
        let tests = vec![
            ("1", Object::Integer(1)),
            ("2", Object::Integer(2)),
            ("1 + 2", Object::Integer(3)),
        ];

        for (input, expected) in tests {
            match run_vm_test(input, expected) {
                Ok(_) => {}
                Err(e) => panic!("{}", e),
            }
        }
    }

    fn run_vm_test(input: &str, expected: Object) -> Result<(), Box<dyn error::Error>> {
        let program = parse(input);

        let mut compiler = Compiler::new();

        let bytecode = compiler.compile(&program)?;
        let mut vm = VM::new(bytecode);
        let result = vm.run()?;

        assert_eq!(
            expected, result,
            "expected {} for '{}' but got {}",
            expected, input, result
        );

        Ok(())
    }

    fn parse(input: &str) -> Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        p.parse_program()
    }
}
