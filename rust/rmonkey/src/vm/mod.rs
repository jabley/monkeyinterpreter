use crate::ast::InfixOperator;
use crate::code::{Instructions, Op};
use crate::compiler::Bytecode;
use crate::object::EvalError;
use crate::object::{HashKey, Object};
use byteorder::{BigEndian, ByteOrder};
use indexmap::IndexMap;
use std::{error, fmt};

#[derive(Debug)]
pub enum VMError {
    Eval(EvalError),
    StackOverflow(),
    StackEmpty(),
    TypeMismatch(InfixOperator, Object, Object),
    UnknownOperation(Op),
    UnsupportedNegationType(Object),
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
            VMError::UnknownOperation(op) => write!(f, "Unknown operation {}", op.name()),
            VMError::UnsupportedNegationType(obj) => {
                write!(f, "Unsupported type for negation: {}", obj.type_name())
            }
            VMError::Eval(e) => write!(f, "{}", e),
        }
    }
}

impl error::Error for VMError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

/// STACK_SIZE is how deep we can go
const STACK_SIZE: usize = 2048;

/// GLOBALS_SIZE is the maximum number of global variables that we can support. This is tied to how
/// wide the operands of Op::SetGlobal and Op::GetGlobal are – currently u8[2]
const GLOBALS_SIZE: usize = 1 << 16;

/// VM is responsible for executing bytecode. It will do the fetch/decode/execute loop for instructions.
pub struct VM {
    constants: Vec<Object>,
    pub globals: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
    sp: usize, // aka stack pointer. Always points to the next value. Top of stack is `stack[sp-1]`
}

pub fn new_globals() -> Vec<Object> {
    let mut globals = Vec::with_capacity(GLOBALS_SIZE);
    globals.resize(GLOBALS_SIZE, Object::Null);

    globals
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        let mut stack = Vec::with_capacity(STACK_SIZE);
        stack.resize(STACK_SIZE, Object::Null);

        VM {
            constants: bytecode.constants,
            globals: new_globals(),
            instructions: bytecode.instructions,
            stack,
            sp: 0,
        }
    }

    pub fn new_with_globals_store(bytecode: Bytecode, globals: Vec<Object>) -> Self {
        let mut vm = Self::new(bytecode);

        vm.globals = globals;

        vm
    }

    pub fn run(&mut self) -> Result<Object, VMError> {
        let mut ip = 0;

        while ip < self.instructions.len() {
            let op_code = self.instructions[ip];
            let op = Op::lookup_op(op_code);

            match op {
                Some(Op::Constant) => {
                    let const_index = self.read_u16(ip + 1);
                    ip += 2;

                    self.push(self.constants[const_index].clone())?;
                }
                Some(Op::Add) | Some(Op::Sub) | Some(Op::Mul) | Some(Op::Div) => {
                    self.execute_binary_operation(op.unwrap())?
                }
                Some(Op::True) => self.push(Object::Boolean(true))?,
                Some(Op::False) => self.push(Object::Boolean(false))?,
                Some(Op::Pop) => {
                    self.pop()?;
                }
                Some(Op::Equal) | Some(Op::NotEqual) | Some(Op::GreaterThan) => {
                    self.execute_comparison_operation(op.unwrap())?
                }
                Some(Op::Bang) => self.execute_bang_operator()?,
                Some(Op::Minus) => self.execute_minus_operator()?,
                Some(Op::Jump) => {
                    let pos = self.read_u16(ip + 1);
                    ip = pos - 1;
                }
                Some(Op::JumpNotTruthy) => {
                    let pos = self.read_u16(ip + 1);
                    ip += 2;

                    let condition = self.pop()?;

                    if !condition.is_truthy() {
                        ip = pos - 1;
                    }
                }
                Some(Op::Null) => self.push(Object::Null)?,
                Some(Op::SetGlobal) => {
                    let global_index = self.read_u16(ip + 1);
                    ip += 2;
                    self.globals[global_index] = self.pop()?;
                }
                Some(Op::GetGlobal) => {
                    let global_index = self.read_u16(ip + 1);
                    ip += 2;
                    self.push(self.globals[global_index].clone())?;
                }
                Some(Op::Array) => {
                    let num_elements = self.read_u16(ip + 1);
                    ip += 2;

                    let array = self.build_array(self.sp - num_elements, self.sp)?;
                    self.sp -= num_elements;

                    self.push(array)?;
                }
                Some(Op::Hash) => {
                    let num_elements = self.read_u16(ip + 1);
                    ip += 2;

                    let hash = self.build_hash(self.sp - num_elements, self.sp)?;
                    self.sp -= num_elements;

                    self.push(hash)?;
                }
                _ => todo!("Unhandled op code {}", op_code),
            }
            ip += 1;
        }

        Ok(self.last_popped_stack_elem())
    }

    fn build_array(&self, start: usize, end: usize) -> Result<Object, VMError> {
        let mut elements = Vec::with_capacity(end - start);
        elements.resize(elements.capacity(), Object::Null);

        elements[0..(end - start)].clone_from_slice(&self.stack[start..end]);

        Ok(Object::Array(elements))
    }

    fn build_hash(&self, start: usize, end: usize) -> Result<Object, VMError> {
        let mut hash = IndexMap::new();

        let mut i = start;

        while i < end {
            let key = self.stack[i].clone();
            let value = self.stack[i + 1].clone();

            let hash_key = HashKey::from_object(key).or_else(|e| Err(VMError::Eval(e)))?;
            hash.insert(hash_key, value);
            i += 2;
        }

        Ok(Object::Hash(hash))
    }

    fn execute_bang_operator(&mut self) -> Result<(), VMError> {
        let operand = self.pop()?;

        match operand {
            Object::Boolean(true) => self.push(Object::Boolean(false)),
            Object::Boolean(false) => self.push(Object::Boolean(true)),
            Object::Null => self.push(Object::Boolean(true)),
            _ => self.push(Object::Boolean(false)),
        }
    }

    fn execute_binary_operation(&mut self, op: Op) -> Result<(), VMError> {
        let right = self.pop()?;
        let left = self.pop()?;

        match (&left, &right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.execute_binary_integer_operation(l, r, op)
            }
            (Object::String(l), Object::String(r)) => {
                self.execute_binary_string_operation(l, r, op)
            }
            _ => Err(VMError::TypeMismatch(InfixOperator::Plus, left, right)),
        }
    }

    fn execute_binary_integer_operation(
        &mut self,
        l: &i64,
        r: &i64,
        op: Op,
    ) -> Result<(), VMError> {
        match op {
            Op::Add => self.push(Object::Integer(l + r)),
            Op::Sub => self.push(Object::Integer(l - r)),
            Op::Mul => self.push(Object::Integer(l * r)),
            Op::Div => self.push(Object::Integer(l / r)),
            _ => Err(VMError::UnknownOperation(op)),
        }
    }

    fn execute_binary_string_operation(&mut self, l: &str, r: &str, op: Op) -> Result<(), VMError> {
        match op {
            Op::Add => self.push(Object::String(format!("{}{}", l, r))),
            _ => Err(VMError::UnknownOperation(op)),
        }
    }

    fn execute_comparison_operation(&mut self, op: Op) -> Result<(), VMError> {
        let right = self.pop()?;
        let left = self.pop()?;

        match (&left, &right) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.execute_integer_comparison(op, left, right)
            }
            _ => match op {
                Op::Equal => self.push(Object::Boolean(left.eq(&right))),
                Op::NotEqual => self.push(Object::Boolean(!left.eq(&right))),
                _ => Err(VMError::UnknownOperation(op)),
            },
        }
    }

    fn execute_integer_comparison(
        &mut self,
        op: Op,
        left: &i64,
        right: &i64,
    ) -> Result<(), VMError> {
        match op {
            Op::Equal => self.push(Object::Boolean(left == right)),
            Op::NotEqual => self.push(Object::Boolean(left != right)),
            Op::GreaterThan => self.push(Object::Boolean(left > right)),
            _ => Err(VMError::UnknownOperation(op)),
        }
    }

    fn execute_minus_operator(&mut self) -> Result<(), VMError> {
        let operand = self.pop()?;

        match operand {
            Object::Integer(v) => self.push(Object::Integer(-v)),
            _ => Err(VMError::UnsupportedNegationType(operand)),
        }
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

    fn read_u16(&self, index: usize) -> usize {
        BigEndian::read_u16(&self.instructions[index..index + 2]) as usize
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{ast::Program, compiler::Compiler, lexer::Lexer, object::Object, parser::Parser};
    use indexmap::IndexMap;
    use std::error;

    #[test]
    fn integer_arithmetic() {
        let tests = vec![
            ("1", Object::Integer(1)),
            ("2", Object::Integer(2)),
            ("1 + 2", Object::Integer(3)),
            ("-5", Object::Integer(-5)),
            ("-10", Object::Integer(-10)),
            ("-50 + 100 + -50", Object::Integer(0)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn boolean_expressions() {
        let tests = vec![
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
            ("!true", Object::Boolean(false)),
            ("!false", Object::Boolean(true)),
            ("!5", Object::Boolean(false)),
            ("!!true", Object::Boolean(true)),
            ("!!false", Object::Boolean(false)),
            ("!!5", Object::Boolean(true)),
            ("!(if (false) { 5; })", Object::Boolean(true)),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn conditionals() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (true) { 10 } else { 20 }", Object::Integer(10)),
            ("if (false) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (false) { 10 }", Object::Null),
            (
                "if ((if (false) { 10 })) { 10 } else { 20 }",
                Object::Integer(20),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn global_let_statements() {
        let tests = vec![
            ("let one = 1; one;", Object::Integer(1)),
            ("let one = 1; let two = 2; one + two;", Object::Integer(3)),
            (
                "let one = 1; let two = one + one; one + two;",
                Object::Integer(3),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn string_expressions() {
        let tests = vec![
            (r#""monkey""#, Object::String("monkey".to_owned())),
            (r#""mon" + "key""#, Object::String("monkey".to_owned())),
            (
                r#""mon" + "key" + "banana""#,
                Object::String("monkeybanana".to_owned()),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn array_literals() {
        let tests = vec![
            ("[]", Object::Array(vec![])),
            (
                "[1, 2, 3]",
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                ]),
            ),
            (
                "[1 + 2, 3 * 4, 5 + 6]",
                Object::Array(vec![
                    Object::Integer(3),
                    Object::Integer(12),
                    Object::Integer(11),
                ]),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn hash_literals() {
        let tests = vec![
            ("{}", build_hash(vec![])),
            (
                "{1: 2, 2: 3}",
                build_hash(vec![
                    (Object::Integer(1), Object::Integer(2)),
                    ((Object::Integer(2), Object::Integer(3))),
                ]),
            ),
            (
                "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                build_hash(vec![
                    (Object::Integer(2), Object::Integer(4)),
                    ((Object::Integer(6), Object::Integer(16))),
                ]),
            ),
        ];

        run_vm_tests(tests);
    }

    fn build_hash(pairs: Vec<(Object, Object)>) -> Object {
        let mut hash = IndexMap::new();

        for (k, v) in pairs {
            match HashKey::from_object(k) {
                Ok(key) => {
                    hash.insert(key, v);
                }
                Err(err) => panic!("{}", err),
            }
        }

        Object::Hash(hash)
    }

    fn run_vm_tests(tests: Vec<(&str, Object)>) {
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
