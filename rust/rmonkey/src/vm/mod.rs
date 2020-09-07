pub mod frame;

use crate::ast::InfixOperator;
use crate::code::{self, Op};
use crate::compiler::Bytecode;
use crate::object::EvalError;
use crate::object::{builtins, HashKey, Object};
use crate::vm::frame::Frame;
use indexmap::IndexMap;
use std::{error, fmt};

#[derive(Debug, PartialEq)]
pub enum VMError {
    CallingNonFunction(Object),
    Eval(EvalError),
    IndexNotSupported(Object),
    NonFunction(Object),
    StackOverflow(),
    StackEmpty(),
    TypeMismatch(InfixOperator, Object, Object),
    UnknownOperation(Op),
    UnsupportedNegationType(Object),
    WrongArity(usize, usize),
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
            VMError::IndexNotSupported(index) => {
                write!(f, "Index not supported: {}", index.type_name())
            }
            VMError::CallingNonFunction(obj) => {
                write!(f, "Calling non-function: {}", obj.type_name())
            }
            VMError::WrongArity(want, got) => {
                write!(f, "wrong number of arguments: want={}, got={}", want, got)
            }
            VMError::NonFunction(obj) => write!(f, "not a function: {}", obj.type_name()),
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

/// MAX_FRAMES limits how many frames we can execute.
const MAX_FRAMES: usize = 1024;

/// VM is responsible for executing bytecode. It will do the fetch/decode/execute loop for instructions.
pub struct VM {
    constants: Vec<Object>,
    pub globals: Vec<Object>,

    stack: Vec<Object>,
    sp: usize, // aka stack pointer. Always points to the next value. Top of stack is `stack[sp-1]`

    frames: Vec<Frame>,
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

        let main_fn = Object::CompiledFunction(bytecode.instructions, 0, 0);
        let main_closure = Object::Closure(Box::new(main_fn), vec![]);
        let main_frame = Frame::new(main_closure, 0);

        let mut frames = Vec::with_capacity(MAX_FRAMES);
        frames.push(main_frame);

        VM {
            constants: bytecode.constants,
            globals: new_globals(),
            stack,
            sp: 0,
            frames,
        }
    }

    pub fn new_with_globals_store(bytecode: Bytecode, globals: Vec<Object>) -> Self {
        let mut vm = Self::new(bytecode);

        vm.globals = globals;

        vm
    }

    pub fn run(&mut self) -> Result<Object, VMError> {
        while self.current_frame().ip < self.current_frame().instructions().len() {
            let ip = self.current_frame().ip;
            let op_code = self.current_frame().instructions()[ip];
            let op = Op::lookup_op(op_code);

            match op {
                Some(Op::Constant) => {
                    let const_index = self.read_u16(ip + 1);
                    self.increment_ip(2);

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
                    self.set_ip(pos - 1);
                }
                Some(Op::JumpNotTruthy) => {
                    let pos = self.read_u16(ip + 1);
                    self.increment_ip(2);

                    let condition = self.pop()?;

                    if !condition.is_truthy() {
                        self.set_ip(pos - 1);
                    }
                }
                Some(Op::Null) => self.push(Object::Null)?,
                Some(Op::SetGlobal) => {
                    let global_index = self.read_u16(ip + 1);
                    self.increment_ip(2);
                    self.globals[global_index] = self.pop()?;
                }
                Some(Op::GetGlobal) => {
                    let global_index = self.read_u16(ip + 1);
                    self.increment_ip(2);
                    self.push(self.globals[global_index].clone())?;
                }
                Some(Op::Array) => {
                    let num_elements = self.read_u16(ip + 1);
                    self.increment_ip(2);

                    let array = self.build_array(self.sp - num_elements, self.sp)?;
                    self.sp -= num_elements;

                    self.push(array)?;
                }
                Some(Op::Hash) => {
                    let num_elements = self.read_u16(ip + 1);
                    self.increment_ip(2);

                    let hash = self.build_hash(self.sp - num_elements, self.sp)?;
                    self.sp -= num_elements;

                    self.push(hash)?;
                }
                Some(Op::Index) => {
                    let index = self.pop()?;
                    let left = self.pop()?;

                    self.execute_index_expression(left, index)?;
                }
                Some(Op::Call) => {
                    let num_args = self.read_u8(ip + 1);
                    self.increment_ip(1);

                    if self.call_function(num_args)? {
                        continue; // we pushed a new frame, so we don't want to increment the new current_frame ip
                    }
                }
                Some(Op::ReturnValue) => {
                    let return_value = self.pop()?;

                    // Pop the frame and update the stack pointer. The additional 1 means that we don't
                    // need to pop the object.CompiledFunction too – just move the stack pointer.
                    let frame = self.pop_frame();
                    self.sp = frame.base_pointer - 1;

                    self.push(return_value)?;
                }
                Some(Op::Return) => {
                    // Pop the frame and update the stack pointer. The additional 1 means that we don't
                    // need to pop the object.CompiledFunction too – just move the stack pointer.
                    let frame = self.pop_frame();
                    self.sp = frame.base_pointer - 1;

                    self.push(Object::Null)?;
                }
                Some(Op::SetLocal) => {
                    let local_index = self.read_u8(ip + 1);
                    self.increment_ip(1);

                    let base_pointer = self.current_frame().base_pointer;

                    let obj = self.pop()?;
                    self.stack[base_pointer + local_index] = obj;
                }
                Some(Op::GetLocal) => {
                    let local_index = self.read_u8(ip + 1);
                    self.increment_ip(1);

                    let base_pointer = self.current_frame().base_pointer;
                    self.push(self.stack[base_pointer + local_index].clone())?;
                }
                Some(Op::GetBuiltIn) => {
                    let builtin_index = self.read_u8(ip + 1);
                    self.increment_ip(1);

                    let builtin = &builtins::BUILTINS[builtin_index];

                    self.push(builtin.builtin.clone())?;
                }
                Some(Op::Closure) => {
                    let const_index = self.read_u16(ip + 1);
                    self.increment_ip(2);

                    let num_free = self.read_u8(ip + 3);
                    self.increment_ip(1);

                    self.push_closure(const_index, num_free)?;
                }
                Some(Op::GetFree) => {
                    let free_index = self.read_u8(ip + 1);
                    self.increment_ip(1);

                    match self.current_frame().closure.clone() {
                        Object::Closure(_, free) => self.push(free[free_index].clone())?,
                        _ => panic!("Should never get here! :D"),
                    }
                }
                _ => todo!("Unhandled op code {} – {:?}", op_code, op),
            }
            self.increment_ip(1);
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

    fn call_function(&mut self, num_args: usize) -> Result<bool, VMError> {
        // In all arms, we are cloning the object on the stack. So just clone it once and use it.
        let context_object = self.stack[self.sp - 1 - num_args].clone();

        match &context_object {
            Object::Closure(compiled_function, free) => {
                if let Object::CompiledFunction(instructions, num_locals, num_parameters) =
                    compiled_function.as_ref()
                {
                    if *num_parameters != num_args {
                        return Err(VMError::WrongArity(*num_parameters, num_args));
                    }
                    let frame = Frame::new(
                        Object::Closure(
                            Box::new(Object::CompiledFunction(
                                instructions.clone(),
                                *num_locals,
                                *num_parameters,
                            )),
                            free.clone(),
                        ),
                        self.sp - num_args,
                    );
                    self.push_frame(frame);
                    self.sp += num_locals;
                    return Ok(true);
                }
            }
            Object::BuiltIn(builtin) => {
                let args = self.stack[(self.sp - num_args)..self.sp].to_vec();

                match builtin(args) {
                    Ok(res) => {
                        self.sp -= num_args + 1;
                        self.push(res)?;
                        return Ok(false);
                    }
                    Err(eval_err) => {
                        return Err(VMError::Eval(eval_err));
                    }
                }
            }
            _ => {}
        }
        Err(VMError::CallingNonFunction(context_object))
    }

    fn current_frame(&self) -> &Frame {
        &self.frames.last().unwrap()
    }

    fn increment_ip(&mut self, diff: usize) {
        self.frames.last_mut().unwrap().ip += diff;
    }

    fn set_ip(&mut self, to: usize) {
        self.frames.last_mut().unwrap().ip = to;
    }

    fn execute_array_index(&mut self, elements: &[Object], index: i64) -> Result<(), VMError> {
        // bounds check
        let max = (elements.len() as i64) - 1;

        if index < 0 || index > max {
            self.push(Object::Null)
        } else {
            self.push(elements[index as usize].clone())
        }
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

    fn execute_index_expression(&mut self, left: Object, index: Object) -> Result<(), VMError> {
        match (&left, &index) {
            (Object::Array(elements), Object::Integer(i)) => {
                self.execute_array_index(&elements, *i)
            }
            (Object::Hash(map), key) => self.execute_hash_index(map, key),
            _ => Err(VMError::IndexNotSupported(left)),
        }
    }

    fn execute_hash_index(
        &mut self,
        map: &IndexMap<HashKey, Object>,
        key: &Object,
    ) -> Result<(), VMError> {
        let hash_key = HashKey::from_object(key.clone()).or_else(|e| Err(VMError::Eval(e)))?;
        match map.get(&hash_key) {
            Some(v) => self.push(v.clone()),
            None => self.push(Object::Null),
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

    fn pop_frame(&mut self) -> Frame {
        self.frames.pop().unwrap()
    }

    fn push_closure(&mut self, const_index: usize, num_free: usize) -> Result<(), VMError> {
        let constant = self.constants[const_index].clone();

        match constant {
            Object::CompiledFunction(_, _, _) => {
                let mut free = Vec::with_capacity(num_free);

                for i in 0..num_free {
                    free.push(self.stack[self.sp - num_free + i].clone());
                }

                self.sp -= num_free;

                let closure = Object::Closure(Box::new(constant), free);

                self.push(closure)
            }
            _ => Err(VMError::NonFunction(constant)),
        }
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames.push(frame);
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
        code::read_u16(&self.current_frame().instructions(), index)
    }

    fn read_u8(&self, index: usize) -> usize {
        code::read_u8(&self.current_frame().instructions(), index)
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

    #[test]
    fn index_expressions() {
        let tests = vec![
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][0 + 2]", Object::Integer(3)),
            ("[[1, 1, 1]][0][0]", Object::Integer(1)),
            ("[][0]", Object::Null),
            ("[1, 2, 3][99]", Object::Null),
            ("[1][-1]", Object::Null),
            ("{1: 1, 2: 2}[1]", Object::Integer(1)),
            ("{1: 1, 2: 2}[2]", Object::Integer(2)),
            ("{1: 1}[0]", Object::Null),
            ("{}[0]", Object::Null),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn calling_functions_without_arguments() {
        let tests = vec![
            (
                "let fivePlusTen = fn() { 5 + 10; };\
                 fivePlusTen();",
                Object::Integer(15),
            ),
            (
                "let one = fn() { 1; };\
                 let two = fn() { 2; };\
                 one() + two()",
                Object::Integer(3),
            ),
            (
                "let a = fn() { 1 };\
                 let b = fn() { a() + 1 };\
                 let c = fn() { b() + 1 };\
                 c()",
                Object::Integer(3),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn functions_with_return_statements() {
        let tests = vec![
            (
                "let earlyExit = fn() { return 99; 100; };\
                 earlyExit();",
                Object::Integer(99),
            ),
            (
                "let earlyExit = fn() { return 99; return 100; };\
                 earlyExit();",
                Object::Integer(99),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn functions_without_return_value() {
        let tests = vec![
            (
                "let noReturn = fn() {  };\
                 noReturn();",
                Object::Null,
            ),
            (
                "let noReturn = fn() {  };\
                 let noReturnTwo = fn() { noReturn(); };
                 noReturn();
                 noReturnTwo();",
                Object::Null,
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn first_class_functions() {
        let tests = vec![
            (
                "let returnsOne = fn() { 1; };\
             let returnsOneReturner = fn() { returnsOne; };\
             returnsOneReturner()();",
                Object::Integer(1),
            ),
            (
                "let returnsOneReturner = fn() {\
                    let returnsOne = fn() { 1; };\
                    returnsOne;\
                };\
                returnsOneReturner()();",
                Object::Integer(1),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn calling_functions_with_bindings() {
        let tests = vec![
            (
                "let one = fn() {\
                    let one = 1;\
                    one\
                };\
                one();",
                Object::Integer(1),
            ),
            (
                "let oneAndTwo = fn() {
                    let one = 1;
                    let two = 2;
                    one + two;
                };
                oneAndTwo();",
                Object::Integer(3),
            ),
            (
                "let oneAndTwo = fn() {
                    let one = 1;
                    let two = 2;
                    one + two;
                };
                let threeAndFour = fn() {
                    let three = 3;
                    let four = 4;
                    three + four;
                };
                oneAndTwo() + threeAndFour();",
                Object::Integer(10),
            ),
            (
                "let firstFoobar = fn() {
                    let foobar = 50;
                    foobar;
                };
                let secondFoobar = fn() {
                    let foobar = 100;
                    foobar;
                };
                firstFoobar() + secondFoobar();",
                Object::Integer(150),
            ),
            (
                "let globalSeed = 50;
                let minusOne = fn() {
                    let num = 1;
                    globalSeed - num;
                }
                let minusTwo = fn() {
                    let num = 2;
                    globalSeed - num;
                }
                minusOne() + minusTwo();",
                Object::Integer(97),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn calling_functions_with_arguments_and_bindings() {
        let tests = vec![
            (
                "let identity = fn(a) { a; };\
                 identity(4);",
                Object::Integer(4),
            ),
            (
                "let sum = fn(a, b) { a + b; };\
                 sum(1, 2)",
                Object::Integer(3),
            ),
            (
                "let sum = fn(a, b) {\
                    let c = a + b;\
                    c;\
                 };\
                 sum(1, 2);",
                Object::Integer(3),
            ),
            (
                "let sum = fn(a, b) {\
                    let c = a + b;\
                    c;\
                 };\
                 let outer = fn() {\
                    sum(1, 2) + sum(3, 4);\
                 };\
                 outer();",
                Object::Integer(10),
            ),
            (
                "let globalNum = 10;\
                 \
                 let sum = fn(a, b) {\
                    let c = a + b;\
                    c + globalNum;\
                 };\
                 \
                 let outer = fn() {\
                    sum(1, 2) + sum(3, 4) + globalNum;\
                 };\
                 \
                 outer() + globalNum;",
                Object::Integer(50),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn calling_functions_with_wrong_arguments() {
        let tests = vec![
            ("fn() { 1; }(1);", VMError::WrongArity(0, 1)),
            ("fn(a) { a; }();", VMError::WrongArity(1, 0)),
            ("fn(a, b) { a + b; }(1);", VMError::WrongArity(2, 1)),
        ];

        run_vm_tests_with_errors(tests);
    }

    #[test]
    // #[ignore]
    fn builtin_functions() {
        let good_program_tests = vec![
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("four")"#, Object::Integer(4)),
            (r#"len("hello world")"#, Object::Integer(11)),
            (r#"len([1, 2, 3])"#, Object::Integer(3)),
            (r#"len([])"#, Object::Integer(0)),
            (r#"puts("hello world")"#, Object::Null),
            (r#"first([1, 2, 3])"#, Object::Integer(1)),
            (r#"first([])"#, Object::Null),
            (r#"last([1, 2, 3])"#, Object::Integer(3)),
            (r#"last([])"#, Object::Null),
            (
                r#"rest([1, 2, 3])"#,
                Object::Array(vec![Object::Integer(2), Object::Integer(3)]),
            ),
            (r#"rest([])"#, Object::Null),
            (r#"push([], 1)"#, Object::Array(vec![Object::Integer(1)])),
            (r#"first([])"#, Object::Null),
        ];

        run_vm_tests(good_program_tests);

        let bad_program_tests = vec![
            (
                r#"len(1)"#,
                VMError::Eval(EvalError::UnsupportedArguments(
                    "len".to_owned(),
                    vec![Object::Integer(1)],
                )),
            ),
            (
                r#"len("one", "two")"#,
                VMError::Eval(EvalError::WrongArgumentCount {
                    expected: 1,
                    given: 2,
                }),
            ),
            (
                r#"first(1)"#,
                VMError::Eval(EvalError::UnsupportedArguments(
                    "first".to_owned(),
                    vec![Object::Integer(1)],
                )),
            ),
            (
                r#"last(1)"#,
                VMError::Eval(EvalError::UnsupportedArguments(
                    "last".to_owned(),
                    vec![Object::Integer(1)],
                )),
            ),
            (
                r#"push(1, 1)"#,
                VMError::Eval(EvalError::UnsupportedArguments(
                    "push".to_owned(),
                    vec![Object::Integer(1), Object::Integer(1)],
                )),
            ),
        ];

        run_vm_tests_with_errors(bad_program_tests);
    }

    #[test]
    fn closures() {
        let tests = vec![
            (
                "let newClosure = fn(a) {
                    fn() { a; };
                };
                let closure = newClosure(99);
                closure();",
                Object::Integer(99),
            ),
            (
                "let newAdder = fn(a, b) {
                    fn(c) { a + b + c };
                };
                let adder = newAdder(1, 2);
                adder(8);",
                Object::Integer(11),
            ),
            (
                "let newAdder = fn(a, b) {
                    let c = a + b;
                    fn(d) { c + d };
                };
                let adder = newAdder(1, 2);
                adder(8);",
                Object::Integer(11),
            ),
            (
                "let newAdderOuter = fn(a, b) {
                    let c = a + b;
                    fn(d) {
                        let e = d + c;
                        fn(f) { e + f; };
                    };
                };
                let newAdderInner = newAdderOuter(1, 2)
                let adder = newAdderInner(3);
                adder(8);",
                Object::Integer(14),
            ),
            (
                "let a = 1;
                let newAdderOuter = fn(b) {
                    fn(c) {
                        fn(d) { a + b + c + d };
                    };
                };
                let newAdderInner = newAdderOuter(2)
                let adder = newAdderInner(3);
                adder(8);",
                Object::Integer(14),
            ),
            (
                "let newClosure = fn(a, b) {
                    let one = fn() { a; };
                    let two = fn() { b; };
                    fn() { one() + two(); };
                };
                let closure = newClosure(9, 90);
                closure();",
                Object::Integer(99),
            ),
        ];

        run_vm_tests(tests);
    }

    fn run_vm_tests_with_errors(tests: Vec<(&str, VMError)>) {
        for (input, expected) in tests {
            let program = parse(input);
            let mut compiler = Compiler::new();
            match compiler.compile(&program) {
                Ok(bytecode) => {
                    let mut vm = VM::new(bytecode);

                    match vm.run() {
                        Ok(_) => assert!(false, "expected failure"),
                        Err(actual) => assert_eq!(expected, actual),
                    }
                }
                Err(e) => assert!(false, "compiler error: {}", e),
            }
        }
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
