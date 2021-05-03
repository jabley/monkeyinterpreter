pub mod frame;

use crate::ast::InfixOperator;
use crate::code::{self, Instructions, Op};
use crate::object::{Closure, CompiledFunction, EvalError};
use crate::object::{HashKey, Object};
use crate::vm::frame::Frame;
use indexmap::IndexMap;
use std::rc::Rc;
use std::{error, fmt};

#[derive(Debug, PartialEq)]
pub enum VMError {
    CallingNonFunction(Rc<Object>),
    Eval(EvalError),
    IndexNotSupported(Rc<Object>),
    NonFunction(Rc<Object>),
    StackOverflow(),
    StackEmpty(),
    TypeMismatch(InfixOperator, Rc<Object>, Rc<Object>),
    UnknownOperation(Op),
    UnsupportedNegationType(Rc<Object>),
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
pub struct VM<'a> {
    constants: &'a [Rc<Object>],
    pub globals: Vec<Rc<Object>>,

    stack: Vec<Rc<Object>>,
    sp: usize, // aka stack pointer. Always points to the next value. Top of stack is `stack[sp-1]`

    frames: Vec<Frame>,
}

pub fn new_globals() -> Vec<Rc<Object>> {
    let mut globals = Vec::with_capacity(GLOBALS_SIZE);
    globals.resize(GLOBALS_SIZE, Rc::new(Object::Null));

    globals
}

impl<'a> VM<'a> {
    pub fn new(constants: &'a [Rc<Object>], instructions: Instructions) -> Self {
        let mut stack = Vec::with_capacity(STACK_SIZE);
        stack.resize(STACK_SIZE, Rc::new(Object::Null));

        let main_func = Rc::new(CompiledFunction {
            instructions,
            num_locals: 0,
            num_parameters: 0,
        });
        let main_closure = Rc::new(Closure {
            func: main_func,
            free: vec![],
        });
        let main_frame = Frame {
            cl: main_closure,
            ip: 0,
            base_pointer: 0,
        };

        let mut frames = Vec::with_capacity(MAX_FRAMES);
        frames.push(main_frame);

        VM {
            constants,
            globals: new_globals(),
            stack,
            sp: 0,
            frames,
        }
    }

    pub fn new_with_globals_store(
        constants: &'a [Rc<Object>],
        instructions: Instructions,
        globals: Vec<Rc<Object>>,
    ) -> Self {
        let mut vm = Self::new(constants, instructions);

        vm.globals = globals;

        vm
    }

    pub fn run(&mut self) -> Result<Rc<Object>, VMError> {
        while self.execute_current_frame() {
            let ip = self.current_frame().ip;
            let op_code = self.current_op_code();
            let op = Op::lookup_op(op_code);

            match op {
                Some(Op::Constant) => self.load_constant(ip)?,
                Some(Op::Add) | Some(Op::Sub) | Some(Op::Mul) | Some(Op::Div) => {
                    self.execute_binary_operation(op.unwrap())?
                }
                Some(Op::True) => self.push(Rc::new(Object::Boolean(true)))?,
                Some(Op::False) => self.push(Rc::new(Object::Boolean(false)))?,
                Some(Op::Pop) => {
                    self.pop()?;
                }
                Some(Op::Equal) | Some(Op::NotEqual) | Some(Op::GreaterThan) => {
                    self.execute_comparison_operation(op.unwrap())?
                }
                Some(Op::Bang) => self.execute_bang_operator()?,
                Some(Op::Minus) => self.execute_minus_operator()?,
                Some(Op::Jump) => self.unconditional_jump(ip),
                Some(Op::JumpNotTruthy) => self.conditional_jump(ip)?,
                Some(Op::Null) => self.push(Rc::new(Object::Null))?,
                Some(Op::SetGlobal) => self.set_global(ip)?,
                Some(Op::GetGlobal) => self.get_global(ip)?,
                Some(Op::Array) => self.load_array(ip)?,
                Some(Op::Hash) => self.load_hash(ip)?,
                Some(Op::Index) => self.index()?,
                Some(Op::Call) => {
                    let num_args = self.read_u8(ip + 1);
                    self.increment_ip(1);

                    if self.call_function(num_args)? {
                        continue; // we pushed a new frame, so we don't want to increment the new current_frame ip
                    }
                }
                Some(Op::ReturnValue) => self.return_value(true)?,
                Some(Op::Return) => self.return_value(false)?,
                Some(Op::SetLocal) => self.set_local(ip)?,
                Some(Op::GetLocal) => self.get_local(ip)?,
                Some(Op::GetBuiltIn) => self.load_builtin(ip)?,
                Some(Op::Closure) => self.load_closure(ip)?,
                Some(Op::GetFree) => self.load_free(ip)?,
                _ => todo!("Unhandled op code {} – {:?}", op_code, op),
            }
            self.increment_ip(1);
        }

        Ok(self.last_popped_stack_elem())
    }

    fn build_array(&self, start: usize, end: usize) -> Result<Object, VMError> {
        let mut elements = Vec::with_capacity(end - start);
        elements.resize(elements.capacity(), Rc::new(Object::Null));

        elements[0..(end - start)].clone_from_slice(&self.stack[start..end]);

        Ok(Object::Array(Rc::new(elements)))
    }

    fn build_hash(&self, start: usize, end: usize) -> Result<Object, VMError> {
        let mut hash = IndexMap::new();

        let mut i = start;

        while i < end {
            let key = self.stack[i].clone();
            let value = self.stack[i + 1].clone();

            let hash_key = HashKey::from_object(&key).or_else(|e| Err(VMError::Eval(e)))?;
            hash.insert(hash_key, value);
            i += 2;
        }

        Ok(Object::Hash(Rc::new(hash)))
    }

    fn call_function(&mut self, num_args: usize) -> Result<bool, VMError> {
        // In all arms, we are cloning the object on the stack. So just clone it once and use it.
        let context_object = self.stack[self.sp - 1 - num_args].clone();

        match &*context_object {
            Object::Closure(cl) => {
                if cl.func.num_parameters != num_args {
                    Err(VMError::WrongArity(cl.func.num_parameters, num_args))
                } else {
                    let frame = Frame {
                        cl: cl.clone(),
                        ip: 0,
                        base_pointer: self.sp - num_args,
                    };
                    self.push_frame(frame);
                    self.sp += cl.func.num_locals;
                    Ok(true)
                }
            }
            Object::BuiltIn(builtin) => {
                let args = self.stack[(self.sp - num_args)..self.sp].to_vec();

                match builtin.apply(&args) {
                    Ok(res) => {
                        self.sp -= num_args + 1;
                        self.push(res)?;
                        Ok(false)
                    }
                    Err(eval_err) => Err(VMError::Eval(eval_err)),
                }
            }
            _ => Err(VMError::CallingNonFunction(context_object)),
        }
    }

    fn load_constant(&mut self, ip: usize) -> Result<(), VMError> {
        let const_index = self.read_u16(ip + 1);
        self.increment_ip(2);

        self.push(self.constants[const_index].clone())
    }

    fn unconditional_jump(&mut self, ip: usize) {
        let pos = self.read_u16(ip + 1);
        self.set_ip(pos - 1);
    }

    fn conditional_jump(&mut self, ip: usize) -> Result<(), VMError> {
        let pos = self.read_u16(ip + 1);
        self.increment_ip(2);

        let condition = self.pop()?;

        if !condition.is_truthy() {
            self.set_ip(pos - 1);
        }

        Ok(())
    }

    fn set_global(&mut self, ip: usize) -> Result<(), VMError> {
        let global_index = self.read_u16(ip + 1);
        self.increment_ip(2);
        self.globals[global_index] = self.pop()?;
        Ok(())
    }

    fn get_global(&mut self, ip: usize) -> Result<(), VMError> {
        let global_index = self.read_u16(ip + 1);
        self.increment_ip(2);
        self.push(self.globals[global_index].clone())
    }

    fn load_array(&mut self, ip: usize) -> Result<(), VMError> {
        let num_elements = self.read_u16(ip + 1);
        self.increment_ip(2);

        let array = self.build_array(self.sp - num_elements, self.sp)?;
        self.sp -= num_elements;

        self.push(Rc::new(array))
    }

    fn load_hash(&mut self, ip: usize) -> Result<(), VMError> {
        let num_elements = self.read_u16(ip + 1);
        self.increment_ip(2);

        let hash = self.build_hash(self.sp - num_elements, self.sp)?;
        self.sp -= num_elements;

        self.push(Rc::new(hash))
    }

    fn index(&mut self) -> Result<(), VMError> {
        let index = self.pop()?;
        let left = self.pop()?;

        self.execute_index_expression(left, index)
    }

    fn return_value(&mut self, has_return_value: bool) -> Result<(), VMError> {
        let return_value = if has_return_value {
            self.pop()?
        } else {
            Rc::new(Object::Null)
        };

        // Pop the frame and update the stack pointer. The additional 1 means that we don't
        // need to pop the object.CompiledFunction too – just move the stack pointer.
        let frame = self.pop_frame();
        self.sp = frame.base_pointer - 1;

        self.push(return_value)
    }

    fn set_local(&mut self, ip: usize) -> Result<(), VMError> {
        let local_index = self.read_u8(ip + 1);
        self.increment_ip(1);

        let base_pointer = self.current_frame().base_pointer;

        let obj = self.pop()?;
        self.stack[base_pointer + local_index] = obj;

        Ok(())
    }

    fn get_local(&mut self, ip: usize) -> Result<(), VMError> {
        let local_index = self.read_u8(ip + 1);
        self.increment_ip(1);

        let base_pointer = self.current_frame().base_pointer;
        self.push(self.stack[base_pointer + local_index].clone())
    }

    fn load_builtin(&mut self, ip: usize) -> Result<(), VMError> {
        let builtin_index = self.read_u8(ip + 1) as u8;
        self.increment_ip(1);

        let builtin = unsafe { ::std::mem::transmute(builtin_index) };

        self.push(Rc::new(Object::BuiltIn(builtin)))
    }

    fn load_closure(&mut self, ip: usize) -> Result<(), VMError> {
        let const_index = self.read_u16(ip + 1);
        self.increment_ip(2);

        let num_free = self.read_u8(ip + 3);
        self.increment_ip(1);

        self.push_closure(const_index, num_free)
    }

    fn load_free(&mut self, ip: usize) -> Result<(), VMError> {
        let free_index = self.read_u8(ip + 1);
        self.increment_ip(1);

        self.push(self.current_frame().cl.free[free_index].clone())
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

    fn execute_array_index(
        &mut self,
        elements: &Rc<Vec<Rc<Object>>>,
        index: i64,
    ) -> Result<(), VMError> {
        // bounds check
        let max = (elements.len() as i64) - 1;

        if index < 0 || index > max {
            self.push(Rc::new(Object::Null))
        } else {
            self.push(elements[index as usize].clone())
        }
    }

    fn execute_bang_operator(&mut self) -> Result<(), VMError> {
        let operand = self.pop()?;

        self.push(Rc::new(Object::Boolean(!operand.is_truthy())))
    }

    fn execute_binary_operation(&mut self, op: Op) -> Result<(), VMError> {
        let right = self.pop()?;
        let left = self.pop()?;

        match (&*left, &*right) {
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
            Op::Add => self.push(Rc::new(Object::Integer(l + r))),
            Op::Sub => self.push(Rc::new(Object::Integer(l - r))),
            Op::Mul => self.push(Rc::new(Object::Integer(l * r))),
            Op::Div => self.push(Rc::new(Object::Integer(l / r))),
            _ => Err(VMError::UnknownOperation(op)),
        }
    }

    fn execute_binary_string_operation(&mut self, l: &str, r: &str, op: Op) -> Result<(), VMError> {
        match op {
            Op::Add => self.push(Rc::new(Object::String(format!("{}{}", l, r)))),
            _ => Err(VMError::UnknownOperation(op)),
        }
    }

    fn execute_comparison_operation(&mut self, op: Op) -> Result<(), VMError> {
        let right = self.pop()?;
        let left = self.pop()?;

        match (&*left, &*right) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.execute_integer_comparison(op, left, right)
            }
            _ => match op {
                Op::Equal => self.push(Rc::new(Object::Boolean(left.eq(&right)))),
                Op::NotEqual => self.push(Rc::new(Object::Boolean(!left.eq(&right)))),
                _ => Err(VMError::UnknownOperation(op)),
            },
        }
    }

    fn execute_index_expression(
        &mut self,
        left: Rc<Object>,
        index: Rc<Object>,
    ) -> Result<(), VMError> {
        match (&*left, &*index) {
            (Object::Array(elements), Object::Integer(i)) => {
                self.execute_array_index(&elements, *i)
            }
            (Object::Hash(map), key) => self.execute_hash_index(map, key),
            _ => Err(VMError::IndexNotSupported(left)),
        }
    }

    fn execute_hash_index(
        &mut self,
        map: &Rc<IndexMap<HashKey, Rc<Object>>>,
        key: &Object,
    ) -> Result<(), VMError> {
        let hash_key = HashKey::from_object(key).or_else(|e| Err(VMError::Eval(e)))?;
        match map.get(&hash_key) {
            Some(v) => self.push(v.clone()),
            None => self.push(Rc::new(Object::Null)),
        }
    }

    fn execute_integer_comparison(
        &mut self,
        op: Op,
        left: &i64,
        right: &i64,
    ) -> Result<(), VMError> {
        match op {
            Op::Equal => self.push(Rc::new(Object::Boolean(left == right))),
            Op::NotEqual => self.push(Rc::new(Object::Boolean(left != right))),
            Op::GreaterThan => self.push(Rc::new(Object::Boolean(left > right))),
            _ => Err(VMError::UnknownOperation(op)),
        }
    }

    fn execute_minus_operator(&mut self) -> Result<(), VMError> {
        let operand = self.pop()?;

        match &*operand {
            Object::Integer(v) => self.push(Rc::new(Object::Integer(-*v))),
            _ => Err(VMError::UnsupportedNegationType(operand)),
        }
    }

    fn last_popped_stack_elem(&self) -> Rc<Object> {
        self.stack[self.sp].clone()
    }

    fn push(&mut self, obj: Rc<Object>) -> Result<(), VMError> {
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

        match &*constant {
            Object::CompiledFunction(func) => {
                let mut free = Vec::with_capacity(num_free);

                for i in 0..num_free {
                    free.push(self.stack[self.sp - num_free + i].clone());
                }

                self.sp -= num_free;

                let closure = Object::Closure(Rc::new(Closure {
                    func: func.clone(),
                    free,
                }));

                self.push(Rc::new(closure))
            }
            _ => Err(VMError::NonFunction(constant)),
        }
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames.push(frame);
    }

    fn pop(&mut self) -> Result<Rc<Object>, VMError> {
        if self.sp == 0 {
            Err(VMError::StackEmpty())
        } else {
            self.sp -= 1;
            Ok(self.stack[self.sp].clone())
        }
    }

    fn read_u16(&self, index: usize) -> usize {
        code::read_u16(&self.current_frame().get_instructions(), index)
    }

    fn read_u8(&self, index: usize) -> usize {
        code::read_u8(&self.current_frame().get_instructions(), index)
    }

    fn execute_current_frame(&self) -> bool {
        let frame = &self.current_frame();
        frame.ip < frame.get_instructions().len()
    }

    fn current_op_code(&self) -> u8 {
        let frame = &self.current_frame();
        frame.get_instructions()[frame.ip]
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
            ("[]", Object::Array(Rc::new(vec![]))),
            (
                "[1, 2, 3]",
                Object::Array(Rc::new(vec![
                    Rc::new(Object::Integer(1)),
                    Rc::new(Object::Integer(2)),
                    Rc::new(Object::Integer(3)),
                ])),
            ),
            (
                "[1 + 2, 3 * 4, 5 + 6]",
                Object::Array(Rc::new(vec![
                    Rc::new(Object::Integer(3)),
                    Rc::new(Object::Integer(12)),
                    Rc::new(Object::Integer(11)),
                ])),
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
            match HashKey::from_object(&k) {
                Ok(key) => {
                    hash.insert(key, Rc::new(v));
                }
                Err(err) => panic!("{}", err),
            }
        }

        Object::Hash(Rc::new(hash))
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
                Object::Array(Rc::new(vec![
                    Rc::new(Object::Integer(2)),
                    Rc::new(Object::Integer(3)),
                ])),
            ),
            (r#"rest([])"#, Object::Null),
            (
                r#"push([], 1)"#,
                Object::Array(Rc::new(vec![Rc::new(Object::Integer(1))])),
            ),
            (r#"first([])"#, Object::Null),
        ];

        run_vm_tests(good_program_tests);

        let bad_program_tests = vec![
            (
                r#"len(1)"#,
                VMError::Eval(EvalError::UnsupportedArguments(
                    "len".to_owned(),
                    vec![Rc::new(Object::Integer(1))],
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
                    vec![Rc::new(Object::Integer(1))],
                )),
            ),
            (
                r#"last(1)"#,
                VMError::Eval(EvalError::UnsupportedArguments(
                    "last".to_owned(),
                    vec![Rc::new(Object::Integer(1))],
                )),
            ),
            (
                r#"push(1, 1)"#,
                VMError::Eval(EvalError::UnsupportedArguments(
                    "push".to_owned(),
                    vec![Rc::new(Object::Integer(1)), Rc::new(Object::Integer(1))],
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

    #[test]
    fn recursive_fibonacci() {
        let tests = vec![(
            "let fibonacci = fn(x) {
                if (x == 0) {
                    return 0;
                } else {
                    if (x == 1) {
                        return 1;
                    } else {
                        fibonacci(x - 1) + fibonacci(x - 2);
                    }
                }
            };
            fibonacci(15);",
            Object::Integer(610),
        )];

        run_vm_tests(tests);
    }

    fn run_vm_tests_with_errors(tests: Vec<(&str, VMError)>) {
        for (input, expected) in tests {
            let program = parse(input);
            let mut compiler = Compiler::new();
            match compiler.compile(&program) {
                Ok(bytecode) => {
                    let mut vm = VM::new(bytecode.constants, bytecode.instructions.to_vec());

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
        let mut vm = VM::new(bytecode.constants, bytecode.instructions.to_vec());
        let result = vm.run()?;

        assert_eq!(
            &expected, &*result,
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
