pub mod symbol_table;

use crate::ast::Expression;
use crate::ast::Statement;
use crate::object::CompiledFunction;
use crate::{
    ast::{BlockStatement, InfixOperator, PrefixOperator, Program},
    code::{make_instruction, Instructions, Op},
    object::Object,
};
use std::rc::Rc;
use std::{error::Error, fmt};
pub use symbol_table::SymbolTable;
use symbol_table::{Symbol, SymbolScope};

#[derive(Clone)]
struct EmittedInstruction {
    op: Op,
    position: usize,
}

#[derive(Debug)]
pub enum CompilerError {
    UnknownVariable(String),
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompilerError::UnknownVariable(ident) => write!(f, "Unknown variable {}", ident),
        }
    }
}

impl Error for CompilerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

/// CompilationScope helps keep track of scope when compiling functions
#[derive(Default)]
struct CompilationScope {
    instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl CompilationScope {
    fn new() -> Self {
        Default::default()
    }
}

/// Compiler is responsible for taking an AST and turning it into bytecode.
#[derive(Default)]
pub struct Compiler {
    pub constants: Vec<Rc<Object>>,
    pub symbol_table: SymbolTable,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let mut res = Compiler {
            symbol_table: SymbolTable::new_with_builtins(),
            ..Default::default()
        };

        let main_scope = CompilationScope::new();

        res.scopes.push(main_scope);

        res
    }

    /// new_with_state returns a Compiler that can accept a SymbolTable and constants of a previous
    /// compilation. This is needed by the REPL.
    pub fn new_with_state(symbol_table: SymbolTable, constants: Vec<Rc<Object>>) -> Self {
        let mut compiler = Self::new();

        compiler.symbol_table = symbol_table;
        compiler.constants = constants;

        compiler
    }

    pub fn compile(&mut self, program: &Program) -> Result<Bytecode, CompilerError> {
        for stmt in &program.statements {
            self.compile_statement(stmt)?;
        }

        Ok(self.bytecode())
    }

    fn bytecode(&mut self) -> Bytecode {
        Bytecode {
            instructions: &self.scopes[self.scope_index].instructions,
            constants: &self.constants,
        }
    }

    fn compile_statement(&mut self, stmt: &Statement) -> Result<(), CompilerError> {
        match stmt {
            Statement::Expression(exp) => {
                self.compile_expression(exp)?;
                self.emit(Op::Pop, &[]);
            }
            Statement::Let(ident, exp) => {
                let symbol = self.symbol_table.define(ident);
                self.compile_expression(exp)?;
                match symbol.scope {
                    SymbolScope::Global => self.emit(Op::SetGlobal, &[symbol.index]),
                    _ => self.emit(Op::SetLocal, &[symbol.index]),
                };
            }
            Statement::Return(Some(exp)) => {
                self.compile_expression(exp)?;
                self.emit(Op::ReturnValue, &[]);
            }
            Statement::Return(None) => {
                self.emit(Op::Return, &[]);
            }
        }
        Ok(())
    }

    fn compile_block_statement(&mut self, body: &BlockStatement) -> Result<(), CompilerError> {
        for stmt in &body.statements {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }

    fn compile_expression(&mut self, exp: &Expression) -> Result<(), CompilerError> {
        match exp {
            Expression::Identifier(ident) => {
                if let Some(symbol) = self.symbol_table.resolve(ident) {
                    self.load_symbol(&symbol);
                } else {
                    return Err(CompilerError::UnknownVariable(ident.to_string()));
                }
            }
            Expression::If(condition, consequence, alternative) => {
                self.compile_expression(condition)?;

                // emit an Op::JumpNotTruthy with a hard-coded nonsense value for now.
                let jump_not_truthy_position = self.emit(Op::JumpNotTruthy, &[9999]); // this is the right structure, but a nonsense value

                self.compile_block_statement(consequence)?;

                if self.last_instruction_is(Op::Pop) {
                    self.remove_last_pop();
                }

                // emit an Op::Jump with a hard-coded nonsense value for now
                let jump_position = self.emit(Op::Jump, &[9999]);

                let after_consequence_position = self.current_instructions().len();

                self.change_operand(jump_not_truthy_position, after_consequence_position);

                match alternative {
                    Some(body) => {
                        self.compile_block_statement(body)?;

                        if self.last_instruction_is(Op::Pop) {
                            self.remove_last_pop();
                        }
                    }
                    None => {
                        self.emit(Op::Null, &[]);
                    }
                }

                let after_alternative_position = self.current_instructions().len();
                self.change_operand(jump_position, after_alternative_position);
            }
            Expression::Infix(operator, left, right) => {
                if *operator == InfixOperator::Lt {
                    // Treat less-than as a special case.
                    // We re-order the code and treat it as a greater-than expression.
                    self.compile_expression(right)?;
                    self.compile_expression(left)?;
                    self.emit(Op::GreaterThan, &[]);

                    return Ok(());
                }

                self.compile_expression(left)?;
                self.compile_expression(right)?;

                match operator {
                    InfixOperator::Plus => self.emit(Op::Add, &[]),
                    InfixOperator::Minus => self.emit(Op::Sub, &[]),
                    InfixOperator::Asterisk => self.emit(Op::Mul, &[]),
                    InfixOperator::Slash => self.emit(Op::Div, &[]),
                    InfixOperator::Gt => self.emit(Op::GreaterThan, &[]),
                    InfixOperator::Eq => self.emit(Op::Equal, &[]),
                    InfixOperator::NotEq => self.emit(Op::NotEqual, &[]),
                    _ => todo!("Unknown operator: {}", operator),
                };
            }
            Expression::Prefix(operator, expr) => {
                self.compile_expression(expr)?;

                match operator {
                    PrefixOperator::Bang => self.emit(Op::Bang, &[]),
                    PrefixOperator::Minus => self.emit(Op::Minus, &[]),
                };
            }
            Expression::IntegerLiteral(v) => {
                let constant = self.add_constant(Object::Integer(*v));
                let operands = vec![constant];
                self.emit(Op::Constant, &operands);
            }
            Expression::Boolean(v) => {
                if *v {
                    self.emit(Op::True, &[]);
                } else {
                    self.emit(Op::False, &[]);
                }
            }
            Expression::StringLiteral(s) => {
                let constant = self.add_constant(Object::String(s.to_string()));
                let operands = vec![constant];
                self.emit(Op::Constant, &operands);
            }
            Expression::ArrayLiteral(elements) => {
                for exp in elements {
                    self.compile_expression(exp)?;
                }
                self.emit(Op::Array, &[elements.len()]);
            }
            Expression::HashLiteral(hash) => {
                for (k, v) in &hash.pairs {
                    self.compile_expression(k)?;
                    self.compile_expression(v)?;
                }

                self.emit(Op::Hash, &[2 * hash.pairs.len()]);
            }
            Expression::IndexExpression(left, index) => {
                self.compile_expression(left)?;
                self.compile_expression(index)?;

                self.emit(Op::Index, &[]);
            }
            Expression::FunctionLiteral(parameters, body) => {
                self.enter_scope();

                for parameter in parameters {
                    self.symbol_table.define(parameter);
                }

                self.compile_block_statement(body)?;

                if self.last_instruction_is(Op::Pop) {
                    self.replace_last_pop_with_return();
                }

                if !self.last_instruction_is(Op::ReturnValue) {
                    self.emit(Op::Return, &[]);
                }

                let num_locals = self.symbol_table.num_definitions();

                let free_symbols: Vec<Symbol> = self.symbol_table.free_symbols.clone();

                let instructions = self.leave_scope();

                for s in &free_symbols {
                    self.load_symbol(s);
                }

                let compiled_function = Object::CompiledFunction(Rc::new(CompiledFunction {
                    instructions,
                    num_locals,
                    num_parameters: parameters.len(),
                }));
                let constant_index = self.add_constant(compiled_function);
                self.emit(Op::Closure, &[constant_index, free_symbols.len()]);
            }
            Expression::Call(function, parameters) => {
                self.compile_expression(function)?;

                for arg in parameters {
                    self.compile_expression(arg)?;
                }

                self.emit(Op::Call, &[parameters.len()]);
            }
        }

        Ok(())
    }

    pub fn current_instructions(&self) -> &Instructions {
        &self.current_scope().instructions
    }

    fn current_scope(&self) -> &CompilationScope {
        &self.scopes[self.scope_index]
    }

    fn emit(&mut self, op: Op, operands: &[usize]) -> usize {
        let instruction = make_instruction(op, operands);
        let pos = self.add_instruction(&instruction);

        self.set_last_instruction(op, pos);

        pos
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope::new();

        self.scopes.push(scope);
        self.scope_index += 1;
        let symbol_table = &self.symbol_table;
        self.symbol_table = SymbolTable::new_enclosed(symbol_table);
    }

    fn add_instruction(&mut self, instruction: &[u8]) -> usize {
        let pos_new_instruction = self.current_instructions().len();
        self.scopes[self.scope_index]
            .instructions
            .extend_from_slice(instruction);

        pos_new_instruction
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(Rc::new(obj));
        self.constants.len() - 1
    }

    fn replace_last_pop_with_return(&mut self) {
        if let Some(last) = &self.current_scope().last_instruction {
            let last_pos = last.position;
            self.replace_instruction(last_pos, make_instruction(Op::ReturnValue, &[]));
            self.scopes[self.scope_index].last_instruction = Some(EmittedInstruction {
                op: Op::ReturnValue,
                position: last_pos,
            });
        }
    }

    fn set_last_instruction(&mut self, op: Op, position: usize) {
        if let Some(ins) = self.scopes[self.scope_index].last_instruction.take() {
            self.scopes[self.scope_index].previous_instruction = Some(ins);
        }
        self.scopes[self.scope_index].last_instruction = Some(EmittedInstruction { op, position });
    }

    fn last_instruction_is(&mut self, op: Op) -> bool {
        self.current_scope()
            .last_instruction
            .as_ref()
            .filter(|emitted| emitted.op == op)
            .is_some()
    }

    fn leave_scope(&mut self) -> Instructions {
        self.scope_index -= 1;
        let outer = self.symbol_table.outer.as_ref().unwrap().borrow().clone();
        self.symbol_table = outer;
        self.scopes.pop().unwrap().instructions
    }

    fn load_symbol(&mut self, symbol: &Symbol) {
        match symbol.scope {
            SymbolScope::Global => self.emit(Op::GetGlobal, &[symbol.index]),
            SymbolScope::Local => self.emit(Op::GetLocal, &[symbol.index]),
            SymbolScope::BuiltIn => self.emit(Op::GetBuiltIn, &[symbol.index]),
            SymbolScope::Free => self.emit(Op::GetFree, &[symbol.index]),
        };
    }

    fn remove_last_pop(&mut self) {
        let current_scope = &mut self.scopes[self.scope_index];

        if let Some(emitted) = &current_scope.last_instruction {
            current_scope.instructions.truncate(emitted.position);
            current_scope.last_instruction = current_scope.previous_instruction.take();
        }
    }

    fn change_operand(&mut self, op_pos: usize, operand: usize) {
        if let Some(op) = Op::lookup_op(self.current_instructions()[op_pos]) {
            let new_instruction = make_instruction(op, &[operand]);
            self.replace_instruction(op_pos, new_instruction);
        } else {
            panic!("No such instruction at {}", op_pos);
        }
    }

    fn replace_instruction(&mut self, pos: usize, instruction: Instructions) {
        for (i, b) in instruction.iter().enumerate() {
            self.scopes[self.scope_index].instructions[pos + i] = *b;
        }
    }
}

pub struct Bytecode<'a> {
    pub instructions: &'a Instructions,
    pub constants: &'a Vec<Rc<Object>>,
}

#[cfg(test)]
mod tests {
    use super::Compiler;
    use crate::object::CompiledFunction;
    use crate::{
        ast::Program,
        code::{make_instruction, Instructions, InstructionsFns, Op},
        lexer::Lexer,
        object::Object,
        parser::Parser,
    };
    use std::borrow::Borrow;
    use std::rc::Rc;

    #[test]
    fn integer_arithmetic() {
        let tests = vec![
            (
                "1 + 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Add, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1 - 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Sub, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1 * 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Mul, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "2 / 1",
                vec![Object::Integer(2), Object::Integer(1)],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Div, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1; 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Pop, &[]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "-1;",
                vec![Object::Integer(1)],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Minus, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn boolean_expressions() {
        let tests = vec![
            (
                "true",
                vec![],
                vec![
                    make_instruction(Op::True, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "false",
                vec![],
                vec![
                    make_instruction(Op::False, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1 > 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::GreaterThan, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1 < 2",
                vec![Object::Integer(2), Object::Integer(1)],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::GreaterThan, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1 == 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Equal, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1 != 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::NotEqual, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "true == false",
                vec![],
                vec![
                    make_instruction(Op::True, &[]),
                    make_instruction(Op::False, &[]),
                    make_instruction(Op::Equal, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "true != false",
                vec![],
                vec![
                    make_instruction(Op::True, &[]),
                    make_instruction(Op::False, &[]),
                    make_instruction(Op::NotEqual, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "!true",
                vec![],
                vec![
                    make_instruction(Op::True, &[]),
                    make_instruction(Op::Bang, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn conditionals() {
        let tests = vec![
            (
                "if (true) { 10 }; 3333;",
                vec![Object::Integer(10), Object::Integer(3333)],
                vec![
                    make_instruction(Op::True, &[]),            // 0000
                    make_instruction(Op::JumpNotTruthy, &[10]), // 0001
                    make_instruction(Op::Constant, &[0]),       // 0004
                    make_instruction(Op::Jump, &[11]),          // 0007
                    make_instruction(Op::Null, &[]),            // 0010
                    make_instruction(Op::Pop, &[]),             // 0011
                    make_instruction(Op::Constant, &[1]),       // 0012
                    make_instruction(Op::Pop, &[]),             // 0015
                ],
            ),
            (
                "if (true) { 10 } else { 20 }; 3333;",
                vec![
                    Object::Integer(10),
                    Object::Integer(20),
                    Object::Integer(3333),
                ],
                vec![
                    make_instruction(Op::True, &[]),            // 0000
                    make_instruction(Op::JumpNotTruthy, &[10]), // 0001
                    make_instruction(Op::Constant, &[0]),       // 0004
                    make_instruction(Op::Jump, &[13]),          // 0007
                    make_instruction(Op::Constant, &[1]),       // 0010
                    make_instruction(Op::Pop, &[]),             // 0013
                    make_instruction(Op::Constant, &[2]),       // 0014
                    make_instruction(Op::Pop, &[]),             // 0017
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn global_let_statements() {
        let tests = vec![
            (
                "let one = 1;\n\
                 let two = 2;",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    make_instruction(Op::Constant, &[0]),  // 0000
                    make_instruction(Op::SetGlobal, &[0]), // 0003
                    make_instruction(Op::Constant, &[1]),  // 0006
                    make_instruction(Op::SetGlobal, &[1]), // 0009
                ],
            ),
            (
                "let one = 1;\n\
                 one;",
                vec![Object::Integer(1)],
                vec![
                    make_instruction(Op::Constant, &[0]),  // 0000
                    make_instruction(Op::SetGlobal, &[0]), // 0003
                    make_instruction(Op::GetGlobal, &[0]), // 0006
                    make_instruction(Op::Pop, &[]),        // 0009
                ],
            ),
            (
                "let one = 1;\n\
                 let two = one;\n\
                 two;",
                vec![Object::Integer(1)],
                vec![
                    make_instruction(Op::Constant, &[0]),  // 0000
                    make_instruction(Op::SetGlobal, &[0]), // 0003
                    make_instruction(Op::GetGlobal, &[0]), // 0006
                    make_instruction(Op::SetGlobal, &[1]), // 0009
                    make_instruction(Op::GetGlobal, &[1]), // 0012
                    make_instruction(Op::Pop, &[]),        // 0015
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn string_expressions() {
        let tests = vec![
            (
                r#""monkey""#,
                vec![Object::String("monkey".to_owned())],
                vec![
                    make_instruction(Op::Constant, &[0]), // 0000
                    make_instruction(Op::Pop, &[]),       // 0003
                ],
            ),
            (
                r#""mon" + "key""#,
                vec![
                    Object::String("mon".to_owned()),
                    Object::String("key".to_owned()),
                ],
                vec![
                    make_instruction(Op::Constant, &[0]), // 0000
                    make_instruction(Op::Constant, &[1]), // 0003
                    make_instruction(Op::Add, &[]),       // 0006
                    make_instruction(Op::Pop, &[]),       // 0007
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn array_literals() {
        let tests = vec![
            (
                "[]",
                vec![],
                vec![
                    make_instruction(Op::Array, &[0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "[1, 2, 3]",
                vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Constant, &[2]),
                    make_instruction(Op::Array, &[3]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "[1 + 2, 3 - 4, 5 * 6]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Add, &[]),
                    make_instruction(Op::Constant, &[2]),
                    make_instruction(Op::Constant, &[3]),
                    make_instruction(Op::Sub, &[]),
                    make_instruction(Op::Constant, &[4]),
                    make_instruction(Op::Constant, &[5]),
                    make_instruction(Op::Mul, &[]),
                    make_instruction(Op::Array, &[3]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn hash_literals() {
        let tests = vec![
            (
                "{}",
                vec![],
                vec![
                    make_instruction(Op::Hash, &[0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "{1: 2, 3: 4, 5: 6}",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Constant, &[2]),
                    make_instruction(Op::Constant, &[3]),
                    make_instruction(Op::Constant, &[4]),
                    make_instruction(Op::Constant, &[5]),
                    make_instruction(Op::Hash, &[6]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "{1: 2 + 3, 4: 5 * 6}",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Constant, &[2]),
                    make_instruction(Op::Add, &[]),
                    make_instruction(Op::Constant, &[3]),
                    make_instruction(Op::Constant, &[4]),
                    make_instruction(Op::Constant, &[5]),
                    make_instruction(Op::Mul, &[]),
                    make_instruction(Op::Hash, &[4]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn index_expressions() {
        let tests = vec![
            (
                "[1, 2, 3][1 + 1]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(1),
                    Object::Integer(1),
                ],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Constant, &[2]),
                    make_instruction(Op::Array, &[3]),
                    make_instruction(Op::Constant, &[3]),
                    make_instruction(Op::Constant, &[4]),
                    make_instruction(Op::Add, &[]),
                    make_instruction(Op::Index, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "{1: 2}[2 - 1]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(2),
                    Object::Integer(1),
                ],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Hash, &[2]),
                    make_instruction(Op::Constant, &[2]),
                    make_instruction(Op::Constant, &[3]),
                    make_instruction(Op::Sub, &[]),
                    make_instruction(Op::Index, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn compiler_scopes() {
        let mut compiler = Compiler::new();

        assert_eq!(
            0, compiler.scope_index,
            "new compiler's scope index should be 0"
        );

        let global_symbol_table = compiler.symbol_table.clone();

        compiler.emit(Op::Mul, &[]);

        compiler.enter_scope();

        assert_ne!(
            global_symbol_table, compiler.symbol_table,
            "SymbolTable has been enclosed"
        );

        assert_eq!(
            1, compiler.scope_index,
            "compiler's scope index should be 1 after entering a new scope"
        );

        compiler.emit(Op::Sub, &[]);

        assert_eq!(1, compiler.scopes[compiler.scope_index].instructions.len());

        let last = &compiler.scopes[compiler.scope_index]
            .last_instruction
            .as_ref()
            .unwrap();
        assert_eq!(Op::Sub, last.op, "last instruction");

        // assert_eq!(
        //     global_symbol_table,
        //     compiler
        //         .symbol_table
        //         .outer
        //         .as_ref()
        //         .unwrap()
        //         .borrow()
        //         .clone(),
        //     "Compiler did not enclose SymbolTable"
        // );

        compiler.leave_scope();

        assert_eq!(
            0, compiler.scope_index,
            "compiler's scope index should be 0 after leaving a scope"
        );

        assert_eq!(
            global_symbol_table, compiler.symbol_table,
            "Compiler did not restore global symbol table"
        );

        assert_eq!(None, compiler.symbol_table.outer);

        compiler.emit(Op::Add, &[]);

        assert_eq!(2, compiler.scopes[compiler.scope_index].instructions.len());

        let last = &compiler.scopes[compiler.scope_index]
            .last_instruction
            .as_ref()
            .unwrap();
        assert_eq!(Op::Add, last.op, "last instruction");

        let previous = &compiler.scopes[compiler.scope_index]
            .previous_instruction
            .as_ref()
            .unwrap();
        assert_eq!(Op::Mul, previous.op, "previous instruction");
    }

    #[test]
    fn functions() {
        let tests = vec![
            (
                "fn() { return 5 + 10 }",
                vec![
                    Object::Integer(5),
                    Object::Integer(10),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::Constant, &[0]),
                            make_instruction(Op::Constant, &[1]),
                            make_instruction(Op::Add, &[]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 0,
                        num_parameters: 0,
                    })),
                ],
                vec![
                    make_instruction(Op::Closure, &[2, 0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "fn() { 5 + 10 }",
                vec![
                    Object::Integer(5),
                    Object::Integer(10),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::Constant, &[0]),
                            make_instruction(Op::Constant, &[1]),
                            make_instruction(Op::Add, &[]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 0,
                        num_parameters: 0,
                    })),
                ],
                vec![
                    make_instruction(Op::Closure, &[2, 0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "fn() { 1; 2 }",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::Constant, &[0]),
                            make_instruction(Op::Pop, &[]),
                            make_instruction(Op::Constant, &[1]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 0,
                        num_parameters: 0,
                    })),
                ],
                vec![
                    make_instruction(Op::Closure, &[2, 0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn functions_without_return_value() {
        let tests = vec![(
            "fn() { }",
            vec![Object::CompiledFunction(Rc::new(CompiledFunction {
                instructions: vec![make_instruction(Op::Return, &[])].concat(),
                num_locals: 0,
                num_parameters: 0,
            }))],
            vec![
                make_instruction(Op::Closure, &[0, 0]),
                make_instruction(Op::Pop, &[]),
            ],
        )];

        run_compiler_tests(tests);
    }

    #[test]
    fn function_calls() {
        let tests = vec![
            (
                "fn() { 24 }();",
                vec![
                    Object::Integer(24),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::Constant, &[0]), // the literal 24
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 0,
                        num_parameters: 0,
                    })),
                ],
                vec![
                    make_instruction(Op::Closure, &[1, 0]), // the compiled function
                    make_instruction(Op::Call, &[0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "let noArg = fn() { 24 };\
                 noArg();\
                 ",
                vec![
                    Object::Integer(24),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::Constant, &[0]), // the literal 24
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 0,
                        num_parameters: 0,
                    })),
                ],
                vec![
                    make_instruction(Op::Closure, &[1, 0]), // the compiled function
                    make_instruction(Op::SetGlobal, &[0]),
                    make_instruction(Op::GetGlobal, &[0]),
                    make_instruction(Op::Call, &[0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "let oneArg = fn(a) { a };\
                 oneArg(24);\
                 ",
                vec![
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::GetLocal, &[0]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 1,
                        num_parameters: 1,
                    })),
                    Object::Integer(24),
                ],
                vec![
                    make_instruction(Op::Closure, &[0, 0]),
                    make_instruction(Op::SetGlobal, &[0]),
                    make_instruction(Op::GetGlobal, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Call, &[1]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "let manyArg = fn(a, b, c) { a; b; c };\
                 manyArg(24, 25, 26);\
                 ",
                vec![
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::GetLocal, &[0]),
                            make_instruction(Op::Pop, &[]),
                            make_instruction(Op::GetLocal, &[1]),
                            make_instruction(Op::Pop, &[]),
                            make_instruction(Op::GetLocal, &[2]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 3,
                        num_parameters: 3,
                    })),
                    Object::Integer(24),
                    Object::Integer(25),
                    Object::Integer(26),
                ],
                vec![
                    make_instruction(Op::Closure, &[0, 0]),
                    make_instruction(Op::SetGlobal, &[0]),
                    make_instruction(Op::GetGlobal, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Constant, &[2]),
                    make_instruction(Op::Constant, &[3]),
                    make_instruction(Op::Call, &[3]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn let_statement_scopes() {
        let tests = vec![
            (
                "let num = 55; fn() { num }",
                vec![
                    Object::Integer(55),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::GetGlobal, &[0]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),

                        num_locals: 0,
                        num_parameters: 0,
                    })),
                ],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::SetGlobal, &[0]),
                    make_instruction(Op::Closure, &[1, 0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "fn() { let num = 55; num }",
                vec![
                    Object::Integer(55),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::Constant, &[0]),
                            make_instruction(Op::SetLocal, &[0]),
                            make_instruction(Op::GetLocal, &[0]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 1,
                        num_parameters: 0,
                    })),
                ],
                vec![
                    make_instruction(Op::Closure, &[1, 0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "fn() { 
                    let a = 55;
                    let b = 77;
                    a + b
                }",
                vec![
                    Object::Integer(55),
                    Object::Integer(77),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::Constant, &[0]),
                            make_instruction(Op::SetLocal, &[0]),
                            make_instruction(Op::Constant, &[1]),
                            make_instruction(Op::SetLocal, &[1]),
                            make_instruction(Op::GetLocal, &[0]),
                            make_instruction(Op::GetLocal, &[1]),
                            make_instruction(Op::Add, &[]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),

                        num_locals: 2,
                        num_parameters: 0,
                    })),
                ],
                vec![
                    make_instruction(Op::Closure, &[2, 0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn builtins() {
        let tests = vec![
            (
                r#"len("")"#,
                vec![Object::String("".to_owned())],
                vec![
                    make_instruction(Op::GetBuiltIn, &[0]),
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Call, &[1]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "len([]);\
                 push([], 1);",
                vec![Object::Integer(1)],
                vec![
                    make_instruction(Op::GetBuiltIn, &[0]),
                    make_instruction(Op::Array, &[0]),
                    make_instruction(Op::Call, &[1]),
                    make_instruction(Op::Pop, &[]),
                    make_instruction(Op::GetBuiltIn, &[5]),
                    make_instruction(Op::Array, &[0]),
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Call, &[2]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "fn() { len([]) }",
                vec![Object::CompiledFunction(Rc::new(CompiledFunction {
                    instructions: vec![
                        make_instruction(Op::GetBuiltIn, &[0]),
                        make_instruction(Op::Array, &[0]),
                        make_instruction(Op::Call, &[1]),
                        make_instruction(Op::ReturnValue, &[]),
                    ]
                    .concat(),
                    num_locals: 0,
                    num_parameters: 0,
                }))],
                vec![
                    make_instruction(Op::Closure, &[0, 0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn closures() {
        let tests = vec![
            (
                "fn(a) {
                    fn(b) {
                        a + b
                    }
                }",
                vec![
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::GetFree, &[0]),
                            make_instruction(Op::GetLocal, &[0]),
                            make_instruction(Op::Add, &[]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 1,
                        num_parameters: 1,
                    })),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::GetLocal, &[0]),
                            make_instruction(Op::Closure, &[0, 1]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 1,
                        num_parameters: 1,
                    })),
                ],
                vec![
                    make_instruction(Op::Closure, &[1, 0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "fn(a) {
                    fn(b) {
                        fn(c) {
                            a + b + c
                        }
                    }
                };",
                vec![
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::GetFree, &[0]),
                            make_instruction(Op::GetFree, &[1]),
                            make_instruction(Op::Add, &[]),
                            make_instruction(Op::GetLocal, &[0]),
                            make_instruction(Op::Add, &[]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 1,
                        num_parameters: 1,
                    })),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::GetFree, &[0]),
                            make_instruction(Op::GetLocal, &[0]),
                            make_instruction(Op::Closure, &[0, 2]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 1,
                        num_parameters: 1,
                    })),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::GetLocal, &[0]),
                            make_instruction(Op::Closure, &[1, 1]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 1,
                        num_parameters: 1,
                    })),
                ],
                vec![
                    make_instruction(Op::Closure, &[2, 0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "let global = 55;

                fn() {
                    let a = 66;

                    fn() {
                        let b = 77;

                        fn() {
                            let c = 88;

                            global + a + b + c;
                        }
                    }
                }",
                vec![
                    Object::Integer(55),
                    Object::Integer(66),
                    Object::Integer(77),
                    Object::Integer(88),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::Constant, &[3]),
                            make_instruction(Op::SetLocal, &[0]),
                            make_instruction(Op::GetGlobal, &[0]),
                            make_instruction(Op::GetFree, &[0]),
                            make_instruction(Op::Add, &[]),
                            make_instruction(Op::GetFree, &[1]),
                            make_instruction(Op::Add, &[]),
                            make_instruction(Op::GetLocal, &[0]),
                            make_instruction(Op::Add, &[]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 1,
                        num_parameters: 0,
                    })),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::Constant, &[2]),
                            make_instruction(Op::SetLocal, &[0]),
                            make_instruction(Op::GetFree, &[0]),
                            make_instruction(Op::GetLocal, &[0]),
                            make_instruction(Op::Closure, &[4, 2]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 1,
                        num_parameters: 0,
                    })),
                    Object::CompiledFunction(Rc::new(CompiledFunction {
                        instructions: vec![
                            make_instruction(Op::Constant, &[1]),
                            make_instruction(Op::SetLocal, &[0]),
                            make_instruction(Op::GetLocal, &[0]),
                            make_instruction(Op::Closure, &[5, 1]),
                            make_instruction(Op::ReturnValue, &[]),
                        ]
                        .concat(),
                        num_locals: 1,
                        num_parameters: 0,
                    })),
                ],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::SetGlobal, &[0]),
                    make_instruction(Op::Closure, &[6, 0]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
        ];

        run_compiler_tests(tests);
    }

    fn run_compiler_tests(tests: Vec<(&str, Vec<Object>, Vec<Instructions>)>) {
        for (input, expected_constants, expected_instructions) in tests {
            let program = parse(input);

            let mut compiler = Compiler::new();

            match compiler.compile(&program) {
                Ok(bytecode) => {
                    expect_instructions(expected_instructions, &bytecode.instructions);
                    expect_constants(expected_constants, bytecode.constants);
                }
                Err(e) => panic!("{}", e),
            }
        }
    }

    fn expect_constants(expected: Vec<Object>, actual: &Vec<Rc<Object>>) {
        assert_eq!(
            expected.len(),
            actual.len(),
            "wrong number of constants. Expected {} but got {}",
            expected.len(),
            actual.len()
        );

        for (i, b) in expected.iter().enumerate() {
            match (b, &actual[i].borrow()) {
                (Object::Integer(expected_value), Object::Integer(actual_value)) => {
                    assert_eq!(
                        expected_value, actual_value,
                        "wrong constant at {}. expected {} but got {}",
                        i, b, actual_value
                    );
                }
                (Object::String(expected_value), Object::String(actual_value)) => {
                    assert_eq!(
                        expected_value, actual_value,
                        "wrong constant at {}. expected {} but got {}",
                        i, b, actual_value
                    );
                }
                (Object::CompiledFunction(expected), Object::CompiledFunction(actual)) => {
                    assert_eq!(
                        expected.num_locals, actual.num_locals,
                        "number of locals matches"
                    );
                    assert_eq!(
                        expected.num_parameters, actual.num_parameters,
                        "number of parameters matches"
                    );
                    expect_instructions(vec![expected.instructions.to_vec()], &actual.instructions);
                }
                _ => panic!(
                    "Type mismatch. Expected {} but got {}",
                    b.type_name(),
                    actual[i].type_name()
                ),
            }
        }
    }

    fn expect_instructions(expected_stream: Vec<Instructions>, actual: &Instructions) {
        let expected = expected_stream.concat();

        assert_eq!(
            expected.len(),
            actual.len(),
            "wrong number of instructions. Expected {} but got {}",
            expected.to_string(),
            actual.to_string()
        );

        for (i, b) in expected.iter().enumerate() {
            assert_eq!(
                *b,
                actual[i],
                "wrong instruction at {}. expected {} in \n{}\nbut got {} in \n{}",
                i,
                b,
                expected.to_string(),
                actual[i],
                actual.to_string(),
            );
        }
    }

    fn parse(input: &str) -> Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        p.parse_program()
    }
}
