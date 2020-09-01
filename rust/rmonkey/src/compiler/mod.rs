pub mod symbol_table;

use crate::ast::Expression;
use crate::ast::Statement;
use crate::{
    ast::{BlockStatement, InfixOperator, PrefixOperator, Program},
    code::{make_instruction, Instructions, Op},
    object::Object,
};
use std::{error::Error, fmt};
pub use symbol_table::SymbolTable;

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
    pub constants: Vec<Object>,
    pub symbol_table: SymbolTable,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let mut res: Self = Default::default();

        let main_scope = CompilationScope::new();

        res.scopes.push(main_scope);

        res
    }

    /// new_with_state returns a Compiler that can accept a SymbolTable and constants of a previous
    /// compilation. This is needed by the REPL.
    pub fn new_with_state(symbol_table: SymbolTable, constants: Vec<Object>) -> Self {
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

    fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions().clone(),
            constants: self.constants.clone(),
        }
    }

    fn compile_statement(&mut self, stmt: &Statement) -> Result<(), CompilerError> {
        match stmt {
            Statement::Expression(exp) => {
                self.compile_expression(&exp)?;
                self.emit(Op::Pop, &[]);
            }
            Statement::Let(ident, exp) => {
                self.compile_expression(exp)?;
                let symbol = self.symbol_table.define(ident);
                self.emit(Op::SetGlobal, &[symbol.index]);
            }
            Statement::Return(Some(exp)) => {
                self.compile_expression(exp)?;
                self.emit(Op::ReturnValue, &[]);
            }
            _ => todo!(),
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
                    self.emit(Op::GetGlobal, &[symbol.index]);
                    Ok(())
                } else {
                    Err(CompilerError::UnknownVariable(ident.to_string()))
                }
            }
            Expression::If(condition, consequence, alternative) => {
                self.compile_expression(condition)?;

                // emit an Op::JumpNotTruthy with a hard-coded nonsense value for now.
                let jump_not_truthy_position = self.emit(Op::JumpNotTruthy, &[9999]); // this is the right structure, but a nonsense value

                self.compile_block_statement(consequence)?;

                if self.is_last_instruction_pop() {
                    self.remove_last_pop();
                }

                // emit an Op::Jump with a hard-coded nonsense value for now
                let jump_position = self.emit(Op::Jump, &[9999]);

                let after_consequence_position = self.current_instructions().len();

                self.change_operand(jump_not_truthy_position, after_consequence_position);

                match alternative {
                    Some(body) => {
                        self.compile_block_statement(body)?;

                        if self.is_last_instruction_pop() {
                            self.remove_last_pop();
                        }
                    }
                    None => {
                        self.emit(Op::Null, &[]);
                    }
                }

                let after_alternative_position = self.current_instructions().len();
                self.change_operand(jump_position, after_alternative_position);

                Ok(())
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

                Ok(())
            }

            Expression::Prefix(operator, expr) => {
                self.compile_expression(expr)?;

                match operator {
                    PrefixOperator::Bang => self.emit(Op::Bang, &[]),
                    PrefixOperator::Minus => self.emit(Op::Minus, &[]),
                };

                Ok(())
            }
            Expression::IntegerLiteral(v) => {
                let constant = self.add_constant(Object::Integer(*v));
                let operands = vec![constant];
                self.emit(Op::Constant, &operands);
                Ok(())
            }
            Expression::Boolean(v) => {
                if *v {
                    self.emit(Op::True, &[]);
                } else {
                    self.emit(Op::False, &[]);
                }
                Ok(())
            }
            Expression::StringLiteral(s) => {
                let constant = self.add_constant(Object::String(s.to_string()));
                let operands = vec![constant];
                self.emit(Op::Constant, &operands);
                Ok(())
            }
            Expression::ArrayLiteral(elements) => {
                for exp in elements {
                    self.compile_expression(exp)?;
                }
                self.emit(Op::Array, &[elements.len()]);

                Ok(())
            }
            Expression::HashLiteral(hash) => {
                for (k, v) in &hash.pairs {
                    self.compile_expression(k)?;
                    self.compile_expression(v)?;
                }

                self.emit(Op::Hash, &[2 * hash.pairs.len()]);

                Ok(())
            }
            Expression::IndexExpression(left, index) => {
                self.compile_expression(left)?;
                self.compile_expression(index)?;

                self.emit(Op::Index, &[]);

                Ok(())
            }
            Expression::FunctionLiteral(_, body) => {
                self.enter_scope();

                self.compile_block_statement(body)?;

                let instructions = self.leave_scope();

                let compiled_function = Object::CompiledFunction(instructions);
                let constant = self.add_constant(compiled_function);
                self.emit(Op::Constant, &[constant]);

                Ok(())
            }
            _ => todo!("Not handling expression {} yet", exp),
        }
    }

    pub fn current_instructions(&self) -> &Instructions {
        &self.scopes[self.scope_index].instructions
    }

    fn emit(&mut self, op: Op, operands: &[usize]) -> usize {
        let instruction = make_instruction(op, operands);
        let pos = self.add_instruction(instruction);

        self.set_last_instruction(op, pos);

        pos
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope::new();

        self.scopes.push(scope);
        self.scope_index += 1;
    }

    fn add_instruction(&mut self, instruction: Instructions) -> usize {
        let pos_new_instruction = self.current_instructions().len();
        self.scopes[self.scope_index]
            .instructions
            .extend(instruction);

        pos_new_instruction
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn set_last_instruction(&mut self, op: Op, position: usize) {
        if let Some(ins) = &self.scopes[self.scope_index].last_instruction {
            self.scopes[self.scope_index].previous_instruction = Some(ins.clone());
        }
        self.scopes[self.scope_index].last_instruction = Some(EmittedInstruction { op, position });
    }

    fn is_last_instruction_pop(&self) -> bool {
        self.scopes[self.scope_index]
            .last_instruction
            .as_ref()
            .filter(|emitted| emitted.op == Op::Pop)
            .is_some()
    }

    fn leave_scope(&mut self) -> Instructions {
        self.scope_index -= 1;

        self.scopes.pop().unwrap().instructions
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
        let scope = &mut self.scopes[self.scope_index];

        for (i, b) in instruction.iter().enumerate() {
            scope.instructions[pos + i] = *b;
        }
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[cfg(test)]
mod tests {
    use super::Compiler;
    use crate::{
        ast::Program,
        code::{make_instruction, Instructions, Op},
        lexer::Lexer,
        object::Object,
        parser::Parser,
    };

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

        compiler.emit(Op::Mul, &[]);

        compiler.enter_scope();

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

        compiler.leave_scope();

        assert_eq!(
            0, compiler.scope_index,
            "compiler's scope index should be 0 after leaving a scope"
        );

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
        let tests = vec![(
            "fn() { return 5 + 10 }",
            vec![
                Object::Integer(5),
                Object::Integer(10),
                Object::CompiledFunction(
                    vec![
                        make_instruction(Op::Constant, &[0]),
                        make_instruction(Op::Constant, &[1]),
                        make_instruction(Op::Add, &[]),
                        make_instruction(Op::ReturnValue, &[]),
                    ]
                    .concat(),
                ),
            ],
            vec![
                make_instruction(Op::Constant, &[2]),
                make_instruction(Op::Pop, &[]),
            ],
        )];

        run_compiler_tests(tests);
    }

    fn run_compiler_tests(tests: Vec<(&str, Vec<Object>, Vec<Instructions>)>) {
        for (input, expected_constants, expected_instructions) in tests {
            let program = parse(input);

            let mut compiler = Compiler::new();

            match compiler.compile(&program) {
                Ok(bytecode) => {
                    expect_instructions(expected_instructions, bytecode.instructions);
                    expect_constants(expected_constants, bytecode.constants);
                }
                Err(e) => panic!(e),
            }
        }
    }

    fn expect_constants(expected: Vec<Object>, actual: Vec<Object>) {
        assert_eq!(
            expected.len(),
            actual.len(),
            "wrong number of constants. Expected {} but got {}",
            expected.len(),
            actual.len()
        );

        for (i, b) in expected.iter().enumerate() {
            match (b, &actual[i]) {
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
                (Object::CompiledFunction(expected_value), Object::CompiledFunction(actual_value)) => {
                    expect_instructions(vec![expected_value.to_vec()], actual_value.clone());
                }
                _ => todo!(
                    "Not implemented assertion for type: {}",
                    actual[i].type_name()
                ),
            }
        }
    }

    fn expect_instructions(expected_stream: Vec<Instructions>, actual: Instructions) {
        let expected = expected_stream.concat();

        assert_eq!(
            expected.len(),
            actual.len(),
            "wrong number of instructions. Expected {} but got {}",
            expected.len(),
            actual.len()
        );

        for (i, b) in expected.iter().enumerate() {
            assert_eq!(
                *b, actual[i],
                "wrong instruction at {}. expected {} but got {}",
                i, b, actual[i]
            );
        }
    }

    fn parse(input: &str) -> Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        p.parse_program()
    }
}
