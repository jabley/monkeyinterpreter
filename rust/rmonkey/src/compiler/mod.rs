use crate::ast::Expression;
use crate::ast::Statement;
use crate::{
    ast::{BlockStatement, InfixOperator, PrefixOperator, Program},
    code::{make_instruction, Instructions, Op},
    object::Object,
};
use std::{error::Error, fmt};

#[derive(Clone)]
struct EmittedInstruction {
    op: Op,
    position: usize,
}

#[derive(Debug)]
pub enum CompilerError {}

impl fmt::Display for CompilerError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Error for CompilerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

/// Compiler is responsible for taking an AST and turning it into bytecode.
#[derive(Default)]
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn compile(&mut self, program: &Program) -> Result<Bytecode, CompilerError> {
        for stmt in &program.statements {
            self.compile_statement(stmt)?;
        }

        Ok(self.bytecode())
    }

    fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn compile_statement(&mut self, stmt: &Statement) -> Result<(), CompilerError> {
        match stmt {
            Statement::Expression(exp) => {
                self.compile_expression(&exp)?;
                self.emit(Op::Pop, &[]);
                Ok(())
            }
            _ => todo!(),
        }
    }

    fn compile_block_statement(&mut self, body: &BlockStatement) -> Result<(), CompilerError> {
        for stmt in &body.statements {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }

    fn compile_expression(&mut self, exp: &Expression) -> Result<(), CompilerError> {
        match exp {
            Expression::If(condition, consequence, alternative) => {
                self.compile_expression(condition)?;

                // emit an Op::JumpNotTruthy with a hard-coded nonsense value for now.
                let jump_not_truthy_position = self.emit(Op::JumpNotTruthy, &[9999]); // this is the right structure, but a nonsense value

                self.compile_block_statement(consequence)?;

                if self.is_last_instruction_pop() {
                    self.remove_last_pop();
                }

                match alternative {
                    Some(body) => {
                        // emit an Op::Jump with a hard-coded nonsense value for now
                        let jump_position = self.emit(Op::Jump, &[9999]);

                        let after_consequence_position = self.instructions.len();
                        self.change_operand(jump_not_truthy_position, after_consequence_position);

                        self.compile_block_statement(body)?;

                        if self.is_last_instruction_pop() {
                            self.remove_last_pop();
                        }

                        let after_alternative_position = self.instructions.len();
                        self.change_operand(jump_position, after_alternative_position);
                    }
                    None => {
                        let after_consequence_position = self.instructions.len();
                        self.change_operand(jump_not_truthy_position, after_consequence_position);
                    }
                }

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
            _ => todo!("Not handling expression {} yet", exp),
        }
    }

    fn emit(&mut self, op: Op, operands: &[usize]) -> usize {
        let instruction = make_instruction(op, operands);
        let pos = self.add_instruction(instruction);

        self.set_last_instruction(op, pos);

        pos
    }

    fn add_instruction(&mut self, instruction: Instructions) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.extend(instruction);
        pos_new_instruction
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn set_last_instruction(&mut self, op: Op, pos: usize) {
        self.previous_instruction = self.last_instruction.take();
        self.last_instruction = Some(EmittedInstruction { op, position: pos });
    }

    fn is_last_instruction_pop(&self) -> bool {
        self.last_instruction
            .as_ref()
            .filter(|emitted| emitted.op == Op::Pop)
            .is_some()
    }

    fn remove_last_pop(&mut self) {
        if let Some(emitted) = &self.last_instruction {
            self.instructions.truncate(emitted.position);
            self.last_instruction = self.previous_instruction.take();
        }
    }

    fn change_operand(&mut self, op_pos: usize, operand: usize) {
        if let Some(op) = Op::lookup_op(self.instructions[op_pos]) {
            let new_instruction = make_instruction(op, &[operand]);
            self.replace_instruction(op_pos, new_instruction);
        } else {
            panic!("No such instruction at {}", op_pos);
        }
    }

    fn replace_instruction(&mut self, pos: usize, instruction: Instructions) {
        for (i, b) in instruction.iter().enumerate() {
            self.instructions[pos + i] = *b;
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
        let tests: Vec<(&str, Vec<i64>, Vec<Instructions>)> = vec![
            (
                "1 + 2",
                vec![1, 2],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Add, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1 - 2",
                vec![1, 2],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Sub, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1 * 2",
                vec![1, 2],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Mul, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "2 / 1",
                vec![2, 1],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Div, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1; 2",
                vec![1, 2],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Pop, &[]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "-1;",
                vec![1],
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
        let tests: Vec<(&str, Vec<i64>, Vec<Instructions>)> = vec![
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
                vec![1, 2],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::GreaterThan, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1 < 2",
                vec![2, 1],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::GreaterThan, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1 == 2",
                vec![1, 2],
                vec![
                    make_instruction(Op::Constant, &[0]),
                    make_instruction(Op::Constant, &[1]),
                    make_instruction(Op::Equal, &[]),
                    make_instruction(Op::Pop, &[]),
                ],
            ),
            (
                "1 != 2",
                vec![1, 2],
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
        let tests: Vec<(&str, Vec<i64>, Vec<Instructions>)> = vec![
            (
                "if (true) { 10 }; 3333;",
                vec![10, 3333],
                vec![
                    make_instruction(Op::True, &[]),           // 0000
                    make_instruction(Op::JumpNotTruthy, &[7]), // 0001
                    make_instruction(Op::Constant, &[0]),      // 0004
                    make_instruction(Op::Pop, &[]),            // 0007
                    make_instruction(Op::Constant, &[1]),      // 0008
                    make_instruction(Op::Pop, &[]),            // 0011
                ],
            ),
            (
                "if (true) { 10 } else { 20 }; 3333;",
                vec![10, 20, 3333],
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

    fn run_compiler_tests(tests: Vec<(&str, Vec<i64>, Vec<Instructions>)>) {
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

    fn expect_constants(expected: Vec<i64>, actual: Vec<crate::object::Object>) {
        assert_eq!(
            expected.len(),
            actual.len(),
            "wrong number of constants. Expected {} but got {}",
            expected.len(),
            actual.len()
        );

        for (i, b) in expected.iter().enumerate() {
            match actual[i] {
                Object::Integer(actual) => {
                    assert_eq!(
                        *b, actual,
                        "wrong instruction at {}. expected {} but got {}",
                        i, b, actual
                    );
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
