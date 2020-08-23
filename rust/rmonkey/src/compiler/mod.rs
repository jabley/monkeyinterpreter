use crate::ast::Expression;
use crate::ast::Statement;
use crate::{
    ast::{InfixOperator, Program},
    code::{make_instruction, Instructions, Op},
    object::Object,
};
use std::{error::Error, fmt};

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

    fn compile_expression(&mut self, exp: &Expression) -> Result<(), CompilerError> {
        match exp {
            Expression::Infix(operator, left, right) => {
                self.compile_expression(left)?;
                self.compile_expression(right)?;

                match operator {
                    InfixOperator::Plus => self.emit(Op::Add, &[]),
                    InfixOperator::Minus => self.emit(Op::Sub, &[]),
                    InfixOperator::Asterisk => self.emit(Op::Mul, &[]),
                    InfixOperator::Slash => self.emit(Op::Div, &[]),
                    _ => todo!("Unknown operator: {}", operator),
                }

                Ok(())
            }
            Expression::IntegerLiteral(v) => {
                let constant = self.add_constant(Object::Integer(*v));
                let operands = vec![constant];
                self.emit(Op::Constant, &operands);
                Ok(())
            }
            _ => todo!("Not handling expression {} yet", exp),
        }
    }

    fn emit(&mut self, op: Op, operands: &[usize]) {
        let instruction = make_instruction(op, operands);
        self.add_instruction(instruction);
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
        ];

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
