use crate::ast::Expression;
use crate::ast::PrefixOperator;
use crate::ast::Program;
use crate::ast::Statement;
use crate::object::Object;
use std::fmt;

type EvalResult = std::result::Result<Object, EvalError>;

pub enum EvalError {
    UnimplementedExpression(String),
    UnimplementedStatement(String),
    UnsupportedPrefixOperator(PrefixOperator, Object),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::UnimplementedExpression(e) => {
                write!(f, "Not yet implemented evaluating the expression '{}'", e)
            }
            EvalError::UnimplementedStatement(s) => {
                write!(f, "Not yet implemented evaluating the expression '{}'", s)
            }
            EvalError::UnsupportedPrefixOperator(operator, obj) => {
                write!(f, "Cannot evaluate {}{}", operator, obj)
            }
        }
    }
}

pub fn eval(program: &Program) -> EvalResult {
    let mut res = Object::Null;

    for statement in &program.statements {
        res = eval_statement(statement)?;
    }

    Ok(res)
}

fn eval_statement(statement: &Statement) -> EvalResult {
    match statement {
        Statement::Expression(exp) => eval_expression(exp),
        s => Err(EvalError::UnimplementedStatement(s.to_string())),
    }
}

fn eval_expression(expression: &Expression) -> EvalResult {
    match expression {
        Expression::IntegerLiteral(v) => Ok(Object::Integer(*v)),
        Expression::Boolean(b) => Ok(Object::Boolean(*b)),
        Expression::Prefix(operator, expression) => eval_prefix_expression(operator, expression),
        expression => Err(EvalError::UnimplementedExpression(expression.to_string())),
    }
}

fn eval_prefix_expression(operator: &PrefixOperator, expression: &Expression) -> EvalResult {
    let obj = eval_expression(expression)?;
    match operator {
        PrefixOperator::Bang => Ok(Object::Boolean(!obj.is_truthy())),
        _ => Err(EvalError::UnsupportedPrefixOperator(operator.clone(), obj)),
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluator;
    use crate::evaluator::EvalResult;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn eval_integer_expression() {
        expect_values(vec![("5;", "5"), ("10;", "10")]);
    }

    #[test]
    fn eval_boolean_expression() {
        expect_values(vec![("true;", "true"), ("false;", "false")]);
    }

    #[test]
    fn eval_bang_expression() {
        expect_values(vec![
            ("!true;", "false"),
            ("!false;", "true"),
            ("!5;", "false"),
            ("!!true;", "true"),
            ("!!false;", "false"),
            ("!!5;;", "true"),
        ]);
    }

    fn expect_values(tests: Vec<(&str, &str)>) {
        for (input, expected) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    assert_eq!(obj.to_string(), expected.to_string(), "for `{}`", input);
                }
                Err(err) => {
                    panic!(
                        "expected `{}`, but got error={} for `{}`",
                        expected, err, input
                    );
                }
            }
        }
    }

    fn eval_input(input: &str) -> EvalResult {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        evaluator::eval(&program)
    }
}
