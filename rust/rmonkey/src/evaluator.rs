use crate::ast::{Expression, InfixOperator, PrefixOperator, Program, Statement};
use crate::object::Object;
use std::fmt;

type EvalResult = std::result::Result<Object, EvalError>;

pub enum EvalError {
    UnimplementedExpression(String),
    UnimplementedStatement(String),
    UnsupportedInfixOperator(InfixOperator, Object, Object),
    UnsupportedPrefixOperator(PrefixOperator, Object),
    TypeMismatch(InfixOperator, Object, Object),
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
            EvalError::TypeMismatch(operator, left, right) => write!(
                f,
                "Type mismatch: {} {} {}",
                left.type_name(),
                operator,
                right.type_name()
            ),
            EvalError::UnsupportedInfixOperator(operator, left, right) => {
                write!(f, "Cannot evaluate {}{}{}", left, operator, right)
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
        Expression::Infix(operator, left, right) => eval_infix_expression(operator, left, right),
        expression => Err(EvalError::UnimplementedExpression(expression.to_string())),
    }
}

fn eval_prefix_expression(operator: &PrefixOperator, expression: &Expression) -> EvalResult {
    let obj = eval_expression(expression)?;

    match operator {
        PrefixOperator::Bang => Ok(Object::Boolean(!obj.is_truthy())),
        PrefixOperator::Minus => match obj {
            Object::Integer(v) => Ok(Object::Integer(-v)),
            _ => Err(EvalError::UnsupportedPrefixOperator(operator.clone(), obj)),
        },
    }
}

fn eval_infix_expression(
    operator: &InfixOperator,
    left_exp: &Expression,
    right_exp: &Expression,
) -> EvalResult {
    let left_obj = eval_expression(left_exp)?;
    let right_obj = eval_expression(right_exp)?;

    match (left_obj, right_obj) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expressions(operator, left, right)
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expressions(operator, left, right)
        }
        (left, right) => Err(EvalError::TypeMismatch(operator.clone(), left, right)),
    }
}

fn eval_integer_infix_expressions(operator: &InfixOperator, left: i64, right: i64) -> EvalResult {
    match operator {
        InfixOperator::Eq => Ok(Object::Boolean(left == right)),
        InfixOperator::NotEq => Ok(Object::Boolean(left != right)),
        InfixOperator::Lt => Ok(Object::Boolean(left < right)),
        InfixOperator::Gt => Ok(Object::Boolean(left > right)),
        InfixOperator::Plus => Ok(Object::Integer(left + right)),
        InfixOperator::Minus => Ok(Object::Integer(left - right)),
        InfixOperator::Asterisk => Ok(Object::Integer(left * right)),
        InfixOperator::Slash => Ok(Object::Integer(left / right)),
    }
}

fn eval_boolean_infix_expressions(operator: &InfixOperator, left: bool, right: bool) -> EvalResult {
    match operator {
        InfixOperator::Eq => Ok(Object::Boolean(left == right)),
        InfixOperator::NotEq => Ok(Object::Boolean(left != right)),
        _ => Err(EvalError::UnsupportedInfixOperator(
            operator.clone(),
            Object::Boolean(left),
            Object::Boolean(right),
        )),
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
        expect_values(vec![
            ("5;", "5"),
            ("10;", "10"),
            ("-5", "-5"),
            ("-10", "-10"),
            ("5 + 5 + 5 + 5 - 10", "10"),
            ("2 * 2 * 2 * 2 * 2", "32"),
            ("-50 + 100 + -50", "0"),
            ("5 * 2 + 10", "20"),
            ("5 + 2 * 10", "25"),
            ("20 + 2 * -10", "0"),
            ("50 / 2 * 2 + 10", "60"),
            ("2 * (5 + 10)", "30"),
            ("3 * 3 * 3 + 10", "37"),
            ("3 * (3 * 3) + 10", "37"),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", "50"),
        ]);
    }

    #[test]
    fn eval_boolean_expression() {
        expect_values(vec![
            ("true;", "true"),
            ("false;", "false"),
            ("1 < 2", "true"),
            ("1 > 2", "false"),
            ("1 < 1", "false"),
            ("1 > 1", "false"),
            ("1 == 1", "true"),
            ("1 != 1", "false"),
            ("1 == 2", "false"),
            ("1 != 2", "true"),
            ("true == true", "true"),
            ("false == false", "true"),
            ("true == false", "false"),
            ("true != false", "true"),
            ("false != true", "true"),
            ("(1 < 2) == true", "true"),
            ("(1 < 2) == false", "false"),
            ("(1 > 2) == true", "false"),
            ("(1 > 2) == false", "true"),
        ]);
    }

    #[test]
    fn eval_nonsensical_minus_expresions() {
        expect_errors(vec![
            ("-true", "Cannot evaluate -true"),
            ("-false", "Cannot evaluate -false"),
            ("5 - false", "Type mismatch: INTEGER - BOOLEAN"),
            ("5 + false", "Type mismatch: INTEGER + BOOLEAN"),
            ("5 * false", "Type mismatch: INTEGER * BOOLEAN"),
            ("5 / false", "Type mismatch: INTEGER / BOOLEAN"),
        ]);
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

    fn expect_errors(tests: Vec<(&str, &str)>) {
        for (input, expected_message) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    panic!("no error object returned. got=`{}` for `{}`", obj, input);
                }
                Err(err) => {
                    assert_eq!(&err.to_string(), expected_message, "for `{}`", input);
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
