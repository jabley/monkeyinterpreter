use crate::ast::{BlockStatement, Expression, InfixOperator, PrefixOperator, Program, Statement};
use crate::environment::Environment;
use crate::object::Object;
use std::fmt;

type EvalResult = std::result::Result<Object, EvalError>;

pub enum EvalError {
    IdentifierNotFound(String),
    NotCallable(Object),
    UnsupportedInfixOperator(InfixOperator, Object, Object),
    UnsupportedPrefixOperator(PrefixOperator, Object),
    TypeMismatch(InfixOperator, Object, Object),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::UnsupportedPrefixOperator(operator, obj) => {
                write!(f, "Unknown operator: {}{}", operator, obj.type_name())
            }
            EvalError::TypeMismatch(operator, left, right) => write!(
                f,
                "Type mismatch: {} {} {}",
                left.type_name(),
                operator,
                right.type_name()
            ),
            EvalError::UnsupportedInfixOperator(operator, left, right) => write!(
                f,
                "Unknown operator: {} {} {}",
                left.type_name(),
                operator,
                right.type_name()
            ),
            EvalError::IdentifierNotFound(name) => write!(f, "Identifier not found: {}", name),
            EvalError::NotCallable(obj) => write!(f, "Not a function: {}", obj),
        }
    }
}

pub fn eval(program: &Program, env: &mut Environment) -> EvalResult {
    let mut result = Object::Null;

    for statement in &program.statements {
        result = eval_statement(statement, env)?;

        if let Object::Return(value) = result {
            return Ok(*value);
        }
    }

    Ok(result)
}

fn eval_block_statement(block: &BlockStatement, env: &mut Environment) -> EvalResult {
    let mut res = Object::Null;

    for statement in &block.statements {
        res = eval_statement(statement, env)?;

        if let Object::Return(_) = res {
            // Don't unwrap it here - want to propagate it back up the call stack
            return Ok(res);
        }
    }

    Ok(res)
}

fn eval_statement(statement: &Statement, env: &mut Environment) -> EvalResult {
    match statement {
        Statement::Expression(exp) => eval_expression(exp, env),
        Statement::Return(exp) => eval_return_expression(exp, env),
        Statement::Let(name, expression) => {
            let result = eval_expression(expression, env)?;
            env.set(name, result.clone());
            Ok(result)
        }
    }
}

fn eval_return_expression(possibility: &Option<Expression>, env: &mut Environment) -> EvalResult {
    match possibility {
        Some(expression) => {
            let result = eval_expression(expression, env)?;
            Ok(Object::Return(Box::new(result)))
        }
        None => Ok(Object::Return(Box::new(Object::Null))),
    }
}

fn eval_expression(expression: &Expression, env: &mut Environment) -> EvalResult {
    match expression {
        Expression::IntegerLiteral(v) => Ok(Object::Integer(*v)),
        Expression::Boolean(b) => Ok(Object::Boolean(*b)),
        Expression::Prefix(operator, expression) => {
            eval_prefix_expression(operator, expression, env)
        }
        Expression::Infix(operator, left, right) => {
            eval_infix_expression(operator, left, right, env)
        }
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(condition, consequence, alternative, env)
        }
        Expression::Identifier(name) => eval_identifier(name, env),
        Expression::FunctionLiteral(parameters, body) => eval_function(parameters, body, env),
        Expression::Call(function_exp, arg_expressions) => {
            let function = eval_expression(function_exp, env)?;
            let arguments = eval_expressions(arg_expressions, env)?;
            apply_function(&function, arguments)
        }
    }
}

fn apply_function(function: &Object, arguments: Vec<Object>) -> EvalResult {
    match function {
        Object::Function(params, body, env) => {
            let mut new_env = extend_function_env(params, arguments, env);
            let evaluated = eval_block_statement(&body, &mut new_env)?;
            unwrap_return_value(evaluated)
        }
        _ => Err(EvalError::NotCallable(function.clone())),
    }
}

fn unwrap_return_value(obj: Object) -> EvalResult {
    match obj {
        Object::Return(value) => Ok(*value),
        _ => Ok(obj),
    }
}

fn extend_function_env(
    params: &[String],
    arguments: Vec<Object>,
    env: &Environment,
) -> Environment {
    let mut result = Environment::extend(env);

    for (i, p) in params.iter().enumerate() {
        let arg = arguments.get(i).cloned().unwrap_or(Object::Null);
        result.set(p, arg);
    }

    result
}

fn eval_expressions(exps: &[Expression], env: &mut Environment) -> Result<Vec<Object>, EvalError> {
    let mut results = vec![];

    for exp in exps {
        results.push(eval_expression(exp, env)?);
    }

    Ok(results)
}

fn eval_function(parameters: &[String], body: &BlockStatement, env: &Environment) -> EvalResult {
    Ok(Object::Function(
        parameters.to_owned(),
        body.clone(),
        env.clone(),
    ))
}

fn eval_identifier(name: &str, env: &mut Environment) -> EvalResult {
    env.get(name)
        .ok_or_else(|| EvalError::IdentifierNotFound(name.to_string()))
}

fn eval_prefix_expression(
    operator: &PrefixOperator,
    expression: &Expression,
    env: &mut Environment,
) -> EvalResult {
    let obj = eval_expression(expression, env)?;

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
    env: &mut Environment,
) -> EvalResult {
    let left_obj = eval_expression(left_exp, env)?;
    let right_obj = eval_expression(right_exp, env)?;

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

fn eval_if_expression(
    condition: &Expression,
    consequence: &BlockStatement,
    alternative: &Option<BlockStatement>,
    env: &mut Environment,
) -> EvalResult {
    let test = eval_expression(condition, env)?;

    if test.is_truthy() {
        return eval_block_statement(&consequence, env);
    }

    if let Some(alt) = alternative {
        return eval_block_statement(&alt, env);
    }

    Ok(Object::Null)
}

#[cfg(test)]
mod tests {
    use crate::environment::Environment;
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
    fn eval_error_handling() {
        expect_errors(vec![
            ("-true", "Unknown operator: -BOOLEAN"),
            ("-false", "Unknown operator: -BOOLEAN"),
            ("5 - false", "Type mismatch: INTEGER - BOOLEAN"),
            ("5 + false", "Type mismatch: INTEGER + BOOLEAN"),
            ("5 + true;", "Type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "Type mismatch: INTEGER + BOOLEAN"),
            ("5 * false", "Type mismatch: INTEGER * BOOLEAN"),
            ("5 / false", "Type mismatch: INTEGER / BOOLEAN"),
            ("true + false", "Unknown operator: BOOLEAN + BOOLEAN"),
            (
                "true + false + true + false;",
                "Unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("5; true + false; 5", "Unknown operator: BOOLEAN + BOOLEAN"),
            (
                "
            if (10 > 1) {
                if (10 > 1) {
                  return true + false;
                }
              
                return 1;
              }
            ",
                "Unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "Identifier not found: foobar"),
            ("let x = 5; x();", "Not a function: 5"),
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

    #[test]
    fn eval_if() {
        expect_values(vec![
            ("if (true) { 10 }", "10"),
            ("if (false) { 10 }", "null"),
            ("if (1) { 10", "10"),
            ("if (1 < 2) { 10 }", "10"),
            ("if (1 > 2) { 10 }", "null"),
            ("if (1 < 2) { 10 } else { 20 }", "10"),
            ("if (1 > 2) { 10 } else { 20 }", "20"),
        ]);
    }

    #[test]
    fn eval_return() {
        expect_values(vec![
            ("return 10;", "10"),
            ("return 10; 9;", "10"),
            ("return 2 * 5; 9;", "10"),
            ("9; return 2 * 5; 9;", "10"),
            ("if (10 > 1) { return 10; }", "10"),
            (
                "
if (10 > 1) {
  if (10 > 1) {
    return 10;
  }

  return 1;
}
",
                "10",
            ),
        ]);
    }

    #[test]
    fn let_statements() {
        expect_values(vec![
            ("let a = 5; a;", "5"),
            ("let a = 5 * 5; a;", "25"),
            ("let a = 5; let b = a; b;", "5"),
            ("let a = 5; let b = a; let c = a + b + 5; c;", "15"),
        ]);
    }

    #[test]
    fn fn_expressions() {
        expect_values(vec![("fn(x) { x + 2; }", "fn(x) {\n{ (x + 2); }\n}")]);
    }

    #[test]
    fn fn_application() {
        expect_values(vec![
            ("let identity = fn(x) { x; }; identity(5);", "5"),
            ("let identity = fn(x) { return x; }; identity(5);", "5"),
            ("let double = fn(x) { x * 2; }; double(5);", "10"),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", "10"),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                "20",
            ),
            ("fn(x) { x; }(5)", "5"),
        ]);
    }

    #[test]
    fn function_with_closures() {
        expect_values(vec![(
            r#"
let newAdder = fn(x) {
    fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);
"#,
            "4",
        )]);
    }

    fn expect_values(tests: Vec<(&str, &str)>) {
        for (input, expected) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    assert_eq!(expected.to_string(), obj.to_string(), "for `{}`", input);
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
                    assert_eq!(expected_message, &err.to_string(), "for `{}`", input);
                }
            }
        }
    }

    fn eval_input(input: &str) -> EvalResult {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let mut env = Environment::new();
        evaluator::eval(&program, &mut env)
    }
}
