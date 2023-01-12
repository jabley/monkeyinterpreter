use crate::ast::{
    BlockStatement, Expression, HashLiteral, InfixOperator, PrefixOperator, Program, Statement,
};
use crate::object::builtins;
use crate::object::environment::Environment;
use crate::object::{EvalError, EvalResult, HashKey, Object};

use indexmap::IndexMap;
use std::rc::Rc;

pub fn eval(program: &Program, env: &mut Environment) -> EvalResult {
    let mut result = Rc::new(Object::Null);

    for statement in &program.statements {
        result = eval_statement(statement, env)?;

        if let Object::Return(value) = &*result.clone() {
            return Ok(value.clone());
        }
    }

    Ok(result)
}

fn eval_block_statement(block: &BlockStatement, env: &mut Environment) -> EvalResult {
    let mut res = Rc::new(Object::Null);

    for statement in &block.statements {
        res = eval_statement(statement, env)?;

        if let Object::Return(_) = &*res {
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
            Ok(Rc::new(Object::Return(result)))
        }
        None => Ok(Rc::new(Object::Return(Rc::new(Object::Null)))),
    }
}

fn eval_expression(expression: &Expression, env: &mut Environment) -> EvalResult {
    match expression {
        Expression::IntegerLiteral(v) => Ok(Rc::new(Object::Integer(*v))),
        Expression::Boolean(b) => Ok(Rc::new(Object::Boolean(*b))),
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
            apply_function(&function, &arguments)
        }
        Expression::StringLiteral(s) => Ok(Rc::new(Object::String(s.to_string()))),
        Expression::ArrayLiteral(elements) => eval_array_literal(elements, env),
        Expression::IndexExpression(left, index) => eval_index_expression(left, index, env),
        Expression::HashLiteral(hash) => eval_hash_literal(hash, env),
    }
}

fn eval_index_expression(
    left: &Expression,
    index: &Expression,
    env: &mut Environment,
) -> EvalResult {
    let left_value = eval_expression(left, env)?;
    let index_value = eval_expression(index, env)?;

    match (&*left_value, &*index_value) {
        (Object::Array(elements), Object::Integer(i)) => Ok(elements
            .get(*i as usize)
            .cloned()
            .unwrap_or_else(|| Rc::new(Object::Null))),
        (Object::Hash(map), key_ish) => eval_hash_index_expression(map, key_ish),
        (l, i) => Err(EvalError::UnsupportedIndexOperator(
            Rc::new(l.clone()),
            Rc::new(i.clone()),
        )),
    }
}

fn eval_hash_index_expression(
    map: &Rc<IndexMap<HashKey, Rc<Object>>>,
    key_ish: &Object,
) -> EvalResult {
    let key = HashKey::from_object(key_ish)?;
    match map.get(&key) {
        Some(v) => Ok(v.clone()),
        None => Ok(Rc::new(Object::Null)),
    }
}

fn apply_function(function: &Object, arguments: &[Rc<Object>]) -> EvalResult {
    match function {
        Object::Function(params, body, env) => {
            let mut new_env = extend_function_env(params, arguments, env);
            let evaluated = eval_block_statement(body, &mut new_env)?;
            unwrap_return_value(evaluated)
        }
        Object::BuiltIn(func) => func.apply(arguments),
        _ => Err(EvalError::NotCallable(function.clone())),
    }
}

fn unwrap_return_value(obj: Rc<Object>) -> EvalResult {
    match &*obj {
        Object::Return(value) => Ok(value.clone()),
        _ => Ok(obj),
    }
}

fn extend_function_env(
    params: &[String],
    arguments: &[Rc<Object>],
    env: &Environment,
) -> Environment {
    let mut result = env.extend();

    for (i, p) in params.iter().enumerate() {
        let arg = arguments
            .get(i)
            .cloned()
            .unwrap_or_else(|| Rc::new(Object::Null));
        result.set(p, arg);
    }

    result
}

fn eval_array_literal(elements: &[Expression], env: &mut Environment) -> EvalResult {
    let values = eval_expressions(elements, env)?;
    Ok(Rc::new(Object::Array(Rc::new(values))))
}

fn eval_hash_literal(hash: &HashLiteral, env: &mut Environment) -> EvalResult {
    let mut map = IndexMap::new();

    for (k, v) in &hash.pairs {
        let key = eval_expression(k, env)?;
        let value = eval_expression(v, env)?;
        let hash_key = HashKey::from_object(&key)?;
        map.insert(hash_key, value);
    }

    Ok(Rc::new(Object::Hash(Rc::new(map))))
}

fn eval_expressions(
    exps: &[Expression],
    env: &mut Environment,
) -> Result<Vec<Rc<Object>>, EvalError> {
    let mut results = vec![];

    for exp in exps {
        results.push(eval_expression(exp, env)?);
    }

    Ok(results)
}

fn eval_function(
    parameters: &[String],
    body: &BlockStatement,
    env: &mut Environment,
) -> EvalResult {
    Ok(Rc::new(Object::Function(
        parameters.to_owned(),
        body.clone(),
        env.clone(),
    )))
}

fn eval_identifier(name: &str, env: &Environment) -> EvalResult {
    if let Some(r) = env.get(name) {
        return Ok(r);
    }

    if let Some(r) = builtins::BuiltIn::lookup(name) {
        return Ok(Rc::new(r));
    }

    Err(EvalError::IdentifierNotFound(name.to_string()))
}

fn eval_prefix_expression(
    operator: &PrefixOperator,
    expression: &Expression,
    env: &mut Environment,
) -> EvalResult {
    let obj = eval_expression(expression, env)?;

    match operator {
        PrefixOperator::Bang => Ok(Rc::new(Object::Boolean(!obj.is_truthy()))),
        PrefixOperator::Minus => match *obj {
            Object::Integer(v) => Ok(Rc::new(Object::Integer(-v))),
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

    match (&*left_obj, &*right_obj) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expressions(operator, *left, *right)
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expressions(operator, *left, *right)
        }
        (Object::String(left), Object::String(right)) => {
            eval_string_infix_expression(operator, left.clone(), right)
        }
        (left, right) => Err(EvalError::TypeMismatch(
            operator.clone(),
            Rc::new(left.clone()),
            Rc::new(right.clone()),
        )),
    }
}

fn eval_integer_infix_expressions(operator: &InfixOperator, left: i64, right: i64) -> EvalResult {
    match operator {
        InfixOperator::Eq => Ok(Rc::new(Object::Boolean(left == right))),
        InfixOperator::NotEq => Ok(Rc::new(Object::Boolean(left != right))),
        InfixOperator::Lt => Ok(Rc::new(Object::Boolean(left < right))),
        InfixOperator::Gt => Ok(Rc::new(Object::Boolean(left > right))),
        InfixOperator::Plus => Ok(Rc::new(Object::Integer(left + right))),
        InfixOperator::Minus => Ok(Rc::new(Object::Integer(left - right))),
        InfixOperator::Asterisk => Ok(Rc::new(Object::Integer(left * right))),
        InfixOperator::Slash => Ok(Rc::new(Object::Integer(left / right))),
    }
}

fn eval_boolean_infix_expressions(operator: &InfixOperator, left: bool, right: bool) -> EvalResult {
    match operator {
        InfixOperator::Eq => Ok(Rc::new(Object::Boolean(left == right))),
        InfixOperator::NotEq => Ok(Rc::new(Object::Boolean(left != right))),
        _ => Err(EvalError::UnsupportedInfixOperator(
            operator.clone(),
            Object::Boolean(left),
            Object::Boolean(right),
        )),
    }
}

fn eval_string_infix_expression(operator: &InfixOperator, left: String, right: &str) -> EvalResult {
    match operator {
        InfixOperator::Plus => Ok(Rc::new(Object::String(left + right))),
        _ => Err(EvalError::UnsupportedInfixOperator(
            operator.clone(),
            Object::String(left),
            Object::String(String::from(right)),
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
        return eval_block_statement(consequence, env);
    }

    if let Some(alt) = alternative {
        return eval_block_statement(alt, env);
    }

    Ok(Rc::new(Object::Null))
}

#[cfg(test)]
mod tests {
    use crate::evaluator;
    use crate::evaluator::EvalResult;
    use crate::lexer::Lexer;
    use crate::object::environment::Environment;
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
            (r#""Hello" - "World""#, "Unknown operator: STRING - STRING"),
            (
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                "Unusable as hash key: FUNCTION",
            ),
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

    #[test]
    fn string_literal() {
        expect_values(vec![(r#""Hello world!";"#, "Hello world!")]);
    }

    #[test]
    fn string_concatenation() {
        expect_values(vec![(r#""Hello" + " " + "World!""#, "Hello World!")]);
    }

    #[test]
    fn builtin_functions() {
        expect_values(vec![
            (r#"len("")"#, "0"),
            (r#"len("four")"#, "4"),
            (r#"len("hello world")"#, "11"),
            ("len([1, 2, 3])", "3"),
            ("len([])", "0"),
            ("first([1, 2, 3])", "1"),
            ("first([])", "null"),
            ("last([1, 2, 3])", "3"),
            ("last([])", "null"),
            ("rest([1, 2, 3])", "[2, 3]"),
            ("rest([])", "null"),
            ("push([], 1)", "[1]"),
        ]);
        expect_errors(vec![
            (r#"len(1)"#, "argument to `len` not supported, got INTEGER"),
            (
                r#"len("one", "two")"#,
                "wrong number of arguments. got=2, want=1",
            ),
            ("first(1)", "argument to `first` not supported, got INTEGER"),
            ("last(1)", "argument to `last` not supported, got INTEGER"),
            (
                "push(1, 1)",
                "argument to `push` not supported, got INTEGER, INTEGER",
            ),
        ]);
    }

    #[test]
    fn array_literals() {
        expect_values(vec![
            ("[1, 2 * 2, 3 + 3]", "[1, 4, 6]"),
            ("[1, 2 * 2, 3 + 3][0]", "1"),
            ("[1, 2 * 2, 3 + 3][1]", "4"),
            ("[1, 2 * 2, 3 + 3][2]", "6"),
        ])
    }

    #[test]
    fn array_index_expressions() {
        expect_values(vec![
            ("[1, 2 * 2, 3 + 3][0]", "1"),
            ("[1, 2 * 2, 3 + 3][1]", "4"),
            ("[1, 2 * 2, 3 + 3][2]", "6"),
            ("[1, 2, 3][0]", "1"),
            ("[1, 2, 3][1]", "2"),
            ("[1, 2, 3][2]", "3"),
            ("let i = 0; [1][i];", "1"),
            ("[1, 2, 3][1 + 1];", "3"),
            ("let myArray = [1, 2, 3]; myArray[2];", "3"),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                "6",
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                "2",
            ),
            ("[1, 2, 3][3]", "null"),
            ("[1, 2, 3][-1]", "null"),
        ]);
        expect_errors(vec![(
            "[1, 2, 3][[1]]",
            "index operator not supported: [1, 2, 3][[1]]",
        )]);
    }

    #[test]
    fn hash_literals() {
        expect_values(vec![(
            r#"let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }"#,
            r#"{"one": 1, "two": 2, "three": 3, 4: 4, true: 5, false: 6}"#,
        )]);
    }

    #[test]
    fn hash_index_expression() {
        expect_values(vec![
            (r#"{"foo": 5}["foo"]"#, "5"),
            (r#"{"foo": 5}["bar"]"#, "null"),
            (r#"let key = "foo"; {"foo": 5}[key]"#, "5"),
            (r#"{}["foo"]"#, "null"),
            (r#"{5: 5}[5]"#, "5"),
            (r#"{true: 5}[true]"#, "5"),
            (r#"{false: 5}[false]"#, "5"),
        ]);
    }

    #[test]
    fn fib() {
        expect_values(vec![(
            "
        let fibonacci = fn(x) {
            if (x == 0) {
                0
            } else {
                if (x == 1) {
                    return 1;
                } else {
                    fibonacci(x - 1) + fibonacci(x - 2);
                }
            }
        };
        fibonacci(10);
        ",
            "55",
        )])
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
