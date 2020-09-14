use rmonkey::{ast::Program, evaluator, lexer::Lexer, object::Environment, parser::Parser};

fn main() {
    let program = parse();

    let mut env = Environment::new();

    match evaluator::eval(&program, &mut env) {
        Ok(obj) => println!("Result: {}", obj.to_string()),
        Err(e) => println!("Unexpected error: {}", e.to_string()),
    }
}

fn parse() -> Program {
    let lexer = Lexer::new(
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
    fibonacci(33);
    ",
    );
    let mut parser = Parser::new(lexer);
    parser.parse_program()
}
