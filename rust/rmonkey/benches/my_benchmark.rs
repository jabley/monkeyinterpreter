use criterion::{criterion_group, criterion_main, Criterion};
use rmonkey::{
    ast::Program,
    evaluator,
    lexer::Lexer,
    object::{Environment, Object},
    parser::Parser,
};
use std::{cell::RefCell, rc::Rc};

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
    fibonacci(18);
    ",
    );
    let mut parser = Parser::new(lexer);
    parser.parse_program()
}

fn criterion_benchmark(c: &mut Criterion) {
    let program = parse();

    c.bench_function("fib 18", |b| {
        b.iter(|| {
            let env = Rc::new(RefCell::new(Environment::new()));

            match evaluator::eval(&program, env) {
                Ok(Object::Integer(2584)) => {}
                Ok(obj) => println!("Unexpected result: {}", obj.to_string()),
                Err(e) => println!("Unexpected error: {}", e.to_string()),
            }
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
