use criterion::{criterion_group, criterion_main, Criterion};
use rmonkey::{
    ast::Program,
    compiler::Compiler,
    evaluator,
    lexer::Lexer,
    object::{Environment, Object},
    parser::Parser,
    vm::VM,
};
use std::ops::Deref;

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

fn criterion_benchmark_evaluator(c: &mut Criterion) {
    let program = parse();

    c.bench_function("fib 18 (Interpreter)", |b| {
        b.iter(|| {
            let mut env = Environment::new();

            match evaluator::eval(&program, &mut env) {
                Ok(obj) => match obj.deref() {
                    Object::Integer(2584) => {}
                    _ => println!("Unexpected result: {}", obj.to_string()),
                },
                Err(e) => println!("Unexpected error: {}", e.to_string()),
            }
        })
    });
}

fn criterion_benchmark_vm(c: &mut Criterion) {
    let program = parse();

    c.bench_function("fib 18 (VM)", |b| {
        b.iter(|| {
            let mut c = Compiler::new();

            match c.compile(&program) {
                Ok(bytecode) => {
                    let mut vm = VM::new(bytecode.constants, bytecode.instructions.to_vec());

                    match vm.run() {
                        Ok(obj) => match obj.deref() {
                            Object::Integer(2584) => {}
                            _ => println!("Unexpected result: {}", obj.to_string()),
                        },
                        Err(e) => println!("Unexpected error: {}", e.to_string()),
                    }
                }
                Err(e) => println!("Unexpected error: {}", e.to_string()),
            }
        })
    });
}

criterion_group!(
    benches,
    criterion_benchmark_evaluator,
    criterion_benchmark_vm
);
criterion_main!(benches);
