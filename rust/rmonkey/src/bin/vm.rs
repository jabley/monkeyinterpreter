use rmonkey::{ast::Program, compiler::Compiler, lexer::Lexer, parser::Parser, vm::VM};

fn main() {
    let program = parse();

    let mut c = Compiler::new();

    match c.compile(&program) {
        Ok(bytecode) => {
            let mut vm = VM::new(bytecode);

            match vm.run() {
                Ok(obj) => println!("Result: {}", obj.to_string()),
                Err(e) => println!("Unexpected error: {}", e.to_string()),
            }
        }
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
    fibonacci(36);
    ",
    );
    let mut parser = Parser::new(lexer);
    parser.parse_program()
}
