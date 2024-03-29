use rmonkey::{ast::Program, compiler::Compiler, lexer::Lexer, parser::Parser, vm::VM};

fn main() {
    let program = parse();

    let mut c = Compiler::new();

    match c.compile(&program) {
        Ok(bytecode) => {
            let mut vm = VM::new(bytecode.constants, bytecode.instructions.to_vec());

            match vm.run() {
                Ok(obj) => println!("Result: {}", obj),
                Err(e) => println!("Unexpected error: {}", e),
            }
        }
        Err(e) => println!("Unexpected error: {}", e),
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
    fibonacci(35);
    ",
    );
    let mut parser = Parser::new(lexer);
    parser.parse_program()
}
