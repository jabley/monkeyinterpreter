use std::io;
use std::io::BufRead;
use std::io::Write;

use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::VM;

pub fn run() {
    let stdin = io::stdin();

    loop {
        print!(">> ");
        io::stdout().flush().expect("Error flushing stdout");

        let mut line = String::new();
        stdin
            .lock()
            .read_line(&mut line)
            .expect("Error reading from stdin");
        let lexer = Lexer::new(&line);

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            print_parser_errors(parser);
            continue;
        }

        let mut compiler = Compiler::new();
        let bytecode = match compiler.compile(&program) {
            Ok(bytecode) => bytecode,
            Err(err) => {
                println!("Whoops! Compilation failed:\n{}\n", err);
                continue;
            }
        };

        let mut vm = VM::new(bytecode);

        match vm.run() {
            Ok(result) => println!("{}", result),
            Err(err) => println!("Whoops! Executing bytecode failed:\n{}\n", err),
        }
    }
}

fn print_parser_errors(parser: Parser) {
    println!(
        r#"            __,__
.--.  .-"     "-.  .--.
/ .. \/  .-. .-.  \/ .. \
| |  '|  /   Y   \  |'  | |
| \   \  \ 0 | 0 /  /   / |
\ '- ,\.-"""""""-./, -' /
''-' /_   ^ ^   _\ '-''
   |  \._   _./  |
   \   \ '~' /   /
    '._ '-=-' _.'
       '-----'
"#
    );
    println!("Woops! We ran into some monkey business here!");
    for error in parser.errors() {
        println!("\t{:?}", error);
    }
}
