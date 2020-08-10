pub mod ast;
pub mod environment;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod token;

use std::io;
use std::io::BufRead;
use std::io::Write;

use crate::environment::Environment;
use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
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

        let mut env = Environment::new();

        match evaluator::eval(&program, &mut env) {
            Ok(evaluated) => println!("{}", evaluated),
            Err(err) => println!("ERROR: {}", err),
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
