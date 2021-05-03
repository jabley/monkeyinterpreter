use crate::object::Object;
use crate::vm::new_globals;
use std::io;
use std::io::BufRead;
use std::io::Write;

use crate::compiler::{Compiler, SymbolTable};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::VM;
use std::rc::Rc;

pub fn run() {
    let stdin = io::stdin();

    let mut constants: Vec<Rc<Object>> = vec![];
    let mut globals = new_globals();
    let mut symbol_table = SymbolTable::new_with_builtins();

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

        let mut compiler = Compiler::new_with_state(symbol_table, constants);

        match compiler.compile(&program) {
            Ok(_) => {
                let mut vm = VM::new_with_globals_store(
                    &compiler.constants,
                    compiler.current_instructions().clone(),
                    globals,
                );

                match vm.run() {
                    Ok(result) => println!("{}", result),
                    Err(err) => println!("Whoops! Executing bytecode failed:\n{}\n", err),
                }

                globals = vm.globals;
            }
            Err(err) => println!("Whoops! Compilation failed:\n{}\n", err),
        };

        symbol_table = compiler.symbol_table;
        constants = compiler.constants;
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
