pub mod lexer;
pub mod token;

use std::io;
use std::io::BufRead;
use std::io::Write;

use lexer::Lexer;
use token::Token;

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
        let mut lexer = Lexer::new(&line);

        loop {
            let tok = lexer.next_token();
            println!("{:?}", tok);
            if tok == Token::Eof {
                break;
            }
        }
    }
}
