pub mod ast;
pub mod code;
pub mod compiler;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod repl;
pub mod token;
pub mod vm;

fn main() {
    repl::run();
}
