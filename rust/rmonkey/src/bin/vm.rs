use std::{fs::File, io::Write};

use clap::{App, Arg};
use pprof;
use pprof::protos::Message;
use rmonkey::{ast::Program, compiler::Compiler, lexer::Lexer, parser::Parser, vm::VM};

fn main() {
    let matches = App::new("VM example")
        .arg(
            Arg::with_name("cpuprofile")
                .short("p")
                .long("cpuprofile")
                .help("Optional flag indicating whether to save pprof CPU profile")
                .takes_value(true),
        )
        .get_matches();

    let guard = match matches.value_of("cpuprofile") {
        Some(_) => pprof::ProfilerGuard::new(97).ok(),
        None => None,
    };

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

    guard.and_then(|guard| -> Option<()> {
        if let Ok(report) = guard.report().build() {
            let mut file = File::create(matches.value_of("cpuprofile").unwrap()).unwrap();
            let profile = report.pprof().unwrap();

            let mut content = Vec::new();
            profile.encode(&mut content).unwrap();
            file.write_all(&content).unwrap();

            println!("report: {}", &report);
        };

        None
    });
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
