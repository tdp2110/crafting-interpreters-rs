use crate::expr;
use crate::parser;
use crate::scanner;
use crate::treewalk_interpreter;

use std::io::{self, BufRead, Write};
use std::sync::atomic::Ordering;

pub fn run() {
    let mut interpreter: treewalk_interpreter::Interpreter = Default::default();
    println!(
        "============================================\n\
         Welcome to lox! using tree-walk interpreter.\n\
         ============================================\n"
    );

    {
        let interrupt_clone = interpreter.interrupted.clone();
        ctrlc::set_handler(move || {
            interrupt_clone.store(true, Ordering::Release);
        })
        .expect("Error setting Ctrl-C handler");
    }

    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        let stdin = io::stdin();
        let nchar = stdin
            .lock()
            .read_line(&mut line)
            .expect("Could not read line");
        if nchar == 0 {
            break;
        }

        match scanner::scan_tokens(line) {
            Ok(tokens) => match parser::parse(tokens) {
                Ok(stmts) => {
                    let stmts2: Vec<expr::Stmt> = stmts
                        .iter()
                        .map(|stmt| match stmt {
                            expr::Stmt::Expr(expr) => expr::Stmt::Print(expr.clone()),
                            _ => stmt.clone(),
                        })
                        .collect();
                    match interpreter.interpret(&stmts2) {
                        Ok(()) => {}
                        Err(err) => println!("Runtime error: {}", err),
                    }
                }
                Err(err) => println!("\nParse error: {}", err),
            },
            Err(err) => {
                println!("\nScanner error: {}", err);
            }
        }
    }
}
