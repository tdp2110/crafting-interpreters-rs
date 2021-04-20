use crate::expr;
use crate::line_reader;
use crate::parser;
use crate::scanner;
use crate::treewalk_interpreter;

use std::sync::atomic::Ordering;

fn mk_interpreter() -> treewalk_interpreter::Interpreter {
    let interpreter: treewalk_interpreter::Interpreter = Default::default();

    {
        let interrupt_clone = interpreter.interrupted.clone();
        ctrlc::set_handler(move || {
            interrupt_clone.store(true, Ordering::Release);
        })
        .expect("Error setting Ctrl-C handler");
    }

    interpreter
}

pub fn run() {
    let mut interpreter = mk_interpreter();
    let mut line_reader = line_reader::LineReader::new(".repl-history.txt", ">>> ");
    println!(
        "============================================\n\
         Welcome to lox! using tree-walk interpreter.\n\
         ============================================\n"
    );

    loop {
        let readline = line_reader.readline();

        match readline {
            line_reader::LineReadStatus::Line(line) => match scanner::scan_tokens(line) {
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
            },
            line_reader::LineReadStatus::Done => break,
        }
    }
}
