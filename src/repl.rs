use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::expr;
use crate::parser;
use crate::scanner;
use crate::treewalk_interpreter;

use std::sync::atomic::Ordering;

static HISTORY_FILE: &str = ".repl-history.txt";

pub fn run() {
    let mut interpreter: treewalk_interpreter::Interpreter = Default::default();
    let mut rl = Editor::<()>::new();
    rl.load_history(HISTORY_FILE).ok();
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
        let readline = rl.readline(">> ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
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
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("REPL Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history(HISTORY_FILE).unwrap();
}
