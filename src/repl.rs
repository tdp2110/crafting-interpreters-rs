use rustyline::error::ReadlineError;

use crate::expr;
use crate::parser;
use crate::scanner;
use crate::treewalk_interpreter;

use std::sync::atomic::Ordering;

static HISTORY_FILE: &str = ".repl-history.txt";

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

struct LineReader {
    rl: rustyline::Editor<()>,
}

impl Default for LineReader {
    fn default() -> LineReader {
        let mut rl = rustyline::Editor::<()>::new();
        rl.load_history(HISTORY_FILE).ok();
        LineReader { rl }
    }
}

impl Drop for LineReader {
    fn drop(&mut self) {
        self.rl.save_history(HISTORY_FILE).unwrap();
    }
}

impl LineReader {
    pub fn readline(&mut self) -> Result<String, rustyline::error::ReadlineError> {
        let res = self.rl.readline(">>> ");
        match &res {
            Ok(line) => {
                self.rl.add_history_entry(line.as_str());
            }
            _ => {}
        }
        res
    }
}

pub fn run() {
    let mut interpreter = mk_interpreter();
    let mut line_reader = LineReader::default();
    println!(
        "============================================\n\
         Welcome to lox! using tree-walk interpreter.\n\
         ============================================\n"
    );

    loop {
        let readline = line_reader.readline();

        match readline {
            Ok(line) => match scanner::scan_tokens(line) {
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
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("REPL Error: {:?}", err);
                break;
            }
        }
    }
}
