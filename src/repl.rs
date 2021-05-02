use crate::expr;
use crate::line_reader;
use crate::parser;
use crate::scanner;
use crate::treewalk_interpreter;

use std::sync::atomic::Ordering;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");

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

fn eval_tokens(
    interpreter: &mut treewalk_interpreter::Interpreter,
    mut tokens: Vec<scanner::Token>,
    recursion_depth: i64,
) {
    let handle_err = |err| {
        println!("\nParse error: {:?}", err);
    };
    match parser::parse(tokens.clone()) {
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
                Err(err) => println!(
                    "Runtime error: {}\n\n{}",
                    err,
                    interpreter.format_backtrace()
                ),
            }
        }
        Err(
            err
            @
            parser::Error::TokenMismatch {
                expected: scanner::TokenType::Semicolon,
                found:
                    scanner::Token {
                        ty: scanner::TokenType::Eof,
                        ..
                    },
                ..
            },
        ) => {
            let expected_eof = tokens.pop().unwrap();

            tokens.push(scanner::Token {
                ty: scanner::TokenType::Semicolon,
                lexeme: Vec::new(),
                literal: None,
                line: 0,
                col: -1,
            });
            tokens.push(expected_eof);

            if recursion_depth > 0 {
                handle_err(err)
            } else {
                eval_tokens(interpreter, tokens, recursion_depth + 1)
            }
        }
        Err(err) => handle_err(err),
    }
}

pub fn run() {
    let mut interpreter = mk_interpreter();
    let mut line_reader = line_reader::LineReader::new(".repl-history.txt", ">>> ");
    println!(
        "===================================================\n\
         Welcome to lox {}! Using tree-walk interpreter.\n\
         \n\
         Authors: {}\n\
         ===================================================\n",
        VERSION, AUTHORS
    );

    loop {
        let readline = line_reader.readline();

        match readline {
            line_reader::LineReadStatus::Line(line) => match scanner::scan_tokens(line) {
                Ok(tokens) => eval_tokens(&mut interpreter, tokens, 0),
                Err(err) => {
                    println!("\nScanner error: {}", err);
                }
            },
            line_reader::LineReadStatus::Done => break,
        }
    }
}
