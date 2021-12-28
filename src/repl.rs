use crate::error_formatting;
use crate::expr;
use crate::line_reader;
use crate::parser;
use crate::scanner;
use crate::syntax_extensions;
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
    extensions: syntax_extensions::Extensions,
    line: &str,
) {
    let handle_err = |err| {
        error_formatting::format_parse_error(err, line);
    };
    match parser::parse(extensions, tokens.clone()) {
        Ok(stmts) => {
            let stmts2: Vec<expr::Stmt> = stmts
                .iter()
                .enumerate()
                .map(|(idx, stmt)| match stmt {
                    expr::Stmt::Expr(expr) => {
                        let var_sym = expr::Symbol {
                            // hack!!! we should find a fresh varname from somewhere
                            name: format!("isurehopethisisntusedelsewhere{}", idx),
                            line: 0,
                            col: 0,
                        };
                        let var_expr = expr::Expr::Variable(var_sym.clone());
                        expr::Stmt::Block(vec![
                            expr::Stmt::VarDecl(var_sym, Some(expr.clone())),
                            expr::Stmt::If(
                                expr::Expr::Binary(
                                    Box::new(var_expr.clone()),
                                    expr::BinaryOp {
                                        ty: expr::BinaryOpTy::NotEqual,
                                        line: 0,
                                        col: 0,
                                    },
                                    Box::new(expr::Expr::Literal(expr::Literal::Nil)),
                                ),
                                Box::new(expr::Stmt::Print(var_expr)),
                                None,
                            ),
                        ])
                    }
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
                handle_err(&err)
            } else {
                eval_tokens(interpreter, tokens, recursion_depth + 1, extensions, line)
            }
        }
        Err(err) => handle_err(&err),
    }
}

pub fn run(extensions: syntax_extensions::Extensions) {
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
            line_reader::LineReadStatus::Line(line) => match scanner::scan_tokens(line.clone()) {
                Ok(tokens) => eval_tokens(&mut interpreter, tokens, 0, extensions, &line),
                Err(err) => {
                    error_formatting::format_lexical_error(&err);
                }
            },
            line_reader::LineReadStatus::Done => break,
        }
    }
}
