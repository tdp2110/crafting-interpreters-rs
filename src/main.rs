extern crate clap;
extern crate ctrlc;

use clap::{App, Arg};

use std::fs;
use std::io::{self, BufRead, Write};
use std::sync::atomic::Ordering;

mod builtins;
mod bytecode;
mod bytecode_interpreter;
mod compiler;
mod debugger;
mod expr;
mod gc;
mod parser;
mod scanner;
mod treewalk_interpreter;
mod value;

static INPUT_STR: &str = "INPUT";
static SHOW_TOKENS_STR: &str = "tokens";
static SHOW_AST_STR: &str = "ast";
static DISASSEMBLE_STR: &str = "disassemble";
static DEBUG_STR: &str = "debug";
static TREEWALK_STR: &str = "treewalk";

fn run_repl() {
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

fn main() {
    let matches = App::new("loxi")
        .version("0.1.0")
        .about("lox language interpreter")
        .author("Thomas Peters")
        .arg(
            Arg::with_name(INPUT_STR)
                .help("sets input file to use")
                .required(false)
                .index(1),
        )
        .arg(
            Arg::with_name(SHOW_TOKENS_STR)
                .long("--show-tokens")
                .takes_value(false)
                .help("show the token stream"),
        )
        .arg(
            Arg::with_name(SHOW_AST_STR)
                .long("--show-ast")
                .takes_value(false)
                .help("show the AST"),
        )
        .arg(
            Arg::with_name(DISASSEMBLE_STR)
                .long("--disassemble")
                .takes_value(false)
                .help("show the bytecode"),
        )
        .arg(
            Arg::with_name(DEBUG_STR)
                .long("--debug")
                .takes_value(false)
                .help("run in the debugger"),
        )
        .arg(
            Arg::with_name(TREEWALK_STR)
                .long("--treewalk")
                .takes_value(false)
                .help("run the tree-walk interpreter instead of the bytecode interpreter"),
        )
        .get_matches();

    if let Some(input_file) = matches.value_of(INPUT_STR) {
        let maybe_input = fs::read_to_string(input_file);

        match maybe_input {
            Ok(input) => {
                if matches.is_present(SHOW_TOKENS_STR)
                    || matches.is_present(SHOW_AST_STR)
                    || matches.is_present(TREEWALK_STR)
                {
                    match scanner::scan_tokens(input) {
                        Ok(tokens) => {
                            if matches.is_present(SHOW_TOKENS_STR) {
                                println!("tokens: {:#?}", tokens);
                                std::process::exit(0);
                            }

                            let stmts_maybe = parser::parse(tokens);

                            match stmts_maybe {
                                Ok(stmts) => {
                                    if matches.is_present(SHOW_AST_STR) {
                                        println!("AST: {:#?}", stmts);
                                        std::process::exit(0);
                                    }

                                    let interpret_result = treewalk_interpreter::interpret(&stmts);

                                    match interpret_result {
                                        Ok(_) => {
                                            std::process::exit(0);
                                        }
                                        Err(err) => {
                                            println!("Treewalk Interpreter Error: {}", err);
                                            std::process::exit(-1);
                                        }
                                    }
                                }
                                Err(err) => {
                                    println!("parse error: {}", err);
                                    std::process::exit(-1)
                                }
                            }
                        }
                        Err(err) => {
                            println!("lexical error: {}", err);
                            std::process::exit(-1);
                        }
                    }
                }

                let func_or_err = compiler::Compiler::compile(input.clone());

                match func_or_err {
                    Ok(func) => {
                        if matches.is_present(DISASSEMBLE_STR) {
                            println!(
                                "{}",
                                bytecode_interpreter::disassemble_chunk(&func.chunk, input_file)
                            );
                            std::process::exit(0);
                        }
                        if matches.is_present(DEBUG_STR) {
                            debugger::Debugger::new(func, input).debug();
                            std::process::exit(0);
                        }
                        let mut interpreter = bytecode_interpreter::Interpreter::default();
                        let res = interpreter.interpret(func);
                        match res {
                            Ok(()) => {
                                std::process::exit(0);
                            }
                            Err(bytecode_interpreter::InterpreterError::Runtime(err)) => {
                                println!(
                                    "Runtime error: {}\n\n{}",
                                    err,
                                    interpreter.format_backtrace()
                                );

                                std::process::exit(1);
                            }
                        }
                    }
                    Err(err) => {
                        println!("{}", err);
                        std::process::exit(1);
                    }
                }
            }
            Err(err) => {
                println!("Error reading {}: {}", input_file, err);
                std::process::exit(-1);
            }
        }
    } else {
        run_repl();
    }
}
