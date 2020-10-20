extern crate clap;

use clap::{App, Arg};

use std::fs;
use std::io::{self, BufRead, Write};

mod builtins;
mod bytecode;
mod bytecode_interpreter;
mod compiler;
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

fn debug(func: bytecode::Function, input: String) {
    let lines: Vec<_> = input.lines().collect();
    let mut interpreter = bytecode_interpreter::Interpreter::default();
    interpreter.prepare_interpret(func);
    loop {
        if interpreter.is_done() {
            println!("loxdb => done");
        }
        print!("loxdb => ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        let stdin = io::stdin();
        stdin
            .lock()
            .read_line(&mut line)
            .expect("Could not read line");
        let line = line.trim();
        match line {
            "dis" => {
                let func = &interpreter.frame().closure.function;
                bytecode_interpreter::disassemble_chunk(&func.chunk, &func.name)
            }
            "op" => {
                let frame = interpreter.frame();
                println!("{:?}", frame.closure.function.chunk.code[frame.ip].0);
            }
            "step" | "s" => match interpreter.step() {
                Ok(()) => {}
                Err(err) => println!("{}", err),
            },
            "quit" | "q" => {
                break;
            }
            "stack" => {
                for val in interpreter.stack.iter().rev() {
                    println!("{}", interpreter.format_val(&val));
                }
            }
            "globals" => {
                for (name, val) in &interpreter.globals {
                    println!("{}: {}", name, interpreter.format_val(&val));
                }
            }
            "upvals" => {
                for val in &interpreter.upvalues {
                    println!("{}", interpreter.format_upval(&*val.borrow()));
                }
            }
            "list" => println!("{}", lines[interpreter.line]),
            _ => println!("\nunknown command"),
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
                .required(true)
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
                    match scanner::scan_tokens(input.clone()) {
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
                                        Ok(output) => {
                                            println!("{}", output);
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
                            bytecode_interpreter::disassemble_chunk(&func.chunk, input_file);
                        }
                        if matches.is_present(DEBUG_STR) {
                            debug(func, input);
                            std::process::exit(0);
                        }
                        let res = bytecode_interpreter::Interpreter::default().interpret(func);
                        match res {
                            Ok(()) => {
                                std::process::exit(0);
                            }
                            Err(err) => {
                                println!("{:?}", err);
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
    }
}
