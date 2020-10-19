extern crate clap;

use clap::{App, Arg};

use std::fs;

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
static TREEWALK_STR: &str = "treewalk";

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

                let func_or_err = compiler::Compiler::compile(input);

                match func_or_err {
                    Ok(func) => {
                        if matches.is_present(DISASSEMBLE_STR) {
                            bytecode_interpreter::disassemble_chunk(&func.chunk, input_file);
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
