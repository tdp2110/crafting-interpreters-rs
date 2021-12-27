extern crate clap;
extern crate ctrlc;

use clap::{App, Arg};

use std::fs;

mod builtins;
mod bytecode;
mod bytecode_interpreter;
mod compiler;
mod debugger;
mod error_formatting;
mod expr;
mod gc;
mod line_reader;
mod parser;
mod repl;
mod scanner;
mod syntax_extensions;
mod treewalk_interpreter;
mod value;

static INPUT_STR: &str = "INPUT";
static SHOW_TOKENS_STR: &str = "tokens";
static SHOW_AST_STR: &str = "ast";
static DISASSEMBLE_STR: &str = "disassemble";
static DEBUG_STR: &str = "debug";
static TREEWALK_STR: &str = "treewalk";
static LITERAL_INPUT: &str = "c";
static EXTENSION_LISTS: &str = "Xlists";
static EXTENSION_LAMBDAS: &str = "Xlambdas";

fn get_input(matches: &clap::ArgMatches<'_>) -> Option<String> {
    if let Some(literal_input) = matches.value_of(LITERAL_INPUT) {
        return Some(literal_input.to_string());
    }
    if let Some(input_file) = matches.value_of(INPUT_STR) {
        match fs::read_to_string(input_file) {
            Ok(input) => {
                return Some(input);
            }
            Err(err) => {
                println!("Error reading {}: {}", input_file, err);
                std::process::exit(-1);
            }
        }
    }

    None
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
        .arg(
            Arg::with_name(LITERAL_INPUT)
                .long("-c")
                .takes_value(true)
                .help("provide a literal string of Lox code"),
        )
        .arg(
            Arg::with_name(EXTENSION_LISTS)
                .long(&format!["--{}", EXTENSION_LISTS])
                .takes_value(false)
                .help("use the lists extension"),
        )
        .arg(
            Arg::with_name(EXTENSION_LAMBDAS)
                .long(&format!["--{}", EXTENSION_LAMBDAS])
                .takes_value(false)
                .help("use the lambdas extension"),
        )
        .get_matches();

    let extensions = syntax_extensions::Extensions {
        lists: matches.is_present(EXTENSION_LISTS),
        lambdas: matches.is_present(EXTENSION_LAMBDAS),
    };

    if let Some(input) = get_input(&matches) {
        if matches.is_present(SHOW_TOKENS_STR)
            || matches.is_present(SHOW_AST_STR)
            || matches.is_present(TREEWALK_STR)
        {
            match scanner::scan_tokens(input.clone()) {
                Ok(tokens) => {
                    if matches.is_present(SHOW_TOKENS_STR) {
                        println!("{:#?}", tokens);
                        std::process::exit(0);
                    }

                    let stmts_maybe = parser::parse(extensions, tokens);

                    match stmts_maybe {
                        Ok(stmts) => {
                            if matches.is_present(SHOW_AST_STR) {
                                println!("{:#?}", stmts);
                                std::process::exit(0);
                            }

                            let mut interpreter: treewalk_interpreter::Interpreter =
                                Default::default();
                            let interpret_result = interpreter.interpret(&stmts);

                            match interpret_result {
                                Ok(_) => {
                                    std::process::exit(0);
                                }
                                Err(err) => {
                                    println!(
                                        "Runtime Error: {}\n\n{}",
                                        err,
                                        interpreter.format_backtrace()
                                    );
                                    std::process::exit(-1);
                                }
                            }
                        }
                        Err(err) => {
                            error_formatting::format_parse_error(&err, &input);
                            std::process::exit(-1)
                        }
                    }
                }
                Err(err) => {
                    error_formatting::format_lexical_error(&err);
                    std::process::exit(-1);
                }
            }
        }

        let func_or_err = compiler::Compiler::compile(input.clone(), extensions);

        match func_or_err {
            Ok(func) => {
                if matches.is_present(DISASSEMBLE_STR) {
                    println!(
                        "{}",
                        bytecode_interpreter::disassemble_chunk(&func.chunk, &"".to_string())
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
    } else {
        repl::run(extensions);
    }
}
