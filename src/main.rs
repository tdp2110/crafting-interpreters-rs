use std::env;
use std::fs;

mod expr;
mod parser;
mod scanner;
mod treewalk_interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return println!("Expected a file input arg");
    }

    let maybe_input = fs::read_to_string(&args[1]);

    match maybe_input {
        Ok(input) => match scanner::scan_tokens(input) {
            Ok(tokens) => {
                let stmts_maybe = parser::parse(tokens);

                match stmts_maybe {
                    Ok(stmts) => {
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
        },
        Err(err) => {
            println!("Error: {}", err);
            std::process::exit(-1);
        }
    }
}
