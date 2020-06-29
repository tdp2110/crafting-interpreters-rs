use std::env;
use std::fs;

mod expr;
mod interpreter;
mod parser;
mod scanner;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return println!("Expected a file input arg");
    }

    match scanner::scan_tokens(fs::read_to_string(&args[1]).unwrap()) {
        Ok(tokens) => {
            let stmts_maybe = parser::parse(tokens);

            match stmts_maybe {
                Ok(stmts) => {
                    let interpret_result = interpreter::interpret(&stmts);

                    match interpret_result {
                        Ok(output) => {
                            println!("{}", output);
                        }
                        Err(err) => println!("Interpreter Error:\n{}", err),
                    }
                }
                Err(err) => println!("parse error: {}", err),
            }
        }
        Err(err) => println!("lexical error: {}", err),
    }
}
