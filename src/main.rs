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
            println!("Tokens");
            for t in &tokens {
                println!("{:?}", t)
            }
            println!();

            let expr_maybe = parser::parse(tokens);

            match expr_maybe {
                Ok(expr) => {
                    println!("AST:\n{:#?}", expr);
                    let interpret_result = interpreter::interpret(&expr);
                    println!();

                    match interpret_result {
                        Ok(val) => println!("Result:\n{:#?}", val),
                        Err(err) => println!("Interpreter Error:\n{}", err),
                    }
                }
                Err(err) => println!("parse error: {}", err),
            }
        }
        Err(err) => println!("lexical error: {}", err),
    }
}
