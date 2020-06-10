use std::env;
use std::fs;

mod scanner;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return println!("Expected a file input arg");
    }

    let text = scanner::scan_tokens(fs::read_to_string(&args[1]).unwrap());

    match text {
        Ok(tokens) => {
            for t in tokens {
                println!("{:?}", t)
            }
        }
        Err(err) => println!("lexical error: {}", err),
    }
}
