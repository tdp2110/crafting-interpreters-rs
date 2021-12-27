use crate::parser;

use colored::*;

pub fn format_parse_error(err: &parser::Error) {
    println!("{}: {:?}", "parse error".red().bold(), err);
}

pub fn format_lexical_error(err: &str) {
    println!("{}: {}", "lexical error".red().bold(), err);
}
