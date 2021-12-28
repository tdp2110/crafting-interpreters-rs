use crate::parser;

use colored::*;

pub fn format_parse_error(err: &parser::Error, input: &str) {
    let err_str = format!("{:?}", err);
    println!("{}: {}", "parse error".red().bold(), err_str.white().bold());

    let (line, col) = match err {
        parser::Error::UnexpectedToken(tok) => (&tok.line, &tok.col),
        parser::Error::TokenMismatch { found, .. } => (&found.line, &found.col),
        parser::Error::MaxParamsExceeded { line, col, .. } => (line, col),
        parser::Error::ReturnNotInFun { line, col, .. } => (line, col),
        parser::Error::InvalidAssignment { line, col, .. } => (line, col),
        parser::Error::TooManyArguments { line, col, .. } => (line, col),
        parser::Error::ExpectedExpression { line, col, .. } => (line, col),
        parser::Error::InvalidTokenInUnaryOp { line, col, .. } => (line, col),
        parser::Error::InvalidTokenInBinaryOp { line, col, .. } => (line, col),
    };

    println!("{}", input.lines().nth(*line - 1).unwrap());
    print!("{:~<1$}", "".blue().bold(), *col as usize);
    println!("{}", "^".blue().bold());
}

pub fn format_lexical_error(err: &str) {
    println!("{}: {}", "lexical error".red().bold(), err.white().bold());
}
