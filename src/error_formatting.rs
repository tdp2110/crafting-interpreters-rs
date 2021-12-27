use crate::parser;

use colored::*;

pub fn format_parse_error(err: &parser::Error, input: &str) {
    println!("{}: {:?}", "parse error".red().bold(), err);

    let maybe_line_and_col = match err {
        parser::Error::UnexpectedToken(_) => None,
        parser::Error::TokenMismatch { .. } => None,
        parser::Error::MaxParamsExceeded { line, col, .. } => Some((line, col)),
        parser::Error::ReturnNotInFun { line, col, .. } => Some((line, col)),
        parser::Error::InvalidAssignment { line, col, .. } => Some((line, col)),
        parser::Error::TooManyArguments { line, col, .. } => Some((line, col)),
        parser::Error::ExpectedExpression { line, col, .. } => Some((line, col)),
        parser::Error::InvalidTokenInUnaryOp { line, col, .. } => Some((line, col)),
        parser::Error::InvalidTokenInBinaryOp { line, col, .. } => Some((line, col)),
    };

    if let Some((line, col)) = maybe_line_and_col {
        println!("{}", input.lines().nth(*line - 1).unwrap());
        print!("{:~<1$}", "".red().bold(), *col as usize);
        println!("{}", "^".red().bold());
    }
}

pub fn format_lexical_error(err: &str) {
    println!("{}: {}", "lexical error".red().bold(), err);
}
