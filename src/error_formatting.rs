use crate::compiler;
use crate::input;
use crate::parser;
use crate::scanner;

use colored::*;

fn format_input(input: &input::Input, line: usize, col: i64) {
    eprintln!(
        "in {}, at line {}, column {}:",
        match &input.source {
            input::Source::Literal => "<command-line input>",
            input::Source::File(filename) => &filename,
        },
        line,
        col
    );
    eprintln!("{}", input.content.lines().nth(line - 1).unwrap());
    eprint!("{:~<1$}", "".blue().bold(), col as usize);
    eprintln!("{}", "^".blue().bold());
}

enum CompilerErrorKind {
    Parse,
    Semantic,
}

fn format_compiler_error_info(
    err: &compiler::ErrorInfo,
    input: &input::Input,
    kind: CompilerErrorKind,
) {
    eprintln!(
        "loxi: {}: {}",
        match kind {
            CompilerErrorKind::Parse => "parse error",
            CompilerErrorKind::Semantic => "semantic error",
        }
        .to_string()
        .red()
        .bold(),
        err.what.white().bold(),
    );

    format_input(input, err.line, err.col);
}

pub fn format_compiler_error(err: &compiler::Error, input: &input::Input) {
    match err {
        compiler::Error::Lexical(err) => format_lexical_error(err, input),
        compiler::Error::Parse(err) => {
            format_compiler_error_info(err, input, CompilerErrorKind::Parse)
        }
        compiler::Error::Semantic(err) => {
            format_compiler_error_info(err, input, CompilerErrorKind::Semantic)
        }
        compiler::Error::Internal(err) => {
            eprintln!(
                "loxi: {}: {}",
                "internal error".red().bold(),
                err.white().bold()
            );
        }
    }
}

pub fn format_parse_error(err: &parser::Error, input: &input::Input) {
    let err_str = format!("{:?}", err);
    eprintln!(
        "loxi: {}: {}",
        "parse error".red().bold(),
        err_str.white().bold()
    );

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

    format_input(input, *line, *col);
}

pub fn format_lexical_error(err: &scanner::Error, input: &input::Input) {
    eprintln!(
        "loxi: {}: {}",
        "lexical error".red().bold(),
        err.what.white().bold(),
    );

    format_input(input, err.line, err.col);
}
