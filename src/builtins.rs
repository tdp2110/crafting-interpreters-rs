use std::time::{SystemTime, UNIX_EPOCH};

use crate::bytecode;

/*
Arity checking is done in the interpreter prior to calling a builtin function.
*/

pub fn exp(args: Vec<bytecode::Value>) -> Result<bytecode::Value, String> {
    match args[0] {
        bytecode::Value::Number(num) => Ok(bytecode::Value::Number(num.exp())),
        _ => Err(format!(
            "Invalid call: expected number, got {:?}.",
            bytecode::type_of(&args[0])
        )),
    }
}

pub fn sqrt(args: Vec<bytecode::Value>) -> Result<bytecode::Value, String> {
    match args[0] {
        bytecode::Value::Number(num) => Ok(bytecode::Value::Number(num.sqrt())),
        _ => Err(format!(
            "Invalid call: expected number, got {:?}.",
            bytecode::type_of(&args[0])
        )),
    }
}

pub fn clock(_args: Vec<bytecode::Value>) -> Result<bytecode::Value, String> {
    let start = SystemTime::now();
    let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap();

    Ok(bytecode::Value::Number(since_the_epoch.as_millis() as f64))
}
