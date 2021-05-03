use std::time::{SystemTime, UNIX_EPOCH};

use crate::bytecode_interpreter;
use crate::value;

/*
Arity checking is done in the interpreter prior to calling a builtin function.
*/

pub fn exp(
    _interp: &mut bytecode_interpreter::Interpreter,
    args: &[value::Value],
) -> Result<value::Value, String> {
    match args[0] {
        value::Value::Number(num) => Ok(value::Value::Number(num.exp())),
        _ => Err(format!(
            "Invalid call: expected number, got {:?}.",
            value::type_of(&args[0])
        )),
    }
}

pub fn sqrt(
    _interp: &mut bytecode_interpreter::Interpreter,
    args: &[value::Value],
) -> Result<value::Value, String> {
    match args[0] {
        value::Value::Number(num) => Ok(value::Value::Number(num.sqrt())),
        _ => Err(format!(
            "Invalid call: expected number, got {:?}.",
            value::type_of(&args[0])
        )),
    }
}

pub fn clock(
    _interp: &mut bytecode_interpreter::Interpreter,
    _args: &[value::Value],
) -> Result<value::Value, String> {
    let start = SystemTime::now();
    let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap();

    Ok(value::Value::Number(since_the_epoch.as_millis() as f64))
}

pub fn len(
    interp: &mut bytecode_interpreter::Interpreter,
    args: &[value::Value],
) -> Result<value::Value, String> {
    match &args[0] {
        value::Value::String(id) => Ok(value::Value::Number(interp.heap.get_str(*id).len() as f64)),
        value::Value::List(id) => Ok(value::Value::Number(
            interp.heap.get_list_elements(*id).len() as f64,
        )),
        val => Err(format!(
            "Ojbect of type {:?} has no len.",
            value::type_of(val)
        )),
    }
}

pub fn for_each(
    interp: &mut bytecode_interpreter::Interpreter,
    args: &[value::Value],
) -> Result<value::Value, String> {
    match &args[0] {
        value::Value::List(id) => {
            let list_elements = interp.heap.get_list_elements(*id).clone();
            let val_to_call = args[1].clone();
            for element in list_elements.iter().rev() {
                interp.stack.push(val_to_call.clone());
                interp.stack.push(element.clone());
                if let Err(bytecode_interpreter::InterpreterError::Runtime(err)) =
                    interp.call_value(val_to_call.clone(), 1)
                {
                    return Err(err);
                }
            }
            Ok(value::Value::Nil)
        }
        val => Err(format!(
            "Can't call forEach on value of type {:?}.",
            value::type_of(val)
        )),
    }
}
