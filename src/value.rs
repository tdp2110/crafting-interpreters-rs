use crate::bytecode;
use crate::gc;

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
#[allow(dead_code)]
pub enum Upvalue {
    Open(usize),
    Closed(Value),
}

impl Upvalue {
    pub fn is_open(&self) -> bool {
        match self {
            Upvalue::Open(_) => true,
            Upvalue::Closed(_) => false,
        }
    }

    pub fn is_open_with_index(&self, index: usize) -> bool {
        match self {
            Upvalue::Open(idx) => index == *idx,
            Upvalue::Closed(_) => false,
        }
    }
}

#[derive(Default, Clone)]
pub struct Closure {
    pub function: bytecode::Function,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

#[derive(Clone)]
pub struct NativeFunction {
    pub arity: u8,
    pub name: String,
    pub func: fn(&gc::Heap, Vec<Value>) -> Result<Value, String>,
}

#[derive(Clone)]
pub struct Class {
    pub name: String,
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(usize),
    Function(usize),
    Class(usize),
    NativeFunction(NativeFunction),
    Nil,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[allow(dead_code)]
pub enum Type {
    Number,
    Bool,
    String,
    Function,
    NativeFunction,
    Class,
    Nil,
}

pub fn type_of(value: &Value) -> Type {
    match value {
        Value::Number(_) => Type::Number,
        Value::Bool(_) => Type::Bool,
        Value::String(_) => Type::String,
        Value::Function(_) => Type::Function,
        Value::NativeFunction(_) => Type::NativeFunction,
        Value::Class(_) => Type::Class,
        Value::Nil => Type::Nil,
    }
}
