use crate::bytecode;
use crate::gc;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
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
    pub func: fn(&gc::Heap, &Vec<Value>) -> Result<Value, String>,
}

#[derive(Clone)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, gc::HeapId>,
}

#[derive(Clone)]
pub struct Instance {
    pub class_id: gc::HeapId,
    pub fields: HashMap<String, Value>,
}

#[derive(Clone)]
pub struct BoundMethod {
    pub instance_id: gc::HeapId,
    pub closure_id: gc::HeapId,
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(gc::HeapId),
    Function(gc::HeapId),
    Instance(gc::HeapId),
    BoundMethod(gc::HeapId),
    Class(gc::HeapId),
    NativeFunction(NativeFunction),
    Nil,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Number,
    Bool,
    String,
    Function,
    NativeFunction,
    Class,
    BoundMethod,
    Instance,
    Nil,
}

pub fn type_of(value: &Value) -> Type {
    match value {
        Value::Number(_) => Type::Number,
        Value::Bool(_) => Type::Bool,
        Value::String(_) => Type::String,
        Value::Function(_) => Type::Function,
        Value::NativeFunction(_) => Type::NativeFunction,
        Value::BoundMethod(_) => Type::BoundMethod,
        Value::Class(_) => Type::Class,
        Value::Instance(_) => Type::Instance,
        Value::Nil => Type::Nil,
    }
}
