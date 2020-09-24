use serde::{Deserialize, Serialize};

#[derive(Default, Copy, Clone, Debug)]
pub struct Lineno {
    pub value: usize,
}

#[allow(non_snake_case)]
pub fn Lineno(value: usize) -> Lineno {
    Lineno { value }
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Copy, Clone)]
pub enum UpvalueLoc {
    Upvalue(/*upvalue idx*/ usize),
    Local(/*stack idx*/ usize),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum Op {
    Return,
    Constant(usize),
    Closure(usize, Vec<UpvalueLoc>),
    Nil,
    True,
    False,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal(usize),
    GetGlobal(usize),
    SetGlobal(usize),
    GetLocal(usize),
    SetLocal(usize),
    GetUpval(usize),
    SetUpval(usize),
    JumpIfFalse(usize),
    Jump(usize),
    Loop(usize),
    Call(u8),
    CloseUpvalue,
}

#[derive(Default, Clone, Debug)]
pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: String,
}

#[derive(Debug, Clone, Default)]
pub struct Closure {
    pub function: Function,
    pub upvalues: Vec<UpvalueLoc>,
}

#[derive(Debug, Clone)]
pub enum Constant {
    Number(f64),
    String(String),
    Function(Closure),
}

#[derive(Debug, Default, Clone)]
pub struct Chunk {
    pub code: Vec<(Op, Lineno)>,
    pub constants: Vec<Constant>,
}

impl Chunk {
    pub fn add_constant_number(&mut self, c: f64) -> usize {
        self.add_constant(Constant::Number(c))
    }

    pub fn add_constant_string(&mut self, s: String) -> usize {
        self.add_constant(Constant::String(s))
    }

    pub fn add_constant(&mut self, val: Constant) -> usize {
        let const_idx = self.constants.len();
        self.constants.push(val);
        const_idx
    }
}
