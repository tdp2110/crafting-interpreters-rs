use serde::{Deserialize, Serialize};

use std::f64;
use std::fmt;

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
    Class(usize),
    SetProperty(usize),
    GetProperty(usize),
    Method(usize),
    Invoke(/*method_name*/ String, /*arg count*/ u8),
    Inherit,
    GetSuper(usize),
    SuperInvoke(/*method_name*/ String, /*arg count*/ u8),
    BuildList(usize),
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

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Number(n) => write!(f, "{}", n),
            Constant::String(s) => write!(f, "\"{}\"", s),
            Constant::Function(Closure {
                function:
                    Function {
                        arity: _,
                        chunk: _,
                        name,
                    },
                upvalues: _,
            }) => write!(f, "<fn {}>", name),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Chunk {
    pub code: Vec<(Op, Lineno)>,
    pub constants: Vec<Constant>,
}

impl Chunk {
    pub fn add_constant_number(&mut self, c: f64) -> usize {
        if let Some(id) = self.find_number(c) {
            id
        } else {
            self.add_constant(Constant::Number(c))
        }
    }

    pub fn add_constant_string(&mut self, s: String) -> usize {
        if let Some(id) = self.find_string(&s) {
            id
        } else {
            self.add_constant(Constant::String(s))
        }
    }

    pub fn add_constant(&mut self, val: Constant) -> usize {
        let const_idx = self.constants.len();
        self.constants.push(val);
        const_idx
    }

    fn find_string(&self, s: &str) -> Option<usize> {
        self.constants.iter().position(|c| {
            if let Constant::String(s2) = c {
                s == s2
            } else {
                false
            }
        })
    }

    fn find_number(&self, num: f64) -> Option<usize> {
        self.constants.iter().position(|c| {
            if let Constant::Number(num2) = c {
                (num - num2).abs() < f64::EPSILON
            } else {
                false
            }
        })
    }
}
