use serde::{Deserialize, Serialize};

use crate::value;

#[derive(Default, Copy, Clone)]
pub struct Lineno {
    pub value: usize,
}

#[allow(non_snake_case)]
pub fn Lineno(value: usize) -> Lineno {
    Lineno { value }
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
#[serde(untagged)]
pub enum Op {
    Return,
    Constant(usize),
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Default)]
pub struct Chunk {
    pub code: Vec<(Op, Lineno)>,
    pub constants: Vec<value::Value>,
}

impl Chunk {
    pub fn add_constant(&mut self, c: f64) -> usize {
        let const_idx = self.constants.len();
        self.constants.push(value::Value::Number(c));
        const_idx
    }
}
