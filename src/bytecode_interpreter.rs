use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Copy, Clone)]
pub enum Value {
    Number(f64),
}

fn negate(val: Value) -> Value {
    match val {
        Value::Number(num) => Value::Number(-num),
    }
}

fn add(val1: Value, val2: Value) -> Value {
    match (val1, val2) {
        (Value::Number(num1), Value::Number(num2)) => Value::Number(num1 + num2),
    }
}

fn multiply(val1: Value, val2: Value) -> Value {
    match (val1, val2) {
        (Value::Number(num1), Value::Number(num2)) => Value::Number(num1 * num2),
    }
}

fn divide(val1: Value, val2: Value) -> Value {
    match (val1, val2) {
        (Value::Number(num1), Value::Number(num2)) => Value::Number(num1 * num2),
    }
}

fn subtract(val1: Value, val2: Value) -> Value {
    match (val1, val2) {
        (Value::Number(num1), Value::Number(num2)) => Value::Number(num1 - num2),
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(num) => write!(f, "{}", num),
        }
    }
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

#[derive(Default, Copy, Clone)]
pub struct Lineno(usize);

#[derive(Default)]
pub struct Chunk {
    code: Vec<(Op, Lineno)>,
    constants: Vec<Value>,
}

#[allow(dead_code)]
pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    for (idx, (op, Lineno(lineno))) in chunk.code.iter().enumerate() {
        print!("{:04} ", idx);
        match op {
            Op::Return => print!("OP_RETURN"),
            Op::Constant(const_idx) => print!("OP_CONSTANT {:?}", chunk.constants[*const_idx]),
            Op::Negate => print!("OP_NEGATE"),
            Op::Add => print!("OP_ADD"),
            Op::Subtract => print!("OP_SUBTRACT"),
            Op::Multiply => print!("OP_MULTIPLY"),
            Op::Divide => print!("OP_DIVIDE"),
        }
        println!("\t\tline {}", lineno);
    }
}

pub struct Interpreter {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        let mut res = Interpreter {
            chunk: Default::default(),
            ip: 0,
            stack: Vec::new(),
        };
        res.stack.reserve(256);
        res
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum InterpreterResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl Interpreter {
    pub fn interpret(&mut self, chunk: Chunk) -> InterpreterResult {
        self.chunk = chunk;
        return self.run();
    }

    fn run(&mut self) -> InterpreterResult {
        loop {
            match self.next_op() {
                (Op::Return, _) => {
                    println!("{:?}", self.stack.pop());
                    return InterpreterResult::Ok;
                }
                (Op::Constant(idx), _) => {
                    let constant = self.read_constant(idx);
                    self.stack.push(constant);
                }
                (Op::Negate, _) => {
                    let res = negate(self.pop_stack());
                    self.stack.push(res)
                }
                (Op::Add, _) => {
                    let left = self.pop_stack();
                    let right = self.pop_stack();
                    self.stack.push(add(left, right))
                }
                (Op::Subtract, _) => {
                    let left = self.pop_stack();
                    let right = self.pop_stack();
                    self.stack.push(subtract(left, right))
                }
                (Op::Multiply, _) => {
                    let left = self.pop_stack();
                    let right = self.pop_stack();
                    self.stack.push(multiply(left, right))
                }
                (Op::Divide, _) => {
                    let left = self.pop_stack();
                    let right = self.pop_stack();
                    self.stack.push(divide(left, right))
                }
            }
        }
    }

    fn pop_stack(&mut self) -> Value {
        match self.stack.pop() {
            Some(val) => val,
            None => panic!("attempted to pop empty stack!"),
        }
    }

    fn next_op(&mut self) -> (Op, Lineno) {
        let res = self.chunk.code[self.ip];
        self.ip += 1;
        res
    }

    fn read_constant(&self, idx: usize) -> Value {
        self.chunk.constants[idx]
    }
}

#[cfg(test)]
mod tests {
    use crate::bytecode_interpreter::*;

    #[test]
    fn test_interpret_handcoded_bytecode() {
        let code = Chunk {
            code: vec![
                (Op::Constant(0), Lineno(42)),
                (Op::Constant(1), Lineno(42)),
                (Op::Add, Lineno(42)),
                (Op::Constant(2), Lineno(42)),
                (Op::Divide, Lineno(42)),
                (Op::Return, Lineno(42)),
            ],
            constants: vec![Value::Number(1.2), Value::Number(3.4), Value::Number(5.6)],
        };

        let res = Interpreter::default().interpret(code);

        assert_eq!(res, InterpreterResult::Ok)
    }
}
