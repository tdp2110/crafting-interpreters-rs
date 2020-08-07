use serde::{Deserialize, Serialize};
use std::fmt;

use crate::scanner;

#[derive(Copy, Clone)]
pub enum Value {
    Number(f64),
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

#[derive(Default)]
pub struct Compiler {
    tokens: Vec<scanner::Token>,
    current_chunk: Chunk,
    current: usize,
}

impl Compiler {
    fn compile(&mut self, input: String) -> Result<Chunk, String> {
        match scanner::scan_tokens(input) {
            Ok(tokens) => {
                self.tokens = tokens;
                self.current_chunk = Chunk::default();
                self.expression()?;
                Ok(std::mem::replace(&mut self.current_chunk, Chunk::default()))
            }
            Err(err) => Err(err),
        }
    }

    fn number(&mut self) -> Result<(), String> {
        let current_tok = &self.tokens[self.current];
        let lineno = current_tok.line;
        match current_tok.literal {
            Some(scanner::Literal::Number(n)) => {
                self.emit_constant(n, lineno);
                Ok(())
            }
            _ => Err(format!(
                "Expected number at line={},col={}",
                current_tok.line, current_tok.col
            )),
        }
    }

    fn emit_constant(&mut self, n: f64, lineno: usize) {
        let const_idx = self.current_chunk.constants.len();
        self.current_chunk.constants.push(Value::Number(n));
        self.current_chunk
            .code
            .push((Op::Constant(const_idx), Lineno(lineno)));
    }

    fn expression(&mut self) -> Result<(), String> {
        unimplemented!();
    }
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

#[derive(Eq, PartialEq, Debug, Clone)]
#[allow(dead_code)]
pub enum InterpreterError {
    Compile(String),
    Runtime(String),
}

impl Interpreter {
    pub fn interpret(&mut self, chunk: Chunk) -> Result<(), InterpreterError> {
        self.chunk = chunk;
        return self.run();
    }

    fn run(&mut self) -> Result<(), InterpreterError> {
        loop {
            match self.next_op() {
                (Op::Return, _) => {
                    return Ok(());
                }
                (Op::Constant(idx), _) => {
                    let constant = self.read_constant(idx);
                    self.stack.push(constant);
                }
                (Op::Negate, _) => {
                    let to_negate = self.pop_stack();
                    self.stack.push(Interpreter::negate(to_negate)?)
                }
                (Op::Add, _) => {
                    let left = self.pop_stack();
                    let right = self.pop_stack();
                    self.stack.push(Interpreter::add(left, right)?)
                }
                (Op::Subtract, _) => {
                    let left = self.pop_stack();
                    let right = self.pop_stack();
                    self.stack.push(Interpreter::subtract(left, right)?)
                }
                (Op::Multiply, _) => {
                    let left = self.pop_stack();
                    let right = self.pop_stack();
                    self.stack.push(Interpreter::multiply(left, right)?)
                }
                (Op::Divide, _) => {
                    let left = self.pop_stack();
                    let right = self.pop_stack();
                    self.stack.push(Interpreter::divide(left, right)?)
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

    fn negate(val: Value) -> Result<Value, InterpreterError> {
        match val {
            Value::Number(num) => Ok(Value::Number(-num)),
        }
    }

    fn add(val1: Value, val2: Value) -> Result<Value, InterpreterError> {
        match (val1, val2) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Number(num1 + num2)),
        }
    }

    fn multiply(val1: Value, val2: Value) -> Result<Value, InterpreterError> {
        match (val1, val2) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Number(num1 * num2)),
        }
    }

    fn divide(val1: Value, val2: Value) -> Result<Value, InterpreterError> {
        match (val1, val2) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Number(num1 * num2)),
        }
    }

    fn subtract(val1: Value, val2: Value) -> Result<Value, InterpreterError> {
        match (val1, val2) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Number(num1 - num2)),
        }
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

        assert!(res.is_ok())
    }
}
