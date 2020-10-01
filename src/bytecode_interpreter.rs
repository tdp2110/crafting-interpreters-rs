use crate::builtins;
use crate::bytecode;
use crate::gc;
use crate::gc_values;
use crate::value;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub fn disassemble_chunk(chunk: &bytecode::Chunk, name: &str) {
    if !name.is_empty() {
        println!("============ {} ============", name);
    }

    println!("------------ constants -----------");
    for (idx, constant) in chunk.constants.iter().enumerate() {
        println!("{:<4} {:?}", idx, constant);
    }

    println!("\n------------ code -----------------");
    for (idx, (op, lineno)) in chunk.code.iter().enumerate() {
        let formatted_op = match op {
            bytecode::Op::Return => "OP_RETURN".to_string(),
            bytecode::Op::Constant(const_idx) => format!(
                "OP_CONSTANT {:?} (idx={})",
                chunk.constants[*const_idx], *const_idx
            ),
            bytecode::Op::Nil => "OP_NIL".to_string(),
            bytecode::Op::True => "OP_TRUE".to_string(),
            bytecode::Op::False => "OP_FALSE".to_string(),
            bytecode::Op::Negate => "OP_NEGATE".to_string(),
            bytecode::Op::Add => "OP_ADD".to_string(),
            bytecode::Op::Subtract => "OP_SUBTRACT".to_string(),
            bytecode::Op::Multiply => "OP_MULTIPLY".to_string(),
            bytecode::Op::Divide => "OP_DIVIDE".to_string(),
            bytecode::Op::Not => "OP_NOT".to_string(),
            bytecode::Op::Equal => "OP_NOT".to_string(),
            bytecode::Op::Greater => "OP_GREATER".to_string(),
            bytecode::Op::Less => "OP_LESS".to_string(),
            bytecode::Op::Print => "OP_PRINT".to_string(),
            bytecode::Op::Pop => "OP_POP".to_string(),
            bytecode::Op::DefineGlobal(global_idx) => format!(
                "OP_DEFINE_GLOBAL {:?} (idx={})",
                chunk.constants[*global_idx], *global_idx
            ),
            bytecode::Op::GetGlobal(global_idx) => format!(
                "OP_GET_GLOBAL {:?} (idx={})",
                chunk.constants[*global_idx], *global_idx
            ),
            bytecode::Op::SetGlobal(global_idx) => format!(
                "OP_SET_GLOBAL {:?} (idx={})",
                chunk.constants[*global_idx], *global_idx
            ),
            bytecode::Op::GetLocal(idx) => format!("OP_GET_LOCAL idx={}", *idx),
            bytecode::Op::SetLocal(idx) => format!("OP_SET_LOCAL idx={}", *idx),
            bytecode::Op::GetUpval(idx) => format!("OP_GET_UPVAL idx={}", *idx),
            bytecode::Op::SetUpval(idx) => format!("OP_SET_UPVAL idx={}", *idx),
            bytecode::Op::JumpIfFalse(loc) => format!("OP_JUMP_IF_FALSE {}", *loc),
            bytecode::Op::Jump(offset) => format!("OP_JUMP {}", *offset),
            bytecode::Op::Loop(offset) => format!("OP_LOOP {}", *offset),
            bytecode::Op::Call(arg_count) => format!("OP_CALL {}", *arg_count),
            bytecode::Op::Closure(idx, upvals) => format!(
                "OP_CLOSURE {:?} (idx={}, upvals={:?})",
                chunk.constants[*idx], *idx, upvals
            ),
            bytecode::Op::CloseUpvalue => "OP_CLOSE_UPVALUE".to_string(),
        };

        println!(
            "{0: <04}   {1: <30} {2: <30}",
            idx,
            formatted_op,
            format!("line {}", lineno.value)
        );
    }
}

fn dis_builtin(heap: &gc::Heap, args: Vec<value::Value>) -> Result<value::Value, String> {
    // arity checking is done in the interpreter
    match &args[0] {
        value::Value::Function(closure_handle) => {
            let closure = heap.get_closure(closure_handle);
            disassemble_chunk(&closure.function.chunk, "");
            Ok(value::Value::Nil)
        }
        _ => Err(format!(
            "Invalid call: expected lox function, got {:?}.",
            value::type_of(&args[0])
        )),
    }
}

#[allow(dead_code)]
#[derive(Debug)]
enum Binop {
    Add,
    Sub,
    Mul,
    Div,
}

pub struct Interpreter {
    frames: Vec<CallFrame>,
    stack: Vec<value::Value>,
    output: Vec<String>,
    globals: HashMap<String, value::Value>,
    upvalues: Vec<Rc<RefCell<value::Upvalue>>>,
    heap: gc::Heap,
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        let mut res = Interpreter {
            frames: Default::default(),
            stack: Default::default(),
            output: Default::default(),
            globals: Default::default(),
            upvalues: Default::default(),
            heap: Default::default(),
        };
        res.stack.reserve(256);
        res.frames.reserve(64);

        res.globals.insert(
            String::from("dis"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 1,
                name: String::from("dis"),
                func: dis_builtin,
            }),
        );
        res.globals.insert(
            String::from("clock"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 0,
                name: String::from("clock"),
                func: builtins::clock,
            }),
        );
        res.globals.insert(
            String::from("exp"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 1,
                name: String::from("exp"),
                func: builtins::exp,
            }),
        );
        res.globals.insert(
            String::from("sqrt"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 1,
                name: String::from("sqrt"),
                func: builtins::sqrt,
            }),
        );

        res
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
#[allow(dead_code)]
pub enum InterpreterError {
    Compile(String),
    Runtime(String),
}

#[derive(Default)]
struct CallFrame {
    closure: value::Closure,
    ip: usize,
    slots_offset: usize,
}

impl CallFrame {
    fn next_op(&mut self) -> (bytecode::Op, bytecode::Lineno) {
        let res = self.closure.function.chunk.code[self.ip].clone();
        self.ip += 1;
        res
    }

    fn read_constant(&self, idx: usize) -> bytecode::Constant {
        self.closure.function.chunk.constants[idx].clone()
    }
}

impl Interpreter {
    pub fn interpret(&mut self, func: bytecode::Function) -> Result<(), InterpreterError> {
        self.stack
            .push(value::Value::Function(self.heap.manage_closure(
                value::Closure {
                    function: func.clone(),
                    upvalues: Vec::new(),
                },
            )));
        self.frames.push(CallFrame {
            closure: value::Closure {
                function: func,
                upvalues: Vec::new(),
            },
            ip: 0,
            slots_offset: 1,
        });
        self.run()
    }

    pub fn format_val(&self, val: &value::Value) -> String {
        match val {
            value::Value::Number(num) => num.to_string(),
            value::Value::Bool(b) => b.to_string(),
            value::Value::String(str_handle) => self.get_str(&str_handle).clone(),
            value::Value::Function(closure_handle) => {
                format!("<fn {}>", self.get_closure(&closure_handle).function.name)
            }
            value::Value::NativeFunction(func) => format!("<native fn {}>", func.name),
            value::Value::Nil => "nil".to_string(),
        }
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        let frames_len = self.frames.len();
        &mut self.frames[frames_len - 1]
    }

    fn frame(&self) -> &CallFrame {
        &self.frames[self.frames.len() - 1]
    }

    fn run(&mut self) -> Result<(), InterpreterError> {
        loop {
            if self.frames.is_empty()
                || self.frame().ip >= self.frame().closure.function.chunk.code.len()
            {
                return Ok(());
            }

            let op = self.next_op();

            match op {
                (bytecode::Op::Return, _) => {
                    let result = self.pop_stack();

                    for idx in self.frame().slots_offset..self.stack.len() {
                        self.close_upvalues(idx);
                    }

                    let num_to_pop = self.stack.len() - self.frame().slots_offset
                        + usize::from(self.frame().closure.function.arity);
                    self.frames.pop();

                    self.pop_stack_n_times(num_to_pop);

                    if self.frames.is_empty() {
                        self.pop_stack();
                        return Ok(());
                    }

                    self.stack.push(result);
                }
                (bytecode::Op::Closure(idx, upvals), _) => {
                    let constant = self.read_constant(idx);

                    if let value::Value::Function(closure_handle) = constant {
                        let closure = self.get_closure(&closure_handle).clone();
                        let upvalues = upvals
                            .iter()
                            .map(|upval| match upval {
                                bytecode::UpvalueLoc::Upvalue(idx) => {
                                    self.frame().closure.upvalues[*idx].clone()
                                }
                                bytecode::UpvalueLoc::Local(idx) => {
                                    if let Some(upval) = self.find_open_uval(*idx) {
                                        upval
                                    } else {
                                        let index = self.frame().slots_offset + *idx;
                                        let upval =
                                            Rc::new(RefCell::new(value::Upvalue::Open(index)));
                                        self.upvalues.push(upval.clone());
                                        upval
                                    }
                                }
                            })
                            .collect();

                        self.stack
                            .push(value::Value::Function(self.heap.manage_closure(
                                value::Closure {
                                    function: closure.function,
                                    upvalues,
                                },
                            )));
                    } else {
                        panic!(
                            "When interpreting bytecode::Op::Closure, expected function, found {:?}",
                            value::type_of(&constant)
                        );
                    }
                }
                (bytecode::Op::Constant(idx), _) => {
                    let constant = self.read_constant(idx);
                    self.stack.push(constant);
                }
                (bytecode::Op::Nil, _) => {
                    self.stack.push(value::Value::Nil);
                }
                (bytecode::Op::True, _) => {
                    self.stack.push(value::Value::Bool(true));
                }
                (bytecode::Op::False, _) => {
                    self.stack.push(value::Value::Bool(false));
                }
                (bytecode::Op::Negate, lineno) => {
                    let top_stack = self.peek();
                    let maybe_number = Interpreter::extract_number(top_stack);

                    match maybe_number {
                        Some(to_negate) => {
                            self.pop_stack();
                            self.stack.push(value::Value::Number(-to_negate));
                        }
                        None => {
                            return Err(InterpreterError::Runtime(format!(
                                "invalid operand to unary op negate. Expected number, found {:?} at line {}",
                                value::type_of(top_stack), lineno.value
                            )))
                        }
                    }
                }
                (bytecode::Op::Add, lineno) => {
                    let val1 = self.peek_by(0).clone();
                    let val2 = self.peek_by(1).clone();

                    match (&val1, &val2) {
                        (value::Value::Number(n1), value::Value::Number(n2)) => {
                            self.pop_stack();
                            self.pop_stack();
                            self.stack.push(value::Value::Number(n1 + n2));
                        }
                        (value::Value::String(s1), value::Value::String(s2)) => {
                            self.pop_stack();
                            self.pop_stack();
                            self.stack
                                .push(value::Value::String(self.heap.manage_str(format!(
                                    "{}{}",
                                    self.get_str(s2),
                                    self.get_str(s1)
                                ))));
                        }
                        _ => {
                            return Err(InterpreterError::Runtime(format!(
                                "invalid operands of type {:?} and {:?} in add expression: \
                                 both operands must be number or string (line={})",
                                value::type_of(&val1),
                                value::type_of(&val2),
                                lineno.value
                            )))
                        }
                    }
                }
                (bytecode::Op::Subtract, lineno) => match self.numeric_binop(Binop::Sub, lineno) {
                    Ok(()) => {}
                    Err(err) => return Err(err),
                },
                (bytecode::Op::Multiply, lineno) => match self.numeric_binop(Binop::Mul, lineno) {
                    Ok(()) => {}
                    Err(err) => return Err(err),
                },
                (bytecode::Op::Divide, lineno) => match self.numeric_binop(Binop::Div, lineno) {
                    Ok(()) => {}
                    Err(err) => return Err(err),
                },
                (bytecode::Op::Not, lineno) => {
                    let top_stack = self.peek();
                    let maybe_bool = Interpreter::extract_bool(top_stack);

                    match maybe_bool {
                        Some(b) => {
                            self.pop_stack();
                            self.stack.push(value::Value::Bool(!b));
                        }
                        None => {
                            return Err(InterpreterError::Runtime(format!(
                                "invalid operand in not expression. Expected boolean, found {:?} at line {}",
                                value::type_of(top_stack), lineno.value)))
                        }
                    }
                }
                (bytecode::Op::Equal, _) => {
                    let val1 = self.pop_stack();
                    let val2 = self.pop_stack();
                    self.stack
                        .push(value::Value::Bool(self.values_equal(&val1, &val2)));
                }
                (bytecode::Op::Greater, lineno) => {
                    let val1 = self.peek_by(0).clone();
                    let val2 = self.peek_by(1).clone();

                    match (&val1, &val2) {
                        (value::Value::Number(n1), value::Value::Number(n2)) => {
                            self.pop_stack();
                            self.pop_stack();

                            self.stack.push(value::Value::Bool(n2 > n1));
                        }
                        _ => return Err(InterpreterError::Runtime(format!(
                            "invalid operands in Greater expression. Expected numbers, found {:?} and {:?} at line {}",
                            value::type_of(&val1), value::type_of(&val2), lineno.value)))

                    }
                }
                (bytecode::Op::Less, lineno) => {
                    let val1 = self.peek_by(0).clone();
                    let val2 = self.peek_by(1).clone();

                    match (&val1, &val2) {
                        (value::Value::Number(n1), value::Value::Number(n2)) => {
                            self.pop_stack();
                            self.pop_stack();
                            self.stack.push(value::Value::Bool(n2 < n1));
                        }
                        _ => return Err(InterpreterError::Runtime(format!(
                            "invalid operands in Less expression. Expected numbers, found {:?} and {:?} at line {}",
                            value::type_of(&val1), value::type_of(&val2), lineno.value)))

                    }
                }
                (bytecode::Op::Print, _) => {
                    let to_print = self.peek().clone();
                    self.print_val(&to_print);
                }
                (bytecode::Op::Pop, _) => {
                    self.pop_stack();
                }
                (bytecode::Op::DefineGlobal(idx), _) => {
                    if let value::Value::String(name) = self.read_constant(idx) {
                        let val = self.pop_stack();
                        self.globals.insert(self.get_str(&name).clone(), val);
                    } else {
                        panic!(
                            "expected string when defining global, found {:?}",
                            value::type_of(&self.read_constant(idx))
                        );
                    }
                }
                (bytecode::Op::GetGlobal(idx), lineno) => {
                    if let value::Value::String(name) = self.read_constant(idx) {
                        match self.globals.get(self.get_str(&name)) {
                            Some(val) => {
                                self.stack.push(val.clone());
                            }
                            None => {
                                return Err(InterpreterError::Runtime(format!(
                                    "Undefined variable '{}' at line {}.",
                                    self.get_str(&name),
                                    lineno.value
                                )));
                            }
                        }
                    } else {
                        panic!(
                            "expected string when defining global, found {:?}",
                            value::type_of(&self.read_constant(idx))
                        );
                    }
                }
                (bytecode::Op::SetGlobal(idx), lineno) => {
                    if let value::Value::String(name) = self.read_constant(idx) {
                        let name_str = self.get_str(&name).clone();
                        if self.globals.contains_key(&name_str) {
                            let val = self.peek().clone();
                            self.globals.insert(name_str, val);
                        } else {
                            return Err(InterpreterError::Runtime(format!(
                                "Use of undefined variable {} in setitem expression at line {}.",
                                name_str, lineno.value
                            )));
                        }
                    } else {
                        panic!(
                            "expected string when setting global, found {:?}",
                            value::type_of(&self.read_constant(idx))
                        );
                    }
                }
                (bytecode::Op::GetLocal(idx), _) => {
                    let slots_offset = self.frame().slots_offset;
                    let val = self.stack[slots_offset + idx].clone();
                    self.stack.push(val);
                }
                (bytecode::Op::SetLocal(idx), _) => {
                    let val = self.peek();
                    let slots_offset = self.frame().slots_offset;
                    self.stack[slots_offset + idx] = val.clone();
                }
                (bytecode::Op::GetUpval(idx), _) => {
                    let upvalue = self.frame().closure.upvalues[idx].clone();
                    let val = match &*upvalue.borrow() {
                        value::Upvalue::Closed(value) => value.clone(),
                        value::Upvalue::Open(stack_index) => self.stack[*stack_index].clone(),
                    };
                    self.stack.push(val);
                }
                (bytecode::Op::SetUpval(idx), _) => {
                    let new_value = self.peek().clone();
                    let upvalue = self.frame().closure.upvalues[idx].clone();
                    match &mut *upvalue.borrow_mut() {
                        value::Upvalue::Closed(value) => *value = new_value,
                        value::Upvalue::Open(stack_index) => self.stack[*stack_index] = new_value,
                    };
                }
                (bytecode::Op::JumpIfFalse(offset), _) => {
                    if self.is_falsey(&self.peek()) {
                        self.frame_mut().ip += offset;
                    }
                }
                (bytecode::Op::Jump(offset), _) => {
                    self.frame_mut().ip += offset;
                }
                (bytecode::Op::Loop(offset), _) => {
                    self.frame_mut().ip -= offset;
                }
                (bytecode::Op::Call(arg_count), _) => {
                    self.call_value(self.peek_by(arg_count.into()).clone(), arg_count)?;
                }
                (bytecode::Op::CloseUpvalue, _) => {
                    let idx = self.stack.len() - 1;
                    self.close_upvalues(idx);
                    self.stack.pop();
                }
            }
        }
    }

    fn close_upvalues(&mut self, index: usize) {
        let value = &self.stack[index];
        for upval in &self.upvalues {
            if upval.borrow().is_open_with_index(index) {
                upval.replace(value::Upvalue::Closed(value.clone()));
            }
        }

        self.upvalues.retain(|u| u.borrow().is_open());
    }

    fn find_open_uval(&self, index: usize) -> Option<Rc<RefCell<value::Upvalue>>> {
        for upval in self.upvalues.iter().rev() {
            if upval.borrow().is_open_with_index(index) {
                return Some(upval.clone());
            }
        }

        None
    }

    fn call_value(
        &mut self,
        val_to_call: value::Value,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        match val_to_call {
            value::Value::Function(func) => {
                self.call(func, arg_count)?;
                Ok(())
            }
            value::Value::NativeFunction(native_func) => {
                self.native_call(native_func, arg_count)?;
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "attempted to call non-callable value of type {:?}.",
                value::type_of(&val_to_call)
            ))),
        }
    }

    fn native_call(
        &mut self,
        native_func: value::NativeFunction,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        if arg_count != native_func.arity {
            return Err(InterpreterError::Runtime(format!(
                "Expected {} arguments but found {}.",
                native_func.arity, arg_count
            )));
        }

        let mut args = Vec::new();
        for _ in 0..arg_count {
            args.push(self.pop_stack()) // pop args
        }
        args.reverse();
        let args = args;
        self.pop_stack(); // native function value

        let res = (native_func.func)(&self.heap, args);

        match res {
            Ok(result) => {
                self.stack.push(result);
                Ok(())
            }
            Err(err) => Err(InterpreterError::Runtime(format!(
                "When calling {}: {}.",
                native_func.name, err
            ))),
        }
    }

    fn call(
        &mut self,
        closure_handle: gc_values::GcClosure,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        let closure = self.get_closure(&closure_handle).clone();
        let func = &closure.function;
        if arg_count != func.arity {
            return Err(InterpreterError::Runtime(format!(
                "Expected {} arguments but found {}.",
                func.arity, arg_count
            )));
        }

        self.frames.push(CallFrame::default());
        let mut frame = self.frames.last_mut().unwrap();
        frame.closure = closure;
        frame.slots_offset = self.stack.len() - usize::from(arg_count);
        Ok(())
    }

    fn pop_stack_n_times(&mut self, num_to_pop: usize) {
        for _ in 0..num_to_pop {
            self.pop_stack();
        }
    }

    fn is_falsey(&self, val: &value::Value) -> bool {
        match val {
            value::Value::Nil => true,
            value::Value::Bool(b) => !*b,
            value::Value::Number(f) => *f == 0.0,
            value::Value::Function(_) => false,
            value::Value::NativeFunction(_) => false,
            value::Value::String(s) => self.get_str(&s).is_empty(),
        }
    }

    fn print_val(&mut self, val: &value::Value) {
        let output = self.format_val(val);
        println!("{}", output);
        self.output.push(output);
    }

    fn values_equal(&self, val1: &value::Value, val2: &value::Value) -> bool {
        match (val1, val2) {
            (value::Value::Number(n1), value::Value::Number(n2)) => (n1 - n2).abs() < f64::EPSILON,
            (value::Value::Bool(b1), value::Value::Bool(b2)) => b1 == b2,
            (value::Value::String(s1), value::Value::String(s2)) => {
                self.get_str(&s1) == self.get_str(&s2)
            }
            (value::Value::Nil, value::Value::Nil) => true,
            (_, _) => false,
        }
    }

    fn numeric_binop(
        &mut self,
        binop: Binop,
        lineno: bytecode::Lineno,
    ) -> Result<(), InterpreterError> {
        let val1 = self.peek_by(0).clone();
        let val2 = self.peek_by(1).clone();

        match (&val1, &val2) {
            (value::Value::Number(n1), value::Value::Number(n2)) => {
                self.pop_stack();
                self.pop_stack();
                self.stack
                    .push(value::Value::Number(Interpreter::apply_numeric_binop(
                        *n2, *n1, binop, // note the order!
                    )));
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "Expected numbers in {:?} expression. Found {:?} and {:?} (line={})",
                binop,
                value::type_of(&val1),
                value::type_of(&val2),
                lineno.value
            ))),
        }
    }

    fn apply_numeric_binop(left: f64, right: f64, binop: Binop) -> f64 {
        match binop {
            Binop::Add => left + right,
            Binop::Sub => left - right,
            Binop::Mul => left * right,
            Binop::Div => left / right,
        }
    }

    fn pop_stack(&mut self) -> value::Value {
        match self.stack.pop() {
            Some(val) => val,
            None => panic!("attempted to pop empty stack!"),
        }
    }

    fn peek(&self) -> &value::Value {
        self.peek_by(0)
    }

    fn peek_by(&self, n: usize) -> &value::Value {
        &self.stack[self.stack.len() - n - 1]
    }

    fn next_op(&mut self) -> (bytecode::Op, bytecode::Lineno) {
        self.frame_mut().next_op()
    }

    fn read_constant(&mut self, idx: usize) -> value::Value {
        let constant = self.frame().read_constant(idx);
        match constant {
            bytecode::Constant::Number(num) => value::Value::Number(num),
            bytecode::Constant::String(s) => value::Value::String(self.heap.manage_str(s)),
            bytecode::Constant::Function(f) => {
                value::Value::Function(self.heap.manage_closure(value::Closure {
                    function: f.function,
                    upvalues: Vec::new(),
                }))
            }
        }
    }

    fn extract_number(val: &value::Value) -> Option<f64> {
        match val {
            value::Value::Number(f) => Some(*f),
            _ => None,
        }
    }

    fn extract_bool(val: &value::Value) -> Option<bool> {
        match val {
            value::Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    fn get_str(&self, str_handle: &gc_values::GcString) -> &String {
        self.heap.get_str(&str_handle)
    }
    fn get_closure(&self, closure_handle: &gc_values::GcClosure) -> &value::Closure {
        self.heap.get_closure(&closure_handle)
    }
}

#[cfg(test)]
mod tests {
    use crate::bytecode_interpreter::*;
    use crate::compiler::*;

    #[test]
    fn test_compiler_1() {
        let func_or_err = Compiler::compile(String::from("print 42 * 12;"));

        match func_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_compiler_2() {
        let func_or_err = Compiler::compile(String::from("print -2 * 3 + (-4 / 2);"));

        match func_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_decl_1() {
        let func_or_err = Compiler::compile(String::from("var x = 2;"));

        match func_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_decl_implicit_nil() {
        let func_or_err = Compiler::compile(String::from("var x;"));

        match func_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_reading_1() {
        let func_or_err = Compiler::compile(String::from("var x = 2; print x;"));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["2"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_reading_locals_1() {
        let func_or_err = Compiler::compile(String::from("{var x = 2; print x;}"));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["2"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_reading_2() {
        let func_or_err = Compiler::compile(String::from("var x; print x;"));

        match func_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_reading_3() {
        let func_or_err = Compiler::compile(String::from("var x; print x * 2 + x;"));

        match func_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_reading_4() {
        let func_or_err = Compiler::compile(String::from(
            "var x = 2;\n\
             var y = 3;\n\
             print x * y + 4;",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["10"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_reading_locals_2() {
        let func_or_err = Compiler::compile(String::from(
            "{\n\
               var x = 2;\n\
               var y = 3;\n\
               print x * y + 4;\n\
             }\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["10"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_div_by_zero() {
        let func_or_err = Compiler::compile(String::from("print 1 / 0;"));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["inf"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_setitem_globals() {
        let func_or_err = Compiler::compile(String::from(
            "var breakfast = \"beignets\";\n\
             var beverage = \"cafe au lait\";\n\
             breakfast = \"beignets with \" + beverage;\n\
             print breakfast;",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["beignets with cafe au lait"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_setitem_locals() {
        let func_or_err = Compiler::compile(String::from(
            "{\n\
               var breakfast = \"beignets\";\n\
               var beverage = \"cafe au lait\";\n\
               breakfast = \"beignets with \" + beverage;\n\
               print breakfast;\n\
             }\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["beignets with cafe au lait"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_setitem_illegal_target_globals() {
        let func_or_err = Compiler::compile(String::from(
            "var x = 2;\n\
             var y = 3;\n\
             x * y = 5;",
        ));

        match func_or_err {
            Ok(_) => panic!("expected compile error"),
            Err(err) => assert!(err.starts_with("Invalid assignment target")),
        }
    }

    #[test]
    fn test_setitem_illegal_target_locals() {
        let func_or_err = Compiler::compile(String::from(
            "{\n\
               var x = 2;\n\
               var y = 3;\n\
               x * y = 5;\n\
             }\n",
        ));

        match func_or_err {
            Ok(_) => panic!("expected compile error"),
            Err(err) => assert!(err.starts_with("Invalid assignment target")),
        }
    }

    #[test]
    fn test_redeclaration_of_locals_is_error() {
        let func_or_err = Compiler::compile(String::from(
            "{\n\
               var x = 2;\n\
               var x = 3;\n\
             }",
        ));

        match func_or_err {
            Ok(_) => panic!("expected compile error"),
            Err(err) => assert!(err.starts_with("Redeclaration of variable")),
        }
    }

    #[test]
    fn test_read_in_own_initializer() {
        let func_or_err = Compiler::compile(String::from(
            "{\n\
               var a = \"outer\";\n\
               {\n\
                 var a = a;\n\
               }\n\
             }\n",
        ));

        match func_or_err {
            Ok(_) => panic!("expected compile error"),
            Err(err) => {
                assert!(err.starts_with("Cannot read local variable in its own initializer."))
            }
        }
    }

    #[test]
    fn test_if_stmt() {
        let func_or_err = Compiler::compile(String::from(
            "var x = 0;\n\
             var y = 1;\n\
             if (x) {\n\
               print x;\n\
             }\n\
             if (y) {\n\
               print y;\n\
             }",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["1"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_if_then_else_1() {
        let func_or_err = Compiler::compile(String::from(
            "var x = 0;\n\
             if (x) {\n\
               print \"hello\";\n\
             } else {\n\
               print \"goodbye\";\n\
             }",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["goodbye"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_if_then_else_2() {
        let func_or_err = Compiler::compile(String::from(
            "var x = 1;\n\
             if (x) {\n\
               print \"hello\";\n\
             } else {\n\
               print \"goodbye\";\n\
             }",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["hello"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_print_locals() {
        let func_or_err = Compiler::compile(String::from(
            "{\n\
               var x = 0;\n\
               var y = 1;\n\
               print x;\n\
               print y;\n\
             }",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["0", "1"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_print_globals() {
        let func_or_err = Compiler::compile(String::from(
            "var x = 0;\n\
             var y = 1;\n\
             print x;\n\
             print y;\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["0", "1"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_and_1() {
        let func_or_err = Compiler::compile(String::from(
            "var x = false;\n\
             var y = true;\n\
             if (y and x) {\n\
               print \"cat\";\n\
             } else {\n\
               print \"dog\";\n\
             }\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["dog"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_and_2() {
        let func_or_err = Compiler::compile(String::from(
            "var x = false;\n\
             var y = true;\n\
             if (x and y) {\n\
               print \"cat\";\n\
             } else {\n\
               print \"dog\";\n\
             }\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["dog"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_and_3() {
        let func_or_err = Compiler::compile(String::from(
            "var x = true;\n\
             var y = true;\n\
             if (y and x) {\n\
               print \"cat\";\n\
             } else {\n\
               print \"dog\";\n\
             }\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["cat"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_or_1() {
        let func_or_err = Compiler::compile(String::from(
            "var x = false;\n\
             var y = true;\n\
             if (y or x) {\n\
               print \"cat\";\n\
             } else {\n\
               print \"dog\";\n\
             }\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["cat"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_or_2() {
        let func_or_err = Compiler::compile(String::from(
            "var x = false;\n\
             var y = true;\n\
             if (x or y) {\n\
               print \"cat\";\n\
             } else {\n\
               print \"dog\";\n\
             }\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["cat"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_or_3() {
        let func_or_err = Compiler::compile(String::from(
            "var x = false;\n\
             var y = false;\n\
             if (y or x) {\n\
               print \"cat\";\n\
             } else {\n\
               print \"dog\";\n\
             }\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["dog"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_while() {
        let func_or_err = Compiler::compile(String::from(
            "{var x = 0;\n\
             var sum = 0;\n\
             while (x < 100) {\n\
               x = x + 1;\n\
               sum = sum + x;\n\
             }\n\
             print sum;}",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["5050"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_for() {
        let func_or_err = Compiler::compile(String::from(
            "{\n\
               var fact = 1;\n\
               for (var i = 1; i <= 10; i = i + 1) {\n\
                 fact = fact * i;\n\
               }\n\
               print fact;\n\
             }",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["3628800"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_functions_1() {
        let func_or_err = Compiler::compile(String::from(
            "fun areWeHavingItYet() {\n\
               print \"Yes we are!\";\n\
             }\n\
             \n\
             print areWeHavingItYet;\n",
        ));

        match func_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_functions_2() {
        let func_or_err = Compiler::compile(String::from(
            "fun f(x, y) {\n\
               print x + y;\n\
             }\n\
             \n\
             print f;\n",
        ));

        match func_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_functions_3() {
        let func_or_err = Compiler::compile(String::from(
            "fun f(x, y) {\n\
               return x + y;\n\
             }\n\
             \n\
             print f;\n",
        ));

        match func_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_functions_4() {
        let func_or_err = Compiler::compile(String::from(
            "fun f() {\n\
               return;\n\
             }\n\
             \n\
             print f();\n",
        ));

        match func_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_functions_5() {
        let func_or_err = Compiler::compile(String::from("return 42;"));

        match func_or_err {
            Ok(_) => panic!(),
            Err(err) => assert_eq!(err, "Cannot return from top-level code."),
        }
    }

    #[test]
    fn test_functions_6() {
        let func_or_err = Compiler::compile(String::from(
            "fun f(x, y) {\n\
               return x + y;\n\
             }\n\
             \n\
             print f(1,2);\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["3"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_functions_7() {
        let func_or_err = Compiler::compile(String::from(
            "fun g(x) {\n\
               return 2 * x;\n\
             }\n\
             \n\
             fun f(x, y) {\n\
               return g(x) + y;\n\
             }\n\
             \n\
             print f(1,2);\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["4"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_functions_8() {
        let func_or_err = Compiler::compile(String::from(
            "var x = 2;\n\
             fun f(x) {\n\
               print 2 * x;\n\
             }\n\
             \n\
             f(x);\n\
             print x;\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["4", "2"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_functions_9() {
        fn fact(n: i32) -> i32 {
            if n <= 1 {
                return 1;
            }
            return n * fact(n - 1);
        };

        let func_or_err = Compiler::compile(String::from(
            "fun fact(n) {\n\
               if (n <= 1) { return 1; }\n\
               return n * fact(n - 1);\n\
             }\n\
             \n\
             print fact(10);\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec![format!("{}", fact(10))]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_functions_10() {
        let func_or_err = Compiler::compile(String::from(
            "fun isEven(n) {\n\
               if (n = 0) { return true; }\n\
               return isOdd(n - 1);\n\
             }\n\
             fun isOdd(n) {\n\
               if (n = 1) { return true; }\n\
               return isEven(n - 1);\n\
             }\n\
             \n\
             print isEven(10);\n",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["true"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_native_functions() {
        let func_or_err = Compiler::compile(String::from(
            "fun fib(n) {\n\
               if (n < 2) return n;\n\
               return fib(n - 2) + fib(n - 1);\n\
             }\n\
             \n\
             var start = clock();\n\
             print fib(5);\n\
             print clock() - start;\n\
             print 42;",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output.len(), 3);
                        assert_eq!(interp.output[0], "5");
                        assert_eq!(interp.output[2], "42");
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_get_upval_on_stack() {
        let func_or_err = Compiler::compile(String::from(
            "fun outer() {\n\
               var x = \"outside\";\n\
               fun inner() {\n\
                 print x;\n\
               }\n\
               inner();\n\
             }\n\
             outer();",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["outside"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_set_upval_on_stack() {
        let func_or_err = Compiler::compile(String::from(
            "fun outer() {\n\
               var x = \"before\";\n\
               fun inner() {\n\
                 x = \"assigned\";\n\
               }\n\
               inner();\n\
               print x;\n\
             }\n\
             outer();",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["assigned"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_closing_upvals_after_return() {
        let func_or_err = Compiler::compile(String::from(
            "fun outer() {\n\
               var x = \"outside\";\n\
               fun inner() {\n\
                 print x;\n\
               }\n\
               \n\
               return inner;\n\
            }\n\
            \n\
            var closure = outer();\n\
            closure();",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["outside"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_closing_upvals_after_scope() {
        let func_or_err = Compiler::compile(String::from(
            "var closure;\n\
             {\n\
               var x = \"outside\";\n\
               fun inner() {\n\
                 print x;\n\
               }\n\
               \n\
               closure = inner;\n\
            }\n\
            \n\
            closure();",
        ));

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => {
                        assert_eq!(interp.output, vec!["outside"]);
                    }
                    Err(err) => {
                        panic!("{:?}", err);
                    }
                }
            }
            Err(err) => panic!(err),
        }
    }
}
