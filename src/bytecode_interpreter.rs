use std::collections::HashMap;

use crate::builtins;
use crate::bytecode;

#[allow(dead_code)]
pub fn disassemble_chunk(chunk: &bytecode::Chunk, name: &str) {
    println!("============ {} ============", name);

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
            bytecode::Op::Closure(idx) => {
                format!("OP_CLOSURE {:?} (idx={})", chunk.constants[*idx], *idx)
            }
        };

        println!(
            "{0: <04}   {1: <30} {2: <30}",
            idx,
            formatted_op,
            format!("line {}", lineno.value)
        );
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
    stack: Vec<bytecode::Value>,
    output: Vec<String>,
    globals: HashMap<String, bytecode::Value>,
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        let mut res = Interpreter {
            frames: Vec::new(),
            stack: Vec::new(),
            output: Vec::new(),
            globals: HashMap::new(),
        };
        res.stack.reserve(256);
        res.frames.reserve(64);

        res.globals.insert(
            String::from("clock"),
            bytecode::Value::NativeFunction(bytecode::NativeFunction {
                arity: 0,
                name: String::from("clock"),
                func: builtins::clock,
            }),
        );
        res.globals.insert(
            String::from("exp"),
            bytecode::Value::NativeFunction(bytecode::NativeFunction {
                arity: 1,
                name: String::from("exp"),
                func: builtins::exp,
            }),
        );
        res.globals.insert(
            String::from("sqrt"),
            bytecode::Value::NativeFunction(bytecode::NativeFunction {
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
    closure: bytecode::Closure,
    ip: usize,
    slots_offset: usize,
}

impl CallFrame {
    fn next_op(&mut self) -> (bytecode::Op, bytecode::Lineno) {
        let res = self.closure.function.chunk.code[self.ip];
        self.ip += 1;
        res
    }

    fn read_constant(&self, idx: usize) -> &bytecode::Value {
        &self.closure.function.chunk.constants[idx]
    }
}

impl Interpreter {
    pub fn interpret(&mut self, func: bytecode::Function) -> Result<(), InterpreterError> {
        self.stack
            .push(bytecode::Value::Function(bytecode::Closure {
                function: func.clone(),
            }));
        self.frames.push(CallFrame {
            closure: bytecode::Closure { function: func },
            ip: 0,
            slots_offset: 1,
        });
        self.run()
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
            if self.frames.len() == 0
                || self.frame().ip >= self.frame().closure.function.chunk.code.len()
            {
                return Ok(());
            }

            let op = self.next_op();

            match op {
                (bytecode::Op::Return, _) => {
                    let result = self.pop_stack();

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
                (bytecode::Op::Closure(idx), _) => {
                    let constant = self.read_constant(idx).clone();

                    if let bytecode::Value::Function(closure) = constant {
                        self.stack.push(bytecode::Value::Function(closure));
                    } else {
                        panic!(
                            "When interpreting bytecode::Op::Closure, expected function, found {:?}",
                            bytecode::type_of(&constant)
                        );
                    }
                }
                (bytecode::Op::Constant(idx), _) => {
                    let constant = self.read_constant(idx).clone();
                    self.stack.push(constant);
                }
                (bytecode::Op::Nil, _) => {
                    self.stack.push(bytecode::Value::Nil);
                }
                (bytecode::Op::True, _) => {
                    self.stack.push(bytecode::Value::Bool(true));
                }
                (bytecode::Op::False, _) => {
                    self.stack.push(bytecode::Value::Bool(false));
                }
                (bytecode::Op::Negate, lineno) => {
                    let top_stack = self.peek();
                    let maybe_number = Interpreter::extract_number(top_stack);

                    match maybe_number {
                        Some(to_negate) => {
                            self.pop_stack();
                            self.stack.push(bytecode::Value::Number(-to_negate));
                        }
                        None => {
                            return Err(InterpreterError::Runtime(format!(
                                "invalid operand to unary op negate. Expected number, found {:?} at line {}",
                                bytecode::type_of(top_stack), lineno.value
                            )))
                        }
                    }
                }
                (bytecode::Op::Add, lineno) => {
                    let val1 = self.peek_by(0).clone();
                    let val2 = self.peek_by(1).clone();

                    match (&val1, &val2) {
                        (bytecode::Value::Number(n1), bytecode::Value::Number(n2)) => {
                            self.pop_stack();
                            self.pop_stack();
                            self.stack.push(bytecode::Value::Number(n1 + n2));
                        }
                        (bytecode::Value::String(s1), bytecode::Value::String(s2)) => {
                            self.pop_stack();
                            self.pop_stack();
                            self.stack
                                .push(bytecode::Value::String(format!("{}{}", s2, s1)));
                        }
                        _ => {
                            return Err(InterpreterError::Runtime(format!(
                                "invalid operands of type {:?} and {:?} in add expression: \
                                 both operands must be number or string (line={})",
                                bytecode::type_of(&val1),
                                bytecode::type_of(&val2),
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
                            self.stack.push(bytecode::Value::Bool(!b));
                        }
                        None => {
                            return Err(InterpreterError::Runtime(format!(
                                "invalid operand in not expression. Expected boolean, found {:?} at line {}",
                                bytecode::type_of(top_stack), lineno.value)))
                        }
                    }
                }
                (bytecode::Op::Equal, _) => {
                    let val1 = self.pop_stack();
                    let val2 = self.pop_stack();
                    self.stack
                        .push(bytecode::Value::Bool(Interpreter::values_equal(
                            &val1, &val2,
                        )));
                }
                (bytecode::Op::Greater, lineno) => {
                    let val1 = self.peek_by(0).clone();
                    let val2 = self.peek_by(1).clone();

                    match (&val1, &val2) {
                        (bytecode::Value::Number(n1), bytecode::Value::Number(n2)) => {
                            self.pop_stack();
                            self.pop_stack();

                            self.stack.push(bytecode::Value::Bool(n2 > n1));
                        }
                        _ => return Err(InterpreterError::Runtime(format!(
                            "invalid operands in Greater expression. Expected numbers, found {:?} and {:?} at line {}",
                            bytecode::type_of(&val1), bytecode::type_of(&val2), lineno.value)))

                    }
                }
                (bytecode::Op::Less, lineno) => {
                    let val1 = self.peek_by(0).clone();
                    let val2 = self.peek_by(1).clone();

                    match (&val1, &val2) {
                        (bytecode::Value::Number(n1), bytecode::Value::Number(n2)) => {
                            self.pop_stack();
                            self.pop_stack();
                            self.stack.push(bytecode::Value::Bool(n2 < n1));
                        }
                        _ => return Err(InterpreterError::Runtime(format!(
                            "invalid operands in Less expression. Expected numbers, found {:?} and {:?} at line {}",
                            bytecode::type_of(&val1), bytecode::type_of(&val2), lineno.value)))

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
                    if let bytecode::Value::String(name) = self.read_constant(idx).clone() {
                        let val = self.pop_stack();
                        self.globals.insert(name, val);
                    } else {
                        panic!(
                            "expected string when defining global, found {:?}",
                            bytecode::type_of(self.read_constant(idx))
                        );
                    }
                }
                (bytecode::Op::GetGlobal(idx), lineno) => {
                    if let bytecode::Value::String(name) = self.read_constant(idx) {
                        match self.globals.get(name) {
                            Some(val) => {
                                self.stack.push(val.clone());
                            }
                            None => {
                                return Err(InterpreterError::Runtime(format!(
                                    "Undefined variable '{}' at line {}.",
                                    name, lineno.value
                                )));
                            }
                        }
                    } else {
                        panic!(
                            "expected string when defining global, found {:?}",
                            bytecode::type_of(self.read_constant(idx))
                        );
                    }
                }
                (bytecode::Op::SetGlobal(idx), lineno) => {
                    if let bytecode::Value::String(name) = self.read_constant(idx).clone() {
                        if self.globals.contains_key(&name) {
                            let val = self.peek().clone();
                            self.globals.insert(name, val);
                        } else {
                            return Err(InterpreterError::Runtime(format!(
                                "Use of undefined variable {} in setitem expression at line {}.",
                                name, lineno.value
                            )));
                        }
                    } else {
                        panic!(
                            "expected string when setting global, found {:?}",
                            bytecode::type_of(self.read_constant(idx))
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
                (bytecode::Op::GetUpval(_), _) => {
                    unimplemented!();
                }
                (bytecode::Op::SetUpval(_), _) => {
                    unimplemented!();
                }
                (bytecode::Op::JumpIfFalse(offset), _) => {
                    if Interpreter::is_falsey(&self.peek()) {
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
            }
        }
    }

    fn call_value(
        &mut self,
        val_to_call: bytecode::Value,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        match val_to_call {
            bytecode::Value::Function(func) => {
                self.call(func, arg_count)?;
                Ok(())
            }
            bytecode::Value::NativeFunction(native_func) => {
                self.native_call(native_func, arg_count)?;
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "attempted to call non-callable value of type {:?}.",
                bytecode::type_of(&val_to_call)
            ))),
        }
    }

    fn native_call(
        &mut self,
        native_func: bytecode::NativeFunction,
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

        match (native_func.func)(args) {
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

    fn call(&mut self, closure: bytecode::Closure, arg_count: u8) -> Result<(), InterpreterError> {
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

    fn is_falsey(val: &bytecode::Value) -> bool {
        match val {
            bytecode::Value::Nil => true,
            bytecode::Value::Bool(b) => !*b,
            bytecode::Value::Number(f) => *f == 0.0,
            bytecode::Value::Function(_) => false,
            bytecode::Value::NativeFunction(_) => false,
            bytecode::Value::String(s) => s.is_empty(),
        }
    }

    fn print_val(&mut self, val: &bytecode::Value) {
        let output = format!("{:?}", val);
        println!("{}", output);
        self.output.push(output);
    }

    fn values_equal(val1: &bytecode::Value, val2: &bytecode::Value) -> bool {
        match (val1, val2) {
            (bytecode::Value::Number(n1), bytecode::Value::Number(n2)) => {
                (n1 - n2).abs() < f64::EPSILON
            }
            (bytecode::Value::Bool(b1), bytecode::Value::Bool(b2)) => b1 == b2,
            (bytecode::Value::String(s1), bytecode::Value::String(s2)) => s1 == s2,
            (bytecode::Value::Nil, bytecode::Value::Nil) => true,
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
            (bytecode::Value::Number(n1), bytecode::Value::Number(n2)) => {
                self.pop_stack();
                self.pop_stack();
                self.stack
                    .push(bytecode::Value::Number(Interpreter::apply_numeric_binop(
                        *n2, *n1, binop, // note the order!
                    )));
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "Expected numbers in {:?} expression. Found {:?} and {:?} (line={})",
                binop,
                bytecode::type_of(&val1),
                bytecode::type_of(&val2),
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

    fn pop_stack(&mut self) -> bytecode::Value {
        match self.stack.pop() {
            Some(val) => val,
            None => panic!("attempted to pop empty stack!"),
        }
    }

    fn peek(&self) -> &bytecode::Value {
        self.peek_by(0)
    }

    fn peek_by(&self, n: usize) -> &bytecode::Value {
        &self.stack[self.stack.len() - n - 1]
    }

    fn next_op(&mut self) -> (bytecode::Op, bytecode::Lineno) {
        self.frame_mut().next_op()
    }

    fn read_constant(&self, idx: usize) -> &bytecode::Value {
        &self.frame().read_constant(idx)
    }

    fn extract_number(val: &bytecode::Value) -> Option<f64> {
        match val {
            bytecode::Value::Number(f) => Some(*f),
            _ => None,
        }
    }

    fn extract_bool(val: &bytecode::Value) -> Option<bool> {
        match val {
            bytecode::Value::Bool(b) => Some(*b),
            _ => None,
        }
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
}
