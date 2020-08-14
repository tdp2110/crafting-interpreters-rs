use std::collections::HashMap;

use crate::bytecode;
use crate::value;

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
            bytecode::Op::Return => format!("OP_RETURN"),
            bytecode::Op::Constant(const_idx) => format!(
                "OP_CONSTANT {:?} (idx={})",
                chunk.constants[*const_idx], *const_idx
            ),
            bytecode::Op::Nil => format!("OP_NIL"),
            bytecode::Op::True => format!("OP_TRUE"),
            bytecode::Op::False => format!("OP_FALSE"),
            bytecode::Op::Negate => format!("OP_NEGATE"),
            bytecode::Op::Add => format!("OP_ADD"),
            bytecode::Op::Subtract => format!("OP_SUBTRACT"),
            bytecode::Op::Multiply => format!("OP_MULTIPLY"),
            bytecode::Op::Divide => format!("OP_DIVIDE"),
            bytecode::Op::Not => format!("OP_NOT"),
            bytecode::Op::Equal => format!("OP_NOT"),
            bytecode::Op::Greater => format!("OP_GREATER"),
            bytecode::Op::Less => format!("OP_LESS"),
            bytecode::Op::Print => format!("OP_PRINT"),
            bytecode::Op::Pop => format!("OP_POP"),
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
            bytecode::Op::JumpIfFalse(loc) => format!("OP_JUMP_IF_FALSE {}", *loc),
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
    chunk: bytecode::Chunk,
    ip: usize,
    stack: Vec<value::Value>,
    output: Vec<String>,
    globals: HashMap<String, value::Value>,
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        let mut res = Interpreter {
            chunk: Default::default(),
            ip: 0,
            stack: Vec::new(),
            output: Vec::new(),
            globals: HashMap::new(),
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
    pub fn interpret(&mut self, chunk: bytecode::Chunk) -> Result<(), InterpreterError> {
        self.chunk = chunk;
        self.run()
    }

    fn run(&mut self) -> Result<(), InterpreterError> {
        loop {
            if self.ip >= self.chunk.code.len() {
                return Ok(());
            }

            let op = self.next_op();

            match op {
                (bytecode::Op::Return, _) => {
                    return Ok(());
                }
                (bytecode::Op::Constant(idx), _) => {
                    let constant = self.read_constant(idx).clone();
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
                                .push(value::Value::String(format!("{}{}", s2, s1)));
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
                        .push(value::Value::Bool(Interpreter::values_equal(&val1, &val2)));
                }
                (bytecode::Op::Greater, lineno) => {
                    let val1 = self.peek_by(0).clone();
                    let val2 = self.peek_by(1).clone();

                    match (&val1, &val2) {
                        (value::Value::Number(n1), value::Value::Number(n2)) => {
                            self.pop_stack();
                            self.pop_stack();
                            self.stack.push(value::Value::Bool(n1 > n2));
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
                            self.stack.push(value::Value::Bool(n1 < n2));
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
                    if let value::Value::String(name) = self.read_constant(idx).clone() {
                        let val = self.pop_stack();
                        self.globals.insert(name, val);
                    } else {
                        panic!(
                            "expected string when defining global, found {:?}",
                            value::type_of(self.read_constant(idx))
                        );
                    }
                }
                (bytecode::Op::GetGlobal(idx), lineno) => {
                    if let value::Value::String(name) = self.read_constant(idx) {
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
                            value::type_of(self.read_constant(idx))
                        );
                    }
                }
                (bytecode::Op::SetGlobal(idx), lineno) => {
                    if let value::Value::String(name) = self.read_constant(idx).clone() {
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
                            value::type_of(self.read_constant(idx))
                        );
                    }
                }
                (bytecode::Op::GetLocal(idx), _) => {
                    let val = self.stack[idx].clone();
                    self.stack.push(val);
                }
                (bytecode::Op::SetLocal(idx), _) => {
                    let val = self.peek();
                    self.stack[idx] = val.clone();
                }
                (bytecode::Op::JumpIfFalse(offset), _) => {
                    if Interpreter::is_falsey(&self.peek()) {
                        self.ip += offset;
                    }
                }
            }
        }
    }

    fn is_falsey(val: &value::Value) -> bool {
        match val {
            value::Value::Nil => false,
            value::Value::Bool(b) => *b,
            value::Value::Number(f) => *f == 0.0,
            value::Value::String(s) => s.is_empty(),
        }
    }

    fn print_val(&mut self, val: &value::Value) {
        let output = match val {
            value::Value::Number(n) => format!("{}", n),
            value::Value::Bool(b) => format!("{}", b),
            value::Value::String(s) => format!("{}", s),
            value::Value::Nil => format!("nil"),
        };
        println!("{}", output);
        self.output.push(output);
    }

    fn values_equal(val1: &value::Value, val2: &value::Value) -> bool {
        match (val1, val2) {
            (value::Value::Number(n1), value::Value::Number(n2)) => (n1 - n2).abs() < f64::EPSILON,
            (value::Value::Bool(b1), value::Value::Bool(b2)) => b1 == b2,
            (value::Value::String(s1), value::Value::String(s2)) => s1 == s2,
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
        let res = self.chunk.code[self.ip];
        self.ip += 1;
        res
    }

    fn read_constant(&self, idx: usize) -> &value::Value {
        &self.chunk.constants[idx]
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
}

#[cfg(test)]
mod tests {
    use crate::bytecode_interpreter::*;
    use crate::compiler::*;

    #[test]
    fn test_interpret_handcoded_bytecode() {
        let code = bytecode::Chunk {
            code: vec![
                (bytecode::Op::Constant(0), bytecode::Lineno(42)),
                (bytecode::Op::Constant(1), bytecode::Lineno(42)),
                (bytecode::Op::Add, bytecode::Lineno(42)),
                (bytecode::Op::Constant(2), bytecode::Lineno(42)),
                (bytecode::Op::Divide, bytecode::Lineno(42)),
                (bytecode::Op::Return, bytecode::Lineno(42)),
            ],
            constants: vec![
                value::Value::Number(1.2),
                value::Value::Number(3.4),
                value::Value::Number(5.6),
            ],
        };

        let res = Interpreter::default().interpret(code);

        assert!(res.is_ok())
    }

    #[test]
    fn test_compiler_1() {
        let code_or_err = Compiler::default().compile(String::from("print 42 * 12;"));

        match code_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_compiler_2() {
        let code_or_err = Compiler::default().compile(String::from("print -2 * 3 + (-4 / 2);"));

        match code_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_decl_1() {
        let code_or_err = Compiler::default().compile(String::from("var x = 2;"));

        match code_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_decl_implicit_nil() {
        let code_or_err = Compiler::default().compile(String::from("var x;"));

        match code_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_reading_1() {
        let code_or_err = Compiler::default().compile(String::from("var x = 2; print x;"));

        match code_or_err {
            Ok(code) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(code);
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
        let code_or_err = Compiler::default().compile(String::from("{var x = 2; print x;}"));

        match code_or_err {
            Ok(code) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(code);
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
        let code_or_err = Compiler::default().compile(String::from("var x; print x;"));

        match code_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_reading_3() {
        let code_or_err = Compiler::default().compile(String::from("var x; print x * 2 + x;"));

        match code_or_err {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_var_reading_4() {
        let code_or_err = Compiler::default().compile(String::from(
            "var x = 2;\n\
             var y = 3;\n\
             print x * y + 4;",
        ));

        match code_or_err {
            Ok(code) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(code);
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
        let code_or_err = Compiler::default().compile(String::from(
            "{\n\
               var x = 2;\n\
               var y = 3;\n\
               print x * y + 4;\n\
             }\n",
        ));

        match code_or_err {
            Ok(code) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(code);
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
        let code_or_err = Compiler::default().compile(String::from("print 1 / 0;"));

        match code_or_err {
            Ok(code) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(code);
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
        let code_or_err = Compiler::default().compile(String::from(
            "var breakfast = \"beignets\";\n\
             var beverage = \"cafe au lait\";\n\
             breakfast = \"beignets with \" + beverage;\n\
             print breakfast;",
        ));

        match code_or_err {
            Ok(code) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(code);
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
        let code_or_err = Compiler::default().compile(String::from(
            "{\n\
               var breakfast = \"beignets\";\n\
               var beverage = \"cafe au lait\";\n\
               breakfast = \"beignets with \" + beverage;\n\
               print breakfast;\n\
             }\n",
        ));

        match code_or_err {
            Ok(code) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(code);
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
        let code_or_err = Compiler::default().compile(String::from(
            "var x = 2;\n\
             var y = 3;\n\
             x * y = 5;",
        ));

        match code_or_err {
            Ok(_) => panic!("expected compile error"),
            Err(err) => assert!(err.starts_with("Invalid assignment target")),
        }
    }

    #[test]
    fn test_setitem_illegal_target_locals() {
        let code_or_err = Compiler::default().compile(String::from(
            "{\n\
               var x = 2;\n\
               var y = 3;\n\
               x * y = 5;\n\
             }\n",
        ));

        match code_or_err {
            Ok(_) => panic!("expected compile error"),
            Err(err) => assert!(err.starts_with("Invalid assignment target")),
        }
    }

    #[test]
    fn test_redeclaration_of_locals_is_error() {
        let code_or_err = Compiler::default().compile(String::from(
            "{\n\
               var x = 2;\n\
               var x = 3;\n\
             }",
        ));

        match code_or_err {
            Ok(_) => panic!("expected compile error"),
            Err(err) => assert!(err.starts_with("Redeclaration of variable")),
        }
    }

    #[test]
    fn test_read_in_own_initializer() {
        let code_or_err = Compiler::default().compile(String::from(
            "{\n\
               var a = \"outer\";\n\
               {\n\
                 var a = a;\n\
               }\n\
             }\n",
        ));

        match code_or_err {
            Ok(_) => panic!("expected compile error"),
            Err(err) => {
                assert!(err.starts_with("Cannot read local variable in its own initializer."))
            }
        }
    }

    #[test]
    fn test_if_stmt() {
        let code_or_err = Compiler::default().compile(String::from(
            "var x = 0;\n\
             var y = 1;\n\
             if (x) {\n\
               print x;\n\
             }\n\
             if (y) {\n\
               print y;\n\
             }",
        ));

        match code_or_err {
            Ok(code) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(code);
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
    fn test_print_locals() {
        let code_or_err = Compiler::default().compile(String::from(
            "{\n\
               var x = 0;\n\
               var y = 1;\n\
               print x;\n\
               print y;\n\
             }",
        ));

        match code_or_err {
            Ok(code) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(code);
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
        let code_or_err = Compiler::default().compile(String::from(
            "var x = 0;\n\
             var y = 1;\n\
             print x;\n\
             print y;\n",
        ));

        match code_or_err {
            Ok(code) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(code);
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
}
