use crate::builtins;
use crate::bytecode;
use crate::gc;
use crate::value;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub fn disassemble_code(chunk: &bytecode::Chunk) -> Vec<String> {
    let mut lines: Vec<String> = Vec::new();

    for (idx, (op, lineno)) in chunk.code.iter().enumerate() {
        let formatted_op = match op {
            bytecode::Op::Return => "OP_RETURN".to_string(),
            bytecode::Op::Constant(const_idx) => format!(
                "OP_CONSTANT {} (idx={})",
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
            bytecode::Op::Closure(idx, _) => format!("OP_CLOSURE {}", chunk.constants[*idx],),
            bytecode::Op::CloseUpvalue => "OP_CLOSE_UPVALUE".to_string(),
            bytecode::Op::Class(idx) => format!("OP_CLASS {}", idx),
            bytecode::Op::SetProperty(idx) => format!("OP_SET_PROPERTY {}", idx),
            bytecode::Op::GetProperty(idx) => format!("OP_GET_PROPERTY {}", idx),
            bytecode::Op::Method(idx) => format!("OP_METHOD {}", idx),
            bytecode::Op::Invoke(method_name, arg_count) => {
                format!("OP_INVOKE {} nargs={}", method_name, arg_count)
            }
            bytecode::Op::Inherit => "OP_INHERIT".to_string(),
            bytecode::Op::GetSuper(idx) => format!("OP_GET_SUPER {}", idx),
            bytecode::Op::SuperInvoke(method_name, arg_count) => {
                format!("OP_SUPER_INOKE {} nargs={}", method_name, arg_count)
            }
            bytecode::Op::BuildList(size) => format!("OP_BUILD_LIST {}", size),
            bytecode::Op::Subscr => "OP_SUBSCR".to_string(),
            bytecode::Op::SetItem => "OP_SETITEM".to_string(),
        };

        lines.push(format!(
            "{0: <04}   {1: <50} {2: <50}",
            idx,
            formatted_op,
            format!("line {}", lineno.value)
        ));
    }
    lines
}

pub fn disassemble_chunk(chunk: &bytecode::Chunk, name: &str) -> String {
    let mut lines: Vec<String> = Vec::new();

    if !name.is_empty() {
        lines.push(format!("============ {} ============", name));
    }

    lines.push("------------ constants -----------".to_string());
    for (idx, constant) in chunk.constants.iter().enumerate() {
        lines.push(format!("{:<4} {}", idx, constant));
    }

    lines.push("\n------------ code -----------------".to_string());

    for code_line in disassemble_code(&chunk) {
        lines.push(code_line)
    }

    lines.join("\n")
}

fn dis_builtin(interp: &mut Interpreter, args: &[value::Value]) -> Result<value::Value, String> {
    // arity checking is done in the interpreter
    match &args[0] {
        value::Value::Function(closure_handle) => {
            let closure = interp.heap.get_closure(*closure_handle);
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
    pub frames: Vec<CallFrame>,
    pub stack: Vec<value::Value>,
    output: Vec<String>,
    pub globals: HashMap<String, value::Value>,
    pub upvalues: Vec<Rc<RefCell<value::Upvalue>>>,
    pub heap: gc::Heap,
    gray_stack: Vec<gc::HeapId>,
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
            gray_stack: Default::default(),
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
        res.globals.insert(
            String::from("len"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 1,
                name: String::from("len"),
                func: builtins::len,
            }),
        );
        res.globals.insert(
            String::from("forEach"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 2,
                name: String::from("forEach"),
                func: builtins::for_each,
            }),
        );
        res.globals.insert(
            String::from("map"),
            value::Value::NativeFunction(value::NativeFunction {
                arity: 2,
                name: String::from("map"),
                func: builtins::map,
            }),
        );

        res
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum InterpreterError {
    Runtime(String),
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InterpreterError::Runtime(err) => write!(f, "Lox runtime error: {}", err),
        }
    }
}

#[derive(Default)]
pub struct CallFrame {
    pub closure: value::Closure,
    pub ip: usize,
    pub slots_offset: usize,
}

impl CallFrame {
    fn next_op(&self) -> (bytecode::Op, bytecode::Lineno) {
        self.closure.function.chunk.code[self.ip].clone()
    }

    fn next_op_and_advance(&mut self) -> (bytecode::Op, bytecode::Lineno) {
        let res = self.next_op();
        self.ip += 1;
        res
    }

    fn read_constant(&self, idx: usize) -> bytecode::Constant {
        self.closure.function.chunk.constants[idx].clone()
    }
}

impl Interpreter {
    pub fn prepare_interpret(&mut self, func: bytecode::Function) {
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
    }

    pub fn interpret(&mut self, func: bytecode::Function) -> Result<(), InterpreterError> {
        self.prepare_interpret(func);
        self.run()
    }

    pub fn format_backtrace(&self) -> String {
        let lines: Vec<_> = self
            .frames
            .iter()
            .map(|frame| {
                let frame_name = &frame.closure.function.name;
                let (_, lineno) = frame.closure.function.chunk.code[frame.ip];
                if frame_name.is_empty() {
                    format!("[line {}] in script", lineno.value)
                } else {
                    format!("[line {}] in {}()", lineno.value, frame_name)
                }
            })
            .collect();
        format!("Backtrace (most recent call last):\n\n{}", lines.join("\n"))
    }

    pub fn format_upval(&self, val: &value::Upvalue) -> String {
        match val {
            value::Upvalue::Open(idx) => format!("Open({})", idx),
            value::Upvalue::Closed(val) => format!("Closed({})", self.format_val(&val)),
        }
    }

    pub fn format_val(&self, val: &value::Value) -> String {
        match val {
            value::Value::Number(num) => num.to_string(),
            value::Value::Bool(b) => b.to_string(),
            value::Value::String(str_handle) => self.get_str(*str_handle).clone(),
            value::Value::Function(closure_handle) => {
                format!("<fn '{}'>", self.get_closure(*closure_handle).function.name)
            }
            value::Value::Class(class_handle) => {
                format!("<class '{}'>", self.get_class(*class_handle).name)
            }
            value::Value::Instance(instance_handle) => {
                let instance = self.get_instance(*instance_handle);
                let class_name = &self.get_class(instance.class_id).name;
                format!("<{} instance>", class_name)
            }
            value::Value::NativeFunction(func) => format!("<native fn {}>", func.name),
            value::Value::BoundMethod(bound_method_id) => {
                let bound_method = self.get_bound_method(*bound_method_id);
                let instance = self.get_instance(bound_method.instance_id);
                let class_name = &self.get_class(instance.class_id).name;
                format!("<bound method of {} instance>", class_name)
            }
            value::Value::Nil => "nil".to_string(),
            value::Value::List(list_id) => {
                let elements = self.get_list_elements(*list_id);
                format!(
                    "[{}]",
                    elements
                        .iter()
                        .map(|element| self.format_val(element))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }

    fn run(&mut self) -> Result<(), InterpreterError> {
        loop {
            if self.is_done() {
                return Ok(());
            }

            if let Err(err) = self.step() {
                return Err(err);
            }
        }
    }

    pub fn is_done(&self) -> bool {
        self.frames.is_empty() || self.frame().ip >= self.frame().closure.function.chunk.code.len()
    }

    pub fn step(&mut self) -> Result<(), InterpreterError> {
        let op = self.next_op_and_advance();

        if self.heap.should_collect() {
            self.collect_garbage();
        }

        match op {
            (bytecode::Op::Return, _) => {
                let result = self.pop_stack();

                for idx in self.frame().slots_offset..self.stack.len() {
                    self.close_upvalues(idx);
                }

                if self.frames.len() <= 1 {
                    self.frames.pop();
                    return Ok(());
                }

                let num_to_pop = self.stack.len() - self.frame().slots_offset
                    + usize::from(self.frame().closure.function.arity);
                self.frames.pop();

                self.pop_stack_n_times(num_to_pop);

                self.stack.push(result);
            }
            (bytecode::Op::Closure(idx, upvals), _) => {
                let constant = self.read_constant(idx);

                if let value::Value::Function(closure_handle) = constant {
                    let closure = self.get_closure(closure_handle).clone();
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
                                    let index = self.frame().slots_offset + *idx - 1;
                                    let upval = Rc::new(RefCell::new(value::Upvalue::Open(index)));
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
                    (value::Value::Number(_), value::Value::Number(_)) => {
                        self.numeric_binop(Binop::Add, lineno)?
                    }
                    (value::Value::String(s1), value::Value::String(s2)) => {
                        self.pop_stack();
                        self.pop_stack();
                        self.stack
                            .push(value::Value::String(self.heap.manage_str(format!(
                                "{}{}",
                                self.get_str(*s2),
                                self.get_str(*s1)
                            ))));
                    }
                    (value::Value::List(id1), value::Value::List(id2)) => {
                        self.pop_stack();
                        self.pop_stack();
                        let mut res = self.get_list_elements(*id2).clone();
                        res.extend(self.get_list_elements(*id1).clone());
                        self.stack
                            .push(value::Value::List(self.heap.manage_list(res)));
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
                if let value::Value::String(name_id) = self.read_constant(idx) {
                    let val = self.pop_stack();
                    self.globals.insert(self.get_str(name_id).clone(), val);
                } else {
                    panic!(
                        "expected string when defining global, found {:?}",
                        value::type_of(&self.read_constant(idx))
                    );
                }
            }
            (bytecode::Op::GetGlobal(idx), lineno) => {
                if let value::Value::String(name_id) = self.read_constant(idx) {
                    match self.globals.get(self.get_str(name_id)) {
                        Some(val) => {
                            self.stack.push(val.clone());
                        }
                        None => {
                            return Err(InterpreterError::Runtime(format!(
                                "Undefined variable '{}' at line {}.",
                                self.get_str(name_id),
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
                if let value::Value::String(name_id) = self.read_constant(idx) {
                    let name_str = self.get_str(name_id).clone();
                    let val = self.peek().clone();
                    if let std::collections::hash_map::Entry::Occupied(mut e) =
                        self.globals.entry(name_str.clone())
                    {
                        e.insert(val);
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
                let val = self.stack[slots_offset + idx - 1].clone();
                self.stack.push(val);
            }
            (bytecode::Op::SetLocal(idx), _) => {
                let val = self.peek();
                let slots_offset = self.frame().slots_offset;
                self.stack[slots_offset + idx - 1] = val.clone();
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
            (bytecode::Op::Class(idx), _) => {
                if let value::Value::String(name_id) = self.read_constant(idx) {
                    let name = self.get_str(name_id).clone();
                    self.stack
                        .push(value::Value::Class(self.heap.manage_class(value::Class {
                            name,
                            methods: HashMap::new(),
                        })));
                } else {
                    panic!(
                        "expected string when defining class, found {:?}",
                        value::type_of(&self.read_constant(idx))
                    );
                }
            }
            (bytecode::Op::SetProperty(idx), _) => {
                if let value::Value::String(attr_id) = self.read_constant(idx) {
                    let val = self.pop_stack();
                    let instance = self.pop_stack();
                    self.setattr(instance, val.clone(), attr_id)?;
                    self.stack.push(val);
                } else {
                    panic!(
                        "expected string when setting property, found {:?}",
                        value::type_of(&self.read_constant(idx))
                    )
                }
            }
            (bytecode::Op::GetProperty(idx), _) => {
                if let value::Value::String(attr_id) = self.read_constant(idx) {
                    let maybe_instance = self.peek().clone();

                    let (class_id, instance_id) = match maybe_instance {
                        value::Value::Instance(instance_id) => {
                            let instance = self.heap.get_instance(instance_id).clone();
                            (instance.class_id, instance_id)
                        }
                        _ => panic!(),
                    };

                    let class = self.heap.get_class(class_id).clone();
                    if let Some(attr) = self.getattr(maybe_instance.clone(), attr_id)? {
                        self.pop_stack();
                        self.stack.push(attr);
                    } else if !self.bind_method(instance_id, class, attr_id)? {
                        return Err(InterpreterError::Runtime(format!(
                            "value {} has no attribute {}.",
                            self.format_val(&maybe_instance),
                            self.get_str(attr_id)
                        )));
                    }
                } else {
                    panic!(
                        "expected string when setting property, found {:?}",
                        value::type_of(&self.read_constant(idx))
                    )
                }
            }
            (bytecode::Op::Method(idx), _) => {
                if let value::Value::String(method_name_id) = self.read_constant(idx) {
                    let method_name = self.heap.get_str(method_name_id).clone();
                    let maybe_method = self.peek_by(0).clone();
                    let maybe_method_id = gc::Heap::extract_id(&maybe_method).unwrap();
                    let maybe_class = self.peek_by(1).clone();
                    match maybe_class {
                        value::Value::Class(class_id) => {
                            let class = self.heap.get_class_mut(class_id);
                            class.methods.insert(method_name, maybe_method_id);
                            self.pop_stack();
                        }
                        _ => {
                            panic!(
                                "should only define methods on a class! tried on {:?}",
                                self.format_val(&maybe_class)
                            );
                        }
                    }
                } else {
                    panic!("expected string when defining a method.");
                }
            }
            (bytecode::Op::Invoke(method_name, arg_count), _) => {
                self.invoke(&method_name, arg_count)?;
            }
            (bytecode::Op::Inherit, lineno) => {
                {
                    let (superclass_id, subclass_id) = match (self.peek_by(1), self.peek()) {
                        (value::Value::Class(superclass_id), value::Value::Class(subclass_id)) => {
                            (*superclass_id, *subclass_id)
                        }
                        (not_a_class, value::Value::Class(_)) => {
                            return Err(InterpreterError::Runtime(format!(
                                "Superclass must be a class, found {:?} at lineno={:?}",
                                value::type_of(&not_a_class),
                                lineno
                            )));
                        }
                        _ => panic!("expected classes when interpreting Inherit!"),
                    };

                    let superclass_methods = self.get_class(superclass_id).methods.clone();
                    let subclass = self.get_class_mut(subclass_id);

                    subclass.methods.extend(superclass_methods);
                }
                self.pop_stack(); //subclass
            }
            (bytecode::Op::GetSuper(idx), _) => {
                let method_id = if let value::Value::String(method_id) = self.read_constant(idx) {
                    method_id
                } else {
                    panic!();
                };

                let maybe_superclass = self.pop_stack();
                let superclass = match maybe_superclass {
                    value::Value::Class(class_id) => self.get_class(class_id).clone(),
                    _ => panic!(),
                };

                let maybe_instance = self.peek();
                let instance_id = match maybe_instance {
                    value::Value::Instance(instance_id) => *instance_id,
                    _ => panic!(),
                };

                if !self.bind_method(instance_id, superclass, method_id)? {
                    return Err(InterpreterError::Runtime(format!(
                        "superclass {} has no attribute {}.",
                        self.format_val(&maybe_superclass),
                        self.get_str(method_id)
                    )));
                }
            }
            (bytecode::Op::SuperInvoke(method_name, arg_count), _) => {
                let maybe_superclass = self.pop_stack();
                let superclass_id = match maybe_superclass {
                    value::Value::Class(class_id) => class_id,
                    _ => panic!("{}", self.format_val(&maybe_superclass)),
                };
                self.invoke_from_class(superclass_id, &method_name, arg_count)?;
            }
            (bytecode::Op::BuildList(size), _) => {
                let mut list_elements = Vec::new();
                for _ in 0..size {
                    list_elements.push(self.pop_stack())
                }
                list_elements.reverse();
                self.stack
                    .push(value::Value::List(self.heap.manage_list(list_elements)));
            }
            (bytecode::Op::Subscr, lineno) => {
                let subscript = self.pop_stack();
                let value_to_subscript = self.pop_stack();
                let res = self.subscript(value_to_subscript, subscript, lineno)?;
                self.stack.push(res);
            }
            (bytecode::Op::SetItem, lineno) => {
                let rhs = self.pop_stack();
                let subscript = self.pop_stack();
                let lhs = self.pop_stack();
                self.setitem(lhs, subscript, rhs.clone(), lineno)?;
                self.stack.push(rhs);
            }
        }
        Ok(())
    }

    fn setitem(
        &mut self,
        lhs: value::Value,
        subscript: value::Value,
        rhs: value::Value,
        lineno: bytecode::Lineno,
    ) -> Result<(), InterpreterError> {
        if let value::Value::List(id) = lhs {
            if let value::Value::Number(index_float) = subscript {
                let elements = self.get_list_elements_mut(id);
                match Interpreter::subscript_to_inbound_index(elements.len(), index_float, lineno) {
                    Ok(index_int) => {
                        elements[index_int] = rhs;
                        Ok(())
                    }
                    Err(err) => Err(InterpreterError::Runtime(err)),
                }
            } else {
                Err(InterpreterError::Runtime(format!(
                    "Invalid subscript of type {:?} in subscript expression",
                    value::type_of(&lhs)
                )))
            }
        } else {
            Err(InterpreterError::Runtime(format!(
                "Invalid value of type {:?} in subscript expression",
                value::type_of(&subscript)
            )))
        }
    }

    fn subscript(
        &mut self,
        value: value::Value,
        subscript: value::Value,
        lineno: bytecode::Lineno,
    ) -> Result<value::Value, InterpreterError> {
        if let value::Value::List(id) = value {
            if let value::Value::Number(index_float) = subscript {
                let elements = self.get_list_elements(id);
                match Interpreter::subscript_to_inbound_index(elements.len(), index_float, lineno) {
                    Ok(index_int) => Ok(elements[index_int].clone()),
                    Err(err) => Err(InterpreterError::Runtime(err)),
                }
            } else {
                Err(InterpreterError::Runtime(format!(
                    "Invalid subscript of type {:?} in subscript expression",
                    value::type_of(&value)
                )))
            }
        } else {
            Err(InterpreterError::Runtime(format!(
                "Invalid value of type {:?} in subscript expression",
                value::type_of(&value)
            )))
        }
    }

    fn subscript_to_inbound_index(
        list_len: usize,
        index_float: f64,
        lineno: bytecode::Lineno,
    ) -> Result<usize, String> {
        let index_int = index_float as i64;
        if 0 <= index_int && index_int < list_len as i64 {
            return Ok(index_int as usize);
        }
        if index_int < 0 && -index_int <= list_len as i64 {
            return Ok((list_len as i64 + index_int) as usize);
        }
        Err(format!(
            "List subscript index out of range at {}",
            lineno.value
        ))
    }

    fn invoke(&mut self, method_name: &str, arg_count: u8) -> Result<(), InterpreterError> {
        let receiver_id = match self.peek_by(arg_count.into()) {
            value::Value::Instance(id) => *id,
            _ => {
                return Err(InterpreterError::Runtime(
                    "Only instances have methods.".to_string(),
                ));
            }
        };

        if let Some(field) = self
            .get_instance(receiver_id)
            .fields
            .get(&String::from(method_name))
            .cloned()
        {
            return self.call_value(field, arg_count);
        }

        let class_id = self.get_instance(receiver_id).class_id;
        self.invoke_from_class(class_id, &method_name, arg_count)
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        let frames_len = self.frames.len();
        &mut self.frames[frames_len - 1]
    }

    pub fn maybe_frame(&self) -> Option<&CallFrame> {
        self.frames.last()
    }

    pub fn frame(&self) -> &CallFrame {
        self.maybe_frame().unwrap()
    }

    fn invoke_from_class(
        &mut self,
        class_id: gc::HeapId,
        method_name: &str,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        let method_id = match self
            .get_class(class_id)
            .methods
            .get(&String::from(method_name))
        {
            Some(method_id) => *method_id,
            None => {
                return Err(InterpreterError::Runtime(format!(
                    "Undefined property {}.",
                    method_name
                )))
            }
        };

        self.call_value(value::Value::Function(method_id), arg_count)
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

    pub fn call_value(
        &mut self,
        val_to_call: value::Value,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        match val_to_call {
            value::Value::Function(func) => {
                self.prepare_call(func, arg_count)?;
                Ok(())
            }
            value::Value::NativeFunction(native_func) => {
                self.call_native_func(native_func, arg_count)?;
                Ok(())
            }
            value::Value::Class(class_id) => {
                let new_instance =
                    value::Value::Instance(self.heap.manage_instance(value::Instance {
                        class_id,
                        fields: HashMap::new(),
                    }));

                let arg_count_usize: usize = arg_count.into();
                let stack_len = self.stack.len();
                self.stack[stack_len - 1 - arg_count_usize] = new_instance;

                {
                    let maybe_method_id = self
                        .get_class(class_id)
                        .methods
                        .get(&"init".to_string())
                        .copied();

                    if let Some(method_id) = maybe_method_id {
                        return self.prepare_call(method_id, arg_count);
                    }
                }

                if arg_count > 0 {
                    return Err(InterpreterError::Runtime(format!(
                        "Call to class ctor expected 0 arguments, got {}.",
                        arg_count,
                    )));
                }

                self.create_instance(class_id);
                Ok(())
            }
            value::Value::BoundMethod(method_id) => {
                self.call_bound_method(method_id, arg_count)?;
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "attempted to call non-callable value of type {:?}.",
                value::type_of(&val_to_call)
            ))),
        }
    }

    fn call_native_func(
        &mut self,
        native_func: value::NativeFunction,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        if arg_count != native_func.arity {
            return Err(InterpreterError::Runtime(format!(
                "Native function {} expected {} arguments but found {}.",
                native_func.name, native_func.arity, arg_count
            )));
        }

        let mut args = Vec::new();
        for _ in 0..arg_count {
            args.push(self.pop_stack()) // pop args
        }
        args.reverse();
        let args = args;
        self.pop_stack(); // native function value

        let res = (native_func.func)(self, &args);

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

    fn create_instance(&mut self, class_id: gc::HeapId) {
        self.pop_stack(); // class object
        let instance_id = self.heap.manage_instance(value::Instance {
            class_id,
            fields: HashMap::new(),
        });
        self.stack.push(value::Value::Instance(instance_id));
    }

    fn call_bound_method(
        &mut self,
        method_id: gc::HeapId,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        let bound_method = self.get_bound_method(method_id).clone();
        let closure_id = bound_method.closure_id;
        let arg_count_usize: usize = arg_count.into();
        let stack_len = self.stack.len();
        self.stack[stack_len - arg_count_usize - 1] =
            value::Value::Instance(bound_method.instance_id);
        self.prepare_call(closure_id, arg_count)
    }

    /*
    Set up a few call frame so that on the next interpreter step we'll start executing code inside the function.
     */
    fn prepare_call(
        &mut self,
        closure_handle: gc::HeapId,
        arg_count: u8,
    ) -> Result<(), InterpreterError> {
        let closure = self.get_closure(closure_handle).clone();
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
            value::Value::Class(_) => false,
            value::Value::Instance(_) => false,
            value::Value::BoundMethod(_) => false,
            value::Value::String(id) => self.get_str(*id).is_empty(),
            value::Value::List(id) => self.get_list_elements(*id).is_empty(),
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
                self.get_str(*s1) == self.get_str(*s2)
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

    fn setattr(
        &mut self,
        maybe_instance: value::Value,
        val: value::Value,
        attr_id: gc::HeapId,
    ) -> Result<(), InterpreterError> {
        let attr_name = self.get_str(attr_id).clone();
        match maybe_instance {
            value::Value::Instance(instance_id) => {
                let instance = self.heap.get_instance_mut(instance_id);
                instance.fields.insert(attr_name, val);
                Ok(())
            }
            _ => Err(InterpreterError::Runtime(format!(
                "can't set attribute on value of type {:?}. Need class instance. val = {:?}",
                value::type_of(&maybe_instance),
                self.format_val(&maybe_instance)
            ))),
        }
    }

    fn getattr(
        &self,
        maybe_instance: value::Value,
        attr_id: gc::HeapId,
    ) -> Result<Option<value::Value>, InterpreterError> {
        let attr_name = self.get_str(attr_id).clone();
        match maybe_instance {
            value::Value::Instance(instance_id) => {
                let instance = self.heap.get_instance(instance_id);
                match instance.fields.get(&attr_name) {
                    Some(val) => Ok(Some(val.clone())),
                    None => Ok(None),
                }
            }
            _ => Err(InterpreterError::Runtime(format!(
                "can't get attribute {}  on value of type {:?}. Need class instance.",
                attr_name,
                value::type_of(&maybe_instance)
            ))),
        }
    }

    fn bind_method(
        &mut self,
        instance_id: gc::HeapId,
        class: value::Class,
        attr_id: gc::HeapId,
    ) -> Result<bool, InterpreterError> {
        let attr_name = self.get_str(attr_id).clone();
        if let Some(closure_id) = class.methods.get(&attr_name) {
            self.pop_stack();
            self.stack
                .push(value::Value::BoundMethod(self.heap.manage_bound_method(
                    value::BoundMethod {
                        instance_id,
                        closure_id: *closure_id,
                    },
                )));
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn pop_stack(&mut self) -> value::Value {
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

    pub fn next_line(&self) -> usize {
        self.next_op().1.value
    }

    pub fn next_op(&self) -> (bytecode::Op, bytecode::Lineno) {
        self.frame().next_op()
    }

    fn next_op_and_advance(&mut self) -> (bytecode::Op, bytecode::Lineno) {
        self.frame_mut().next_op_and_advance()
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

    fn get_str(&self, str_handle: gc::HeapId) -> &String {
        self.heap.get_str(str_handle)
    }

    fn get_closure(&self, closure_handle: gc::HeapId) -> &value::Closure {
        self.heap.get_closure(closure_handle)
    }

    fn get_class(&self, class_handle: gc::HeapId) -> &value::Class {
        self.heap.get_class(class_handle)
    }

    fn get_class_mut(&mut self, class_handle: gc::HeapId) -> &mut value::Class {
        self.heap.get_class_mut(class_handle)
    }

    fn get_bound_method(&self, method_handle: gc::HeapId) -> &value::BoundMethod {
        self.heap.get_bound_method(method_handle)
    }

    fn get_list_elements(&self, list_handle: gc::HeapId) -> &Vec<value::Value> {
        self.heap.get_list_elements(list_handle)
    }

    fn get_list_elements_mut(&mut self, list_handle: gc::HeapId) -> &mut Vec<value::Value> {
        self.heap.get_list_elements_mut(list_handle)
    }

    fn get_instance(&self, instance_handle: gc::HeapId) -> &value::Instance {
        self.heap.get_instance(instance_handle)
    }

    fn collect_garbage(&mut self) {
        self.heap.unmark();
        self.mark_roots();
        self.trace_references();

        self.heap.sweep();
    }

    fn trace_references(&mut self) {
        loop {
            let maybe_val = self.gray_stack.pop();
            match maybe_val {
                Some(val) => self.blacken_object(val),
                None => break,
            }
        }
    }

    fn blacken_object(&mut self, val: gc::HeapId) {
        let children_to_walk = self.heap.children(val);
        for child_val in children_to_walk {
            if !self.heap.is_marked(child_val) {
                self.heap.mark(child_val);
                self.blacken_object(child_val);
            }
        }
    }

    fn mark_roots(&mut self) {
        let stack_vals_to_mark: Vec<gc::HeapId> = self
            .stack
            .iter()
            .map(gc::Heap::extract_id)
            .flatten()
            .collect();

        let frame_closure_children: Vec<gc::HeapId> = self
            .frames
            .iter()
            .map(|frame| self.heap.closure_children(&frame.closure))
            .flatten()
            .collect();

        let globals_to_mark: Vec<gc::HeapId> = self
            .globals
            .values()
            .map(gc::Heap::extract_id)
            .flatten()
            .collect();

        for val in stack_vals_to_mark
            .iter()
            .chain(frame_closure_children.iter())
            .chain(globals_to_mark.iter())
        {
            self.mark_value(*val);
        }
    }

    fn mark_value(&mut self, handle: gc::HeapId) {
        let is_marked = self.heap.is_marked(handle);
        if !is_marked {
            self.heap.mark(handle);
        }
        self.gray_stack.push(handle)
    }
}

#[cfg(test)]
mod tests {

    macro_rules! vec_of_strings {
    ($($x:expr),*) => (vec![$($x.to_string()),*]);
}

    use crate::bytecode_interpreter::*;
    use crate::compiler::*;
    use crate::syntax_extensions;

    fn evaluate(
        code: &str,
        extensions: syntax_extensions::Extensions,
    ) -> Result<Vec<String>, String> {
        let func_or_err = Compiler::compile(String::from(code), extensions);

        match func_or_err {
            Ok(func) => {
                let mut interp = Interpreter::default();
                let res = interp.interpret(func);
                match res {
                    Ok(()) => Ok(interp.output),
                    Err(InterpreterError::Runtime(err)) => Err(err),
                }
            }
            Err(err) => Err(err),
        }
    }

    fn check_output(
        code: &str,
        extensions: syntax_extensions::Extensions,
        expected_output: &[String],
    ) {
        let res = evaluate(code, extensions);

        match res {
            Ok(output) => assert_eq!(output, expected_output),
            Err(err) => panic!("{}", err),
        }
    }

    fn check_output_default(code: &str, expected_output: &[String]) {
        check_output(
            code,
            syntax_extensions::Extensions::default(),
            expected_output,
        );
    }

    fn check_output_lists(code: &str, expected_output: &[String]) {
        check_output(
            code,
            syntax_extensions::Extensions { lists: true },
            expected_output,
        );
    }

    fn check_error(code: &str, extensions: syntax_extensions::Extensions, f: &dyn Fn(&str) -> ()) {
        let res = evaluate(code, extensions);

        match res {
            Ok(output) => panic!("{:?}", output),
            Err(err) => f(&err),
        }
    }

    fn check_error_default(code: &str, f: &dyn Fn(&str) -> ()) {
        check_error(code, syntax_extensions::Extensions::default(), f);
    }

    #[test]
    fn test_var_reading_1() {
        check_output_default("var x = 2; print x;", &vec_of_strings!["2"]);
    }

    #[test]
    fn test_var_reading_locals_1() {
        check_output_default("{var x = 2; print x;}", &vec_of_strings!["2"]);
    }

    #[test]
    fn test_var_reading_4() {
        check_output_default(
            "var x = 2;\n\
             var y = 3;\n\
             print x * y + 4;",
            &vec_of_strings!["10"],
        );
    }

    #[test]
    fn test_var_reading_locals_2() {
        check_output_default(
            "{\n\
               var x = 2;\n\
               var y = 3;\n\
               print x * y + 4;\n\
             }\n",
            &vec_of_strings!["10"],
        );
    }

    #[test]
    fn test_div_by_zero() {
        check_output_default("print 1 / 0;", &vec_of_strings!["inf"]);
    }

    #[test]
    fn test_setitem_globals() {
        check_output_default(
            "var breakfast = \"beignets\";\n\
             var beverage = \"cafe au lait\";\n\
             breakfast = \"beignets with \" + beverage;\n\
             print breakfast;",
            &vec_of_strings!["beignets with cafe au lait"],
        );
    }

    #[test]
    fn test_setitem_locals() {
        check_output_default(
            "{\n\
               var breakfast = \"beignets\";\n\
               var beverage = \"cafe au lait\";\n\
               breakfast = \"beignets with \" + beverage;\n\
               print breakfast;\n\
             }\n",
            &vec_of_strings!["beignets with cafe au lait"],
        );
    }

    #[test]
    fn test_read_in_own_initializer() {
        check_error_default(
            "{\n\
               var a = \"outer\";\n\
               {\n\
                 var a = a;\n\
               }\n\
             }\n",
            &|err: &str| {
                assert!(err.starts_with("Cannot read local variable in its own initializer."))
            },
        )
    }

    #[test]
    fn test_if_stmt() {
        check_output_default(
            "var x = 0;\n\
             var y = 1;\n\
             if (x) {\n\
               print x;\n\
             }\n\
             if (y) {\n\
               print y;\n\
             }",
            &vec_of_strings!["1"],
        );
    }

    #[test]
    fn test_if_then_else_1() {
        check_output_default(
            "var x = 0;\n\
             if (x) {\n\
               print \"hello\";\n\
             } else {\n\
               print \"goodbye\";\n\
             }",
            &vec_of_strings!["goodbye"],
        );
    }

    #[test]
    fn test_if_then_else_2() {
        check_output_default(
            "var x = 1;\n\
             if (x) {\n\
               print \"hello\";\n\
             } else {\n\
               print \"goodbye\";\n\
             }",
            &vec_of_strings!["hello"],
        );
    }

    #[test]
    fn test_print_locals() {
        check_output_default(
            "{\n\
               var x = 0;\n\
               var y = 1;\n\
               print x;\n\
               print y;\n\
             }",
            &vec_of_strings!["0", "1"],
        );
    }

    #[test]
    fn test_print_globals() {
        check_output_default(
            "var x = 0;\n\
             var y = 1;\n\
             print x;\n\
             print y;\n",
            &vec_of_strings!["0", "1"],
        );
    }

    #[test]
    fn test_and_1() {
        check_output_default(
            "var x = false;\n\
             var y = true;\n\
             if (y and x) {\n\
               print \"cat\";\n\
             } else {\n\
               print \"dog\";\n\
             }\n",
            &vec_of_strings!["dog"],
        );
    }

    #[test]
    fn test_and_2() {
        check_output_default(
            "var x = false;\n\
             var y = true;\n\
             if (x and y) {\n\
               print \"cat\";\n\
             } else {\n\
               print \"dog\";\n\
             }\n",
            &vec_of_strings!["dog"],
        );
    }

    #[test]
    fn test_and_3() {
        check_output_default(
            "var x = true;\n\
             var y = true;\n\
             if (y and x) {\n\
               print \"cat\";\n\
             } else {\n\
               print \"dog\";\n\
             }\n",
            &vec_of_strings!["cat"],
        );
    }

    #[test]
    fn test_or_1() {
        check_output_default(
            "var x = false;\n\
             var y = true;\n\
             if (y or x) {\n\
               print \"cat\";\n\
             } else {\n\
               print \"dog\";\n\
             }\n",
            &vec_of_strings!["cat"],
        );
    }

    #[test]
    fn test_or_2() {
        check_output_default(
            "var x = false;\n\
             var y = true;\n\
             if (x or y) {\n\
               print \"cat\";\n\
             } else {\n\
               print \"dog\";\n\
             }\n",
            &vec_of_strings!["cat"],
        );
    }

    #[test]
    fn test_or_3() {
        check_output_default(
            "var x = false;\n\
             var y = false;\n\
             if (y or x) {\n\
               print \"cat\";\n\
             } else {\n\
               print \"dog\";\n\
             }\n",
            &vec_of_strings!["dog"],
        );
    }

    #[test]
    fn test_while() {
        check_output_default(
            "{var x = 0;\n\
             var sum = 0;\n\
             while (x < 100) {\n\
               x = x + 1;\n\
               sum = sum + x;\n\
             }\n\
             print sum;}",
            &vec_of_strings!["5050"],
        );
    }

    #[test]
    fn test_for() {
        fn fact(n: i32) -> i32 {
            if n <= 1 {
                return 1;
            }
            return n * fact(n - 1);
        }

        check_output_default(
            "{\n\
               var fact = 1;\n\
               for (var i = 1; i <= 10; i = i + 1) {\n\
                 fact = fact * i;\n\
               }\n\
               print fact;\n\
             }",
            &vec_of_strings![format!("{}", fact(10))],
        );
    }

    #[test]
    fn test_functions_1() {
        check_output_default(
            "fun areWeHavingItYet() {\n\
               print \"Yes we are!\";\n\
             }\n\
             \n\
             print areWeHavingItYet;\n",
            &vec_of_strings!["<fn 'areWeHavingItYet'>"],
        )
    }

    #[test]
    fn test_functions_2() {
        check_output_default(
            "fun f(x, y) {\n\
               print x + y;\n\
             }\n\
             \n\
             print f;\n",
            &vec_of_strings!["<fn 'f'>"],
        )
    }

    #[test]
    fn test_functions_3() {
        check_output_default(
            "fun f(x, y) {\n\
               return x + y;\n\
             }\n\
             \n\
             print f;\n",
            &vec_of_strings!["<fn 'f'>"],
        )
    }

    #[test]
    fn test_functions_4() {
        check_output_default(
            "fun f() {\n\
               return;\n\
             }\n\
             \n\
             print f();\n",
            &vec_of_strings!["nil"],
        )
    }

    #[test]
    fn test_functions_5() {
        check_error_default("return 42;", &|err: &str| {
            assert_eq!(err, "Cannot return from top-level code.")
        })
    }

    #[test]
    fn test_functions_6() {
        check_output_default(
            "fun f(x, y) {\n\
               return x + y;\n\
             }\n\
             \n\
             print f(1,2);\n",
            &vec_of_strings!["3"],
        );
    }

    #[test]
    fn test_functions_7() {
        check_output_default(
            "fun g(x) {\n\
               return 2 * x;\n\
             }\n\
             \n\
             fun f(x, y) {\n\
               return g(x) + y;\n\
             }\n\
             \n\
             print f(1,2);\n",
            &vec_of_strings!["4"],
        );
    }

    #[test]
    fn test_functions_8() {
        check_output_default(
            "var x = 2;\n\
             fun f(x) {\n\
               print 2 * x;\n\
             }\n\
             \n\
             f(x);\n\
             print x;\n",
            &vec_of_strings!["4", "2"],
        );
    }

    #[test]
    fn test_functions_9() {
        fn fact(n: i32) -> i32 {
            if n <= 1 {
                return 1;
            }
            return n * fact(n - 1);
        }

        check_output_default(
            "fun fact(n) {\n\
               if (n <= 1) { return 1; }\n\
               return n * fact(n - 1);\n\
             }\n\
             \n\
             print fact(10);\n",
            &vec_of_strings![format!("{}", fact(10))],
        );
    }

    #[test]
    fn test_functions_10() {
        check_output_default(
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
            &vec_of_strings!["true"],
        );
    }

    #[test]
    fn test_native_functions() {
        let res = evaluate(
            "fun fib(n) {\n\
               if (n < 2) return n;\n\
               return fib(n - 2) + fib(n - 1);\n\
             }\n\
             \n\
             var start = clock();\n\
             print fib(5);\n\
             print clock() - start;\n\
             print 42;",
            syntax_extensions::Extensions::default(),
        );

        match res {
            Ok(output) => {
                assert_eq!(output.len(), 3);
                assert_eq!(output[0], "5");
                assert_eq!(output[2], "42");
            }
            Err(err) => {
                panic!("{:?}", err);
            }
        }
    }

    #[test]
    fn test_get_upval_on_stack() {
        check_output_default(
            "fun outer() {\n\
               var x = \"outside\";\n\
               fun inner() {\n\
                 print x;\n\
               }\n\
               inner();\n\
             }\n\
             outer();",
            &vec_of_strings!["outside"],
        );
    }

    #[test]
    fn test_set_upval_on_stack() {
        check_output_default(
            "fun outer() {\n\
               var x = \"before\";\n\
               fun inner() {\n\
                 x = \"assigned\";\n\
               }\n\
               inner();\n\
               print x;\n\
             }\n\
             outer();",
            &vec_of_strings!["assigned"],
        );
    }

    #[test]
    fn test_closing_upvals_after_return() {
        check_output_default(
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
            &vec_of_strings!["outside"],
        );
    }

    #[test]
    fn test_closing_upvals_after_scope() {
        check_output_default(
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
            &vec_of_strings!["outside"],
        );
    }

    #[test]
    fn test_classes_1() {
        check_output_default(
            "class Brioche {}\n\
             print Brioche;\n",
            &vec_of_strings!["<class 'Brioche'>"],
        );
    }

    #[test]
    fn test_classes_instances_1() {
        check_output_default(
            "class Brioche {}\n\
             var instance = Brioche();\n\
             print instance;\n",
            &vec_of_strings!["<Brioche instance>"],
        );
    }

    #[test]
    fn test_setattr_1() {
        check_output_default(
            "class Foo {}\n\
             var foo = Foo();\n\
             foo.attr = 42;\n\
             print foo.attr;\n",
            &vec_of_strings!["42"],
        );
    }

    #[test]
    fn test_setattr_2() {
        check_output_default(
            "class Toast {}\n\
             var toast = Toast();\n\
             print toast.jam = \"grape\";",
            &vec_of_strings!["grape"],
        );
    }

    #[test]
    fn test_setattr_3() {
        check_output_default(
            "class Pair {}\n\
             var pair = Pair();\n\
             pair.first = 1;\n\
             pair.second = 2;\n\
             print pair.first + pair.second;",
            &vec_of_strings!["3"],
        );
    }

    #[test]
    fn test_bound_methods_1() {
        check_output_default(
            "class Foo {\n\
               bar() {\n\
                 return 42;
               }\n\
             }\n\
             var foo = Foo();\n\
             print foo.bar;",
            &vec_of_strings!["<bound method of Foo instance>"],
        );
    }

    #[test]
    fn test_calling_bound_methods_no_this() {
        check_output_default(
            "class Scone {\n\
               topping(first, second) {\n\
                 print \"scone with \" + first + \" and \" + second;\n\
               }\n\
             }\n\
             \n\
             var scone = Scone();\n\
             scone.topping(\"berries\", \"cream\");",
            &vec_of_strings!["scone with berries and cream"],
        );
    }

    #[test]
    fn test_calling_bound_methods_with_this_1() {
        check_output_default(
            "class Nested {\n\
               method() {\n\
                 print this;\n\
               }\n\
             }\n\
             \n\
             Nested().method();",
            &vec_of_strings!["<Nested instance>"],
        );
    }

    #[test]
    fn test_calling_bound_methods_with_this_2() {
        check_output_default(
            "class Nested {\n\
               method() {\n\
                 fun function() {\n\
                   print this;\n\
                 }\n\
                 \n\
                 function();\n\
               }\n\
             }\n\
             \n\
             Nested().method();",
            &vec_of_strings!["<Nested instance>"],
        );
    }

    #[test]
    fn test_multiple_method_definitions() {
        check_output_default(
            "class Brunch {\n\
               bacon() {}\n\
               eggs() {}\n\
             }\n\
             print Brunch().bacon();",
            &vec_of_strings!["nil"],
        );
    }

    #[test]
    fn test_init_1() {
        check_output_default(
            "class Brunch {\n\
               init(x) {this.x = x;}\n\
               eggs(y) {return this.x + y;}\n\
             }\n\
             print Brunch(2).eggs(3);",
            &vec_of_strings!["5"],
        );
    }

    #[test]
    fn test_invoking_fields() {
        check_output_default(
            "class Oops {\n\
               init() {\n\
                 fun f() {\n\
                   print \"not a method\";\n\
                 }\n\
                 \n\
                 this.field = f;\n\
               }\n\
             }\n\
             \n\
             var oops = Oops();\n\
             oops.field();\n",
            &vec_of_strings!["not a method"],
        );
    }

    #[test]
    fn test_inheritance_1() {
        check_output_default(
            "class A {\n\
               f() {\n\
                 return \"cat\";\n\
               }\n\
             }\n\
             class B < A {}\n\
             var b = B();\n\
             print b.f();",
            &vec_of_strings!["cat"],
        );
    }

    #[test]
    fn test_inheritance_2() {
        check_output_default(
            "class A {\n\
               f() {\n\
                 return \"cat\";\n\
               }\n\
             }\n\
             class B < A {}\n\
             class C < B {}\n\
             var c = C();\n\
             print c.f();",
            &vec_of_strings!["cat"],
        );
    }

    #[test]
    fn test_inheritance_3() {
        check_output_default(
            "class A {\n\
               f() {\n\
                 return this.attr;
               }\n\
             }\n\
             class B < A {\n\
               init(attr) {\n\
                 this.attr = attr;\n\
               }\n\
             }\n\
             var b = B(42);\n\
             print b.f();",
            &vec_of_strings!["42"],
        );
    }

    #[test]
    fn test_inheritance_4() {
        check_output_default(
            "class A {\n\
               f() {\n\
                 return this.attr;
               }\n\
             }\n\
             class B < A {\n\
             }\n\
             var b = B();\n\
             b.attr = 42;
             print b.f();",
            &vec_of_strings!["42"],
        );
    }

    #[test]
    fn test_inheriting_non_class() {
        check_error_default(
            "var NotClass = \"So not a class\";\n\
             class OhNo < NotClass {}\n",
            &|err: &str| {
                assert!(err.starts_with("Superclass must be a class, found String at lineno="))
            },
        )
    }

    #[test]
    fn test_super_1() {
        check_output_default(
            "class A {\n\
               method() {\n\
                 print \"A method\";\n\
               }\n\
             }\n\
             \n\
             class B < A {\n\
               method() {\n\
                 print \"B method\";\n\
               }\n\
               \n\
               test() {\n\
                 super.method();\n\
               }\n\
             }\n\
             \n\
             class C < B {}\n\
             \n\
             C().test();\n",
            &vec_of_strings!["A method"],
        )
    }

    #[test]
    fn test_super_2() {
        check_output_default(
            "class A {\n\
               method() {\n\
                 print \"A method\";\n\
               }\n\
             }\n\
             \n\
             class B < A {\n\
               method() {\n\
                 print \"B method\";\n\
               }\n\
               \n\
               test() {\n\
                 var func = super.method;\n\
                 func();\n\
               }\n\
             }\n\
             \n\
             class C < B {}\n\
             \n\
             C().test();\n",
            &vec_of_strings!["A method"],
        )
    }

    #[test]
    fn test_super_3() {
        check_output_default(
            "class Doughnut {\n\
               cook() {\n\
                 print \"Dunk in the fryer.\";\n\
                 this.finish(\"sprinkles\");\n\
               }\n\
               \n\
               finish(ingredient) {\n\
                 print \"Finish with \" + ingredient;\n\
               }\n\
             }\n\
             \n\
             class Cruller < Doughnut {\n\
               finish(ingredient) {\n\
                 // No sprinkles.\n\
                 super.finish(\"icing\");\n\
               }\n\
             }\n\
             \n\
             Doughnut().cook();\n\
             Cruller().cook();\n",
            &vec_of_strings![
                "Dunk in the fryer.",
                "Finish with sprinkles",
                "Dunk in the fryer.",
                "Finish with icing"
            ],
        )
    }

    #[test]
    fn test_late_binding() {
        check_output_default(
            "fun a() { b(); }\n\
             fun b() { print \"hello world\"; }\n\
             \n\
             a();\n",
            &vec_of_strings!["hello world"],
        )
    }

    #[test]
    fn test_list_building() {
        check_output_lists("print([1,2,3]);", &vec_of_strings!["[1, 2, 3]"])
    }

    #[test]
    fn test_empty_list_building() {
        check_output_lists("print([]);", &vec_of_strings!["[]"])
    }

    #[test]
    fn test_list_concat() {
        check_output_lists(
            "print([1,2,3] + [4,5,6]);",
            &vec_of_strings!["[1, 2, 3, 4, 5, 6]"],
        )
    }

    #[test]
    fn test_len() {
        check_output_lists(
            "print(len(\"\")); \n\
             print(len(\"cat\")); \n\
             print(len([])); \n\
             print(len([1,2,3,4]));",
            &vec_of_strings!["0", "3", "0", "4"],
        )
    }

    #[test]
    fn test_for_each() {
        check_output_lists(
            "fun f(arg) { print arg; } \n\
             forEach([1,2,3,4], f);",
            &vec_of_strings!["1", "2", "3", "4"],
        )
    }

    #[test]
    fn test_map() {
        check_output_lists(
            "fun f(arg) { return arg + 1; } \n\
             print(map(f, [1,2,3,4]));",
            &vec_of_strings!["[2, 3, 4, 5]"],
        )
    }

    #[test]
    fn test_list_subscript() {
        check_output_lists(
            "var xs = [0,1]; \n\
             print(xs[0]); \n\
             print(xs[1]); \n\
             print(xs[-1]); \n\
             print(xs[-2]); \n\
             ",
            &vec_of_strings!["0", "1", "1", "0"],
        )
    }

    #[test]
    fn test_list_setitem_1() {
        check_output_lists(
            "var xs = [0,1]; \n\
             xs[-1] = 42; \n\
             print(xs);",
            &vec_of_strings!["[0, 42]"],
        )
    }

    #[test]
    fn test_list_setitem_2() {
        check_output_lists(
            "var xs = [[0,1]]; \n\
             xs[0][1] = 42; \n\
             print(xs);",
            &vec_of_strings!["[[0, 42]]"],
        )
    }

    #[test]
    fn test_list_setitem_3() {
        check_output_lists(
            "class Foo {}\n\
             var foo = Foo();\n\
             foo.attr = [0];\n\
             foo.attr[0] = 1337;\n\
             print foo.attr;",
            &vec_of_strings!["[1337]"],
        )
    }
}
