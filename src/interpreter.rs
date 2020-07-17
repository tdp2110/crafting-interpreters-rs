use std::collections::HashMap;
use std::convert::TryInto;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::expr;
use std::fmt;

trait Callable {
    fn arity(&self) -> u8;
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>;
}

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: u8,
    pub callable: fn(&[Value]) -> Result<Value, String>,
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NativeFunction({})", self.name)
    }
}

impl Callable for NativeFunction {
    fn arity(&self) -> u8 {
        self.arity
    }
    fn call(&self, _interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String> {
        (self.callable)(args)
    }
}

#[derive(Clone, Debug)]
pub struct LoxFunction {
    pub name: expr::Symbol,
    pub parameters: Vec<expr::Symbol>,
    pub body: Vec<expr::Stmt>,
    pub closure: Environment,
}

impl Callable for LoxFunction {
    fn arity(&self) -> u8 {
        self.parameters.len().try_into().unwrap()
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String> {
        let args_env: HashMap<_, _> = self
            .parameters
            .iter()
            .zip(args.iter())
            .map(|(param, arg)| {
                (
                    param.name.clone(),
                    (
                        Some(arg.clone()),
                        SourceLocation {
                            line: param.line,
                            col: param.col,
                        },
                    ),
                )
            })
            .collect();

        let saved_env = interpreter.env.clone();
        let saved_retval = interpreter.retval.clone();

        let mut env = self.closure.clone();
        env.venv.extend(args_env);
        let env = env;

        interpreter.env = env;
        interpreter.interpret(&self.body)?;

        let retval = interpreter.retval.clone();
        interpreter.env = saved_env;
        interpreter.retval = saved_retval;

        Ok(match retval {
            Some(val) => val,
            None => Value::Nil,
        })
    }
}

#[derive(Clone, Debug)]
pub struct LoxClass {
    pub name: expr::Symbol,
    pub id: u64,
    pub methods: HashMap<String, u64>,
}

impl Callable for LoxClass {
    fn arity(&self) -> u8 {
        0
    }
    fn call(&self, interpreter: &mut Interpreter, _args: &[Value]) -> Result<Value, String> {
        Ok(interpreter.create_instance(&self.name, self.id))
    }
}

#[derive(Clone, Debug)]
pub struct LoxInstance {
    pub class_name: expr::Symbol,
    pub class_id: u64,
    pub fields: HashMap<String, Value>,
}

impl LoxInstance {
    fn getattr(&self, attr: &String, interpreter: &Interpreter) -> Result<Value, String> {
        match self.fields.get(attr) {
            Some(val) => Ok(val.clone()),
            None => {
                if let Some(cls) = interpreter.lox_classes.get(&self.class_id) {
                    if let Some(method_id) = cls.methods.get(attr) {
                        if let Some(lox_fn) = interpreter.lox_functions.get(method_id) {
                            Ok(Value::LoxFunction(lox_fn.name.clone(), *method_id))
                        } else {
                            panic!(
                                "Internal interpreter error! Could not find lox fn with id {}.",
                                method_id
                            );
                        }
                    } else {
                        Err(format!(
                            "AttributeError: '{}' instance has no '{}' attribute.",
                            self.class_name.name, attr
                        ))
                    }
                } else {
                    panic!(
                        "Internal interpreter error! Could not find class with id {}",
                        self.class_id
                    );
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
    NativeFunction(NativeFunction),
    LoxFunction(expr::Symbol, u64),
    LoxClass(expr::Symbol, u64),
    LoxInstance(expr::Symbol, u64),
}

fn as_callable(interpreter: &Interpreter, value: &Value) -> Option<Box<dyn Callable>> {
    match value {
        Value::NativeFunction(f) => Some(Box::new(f.clone())),
        Value::LoxFunction(_, id) => match interpreter.lox_functions.get(id) {
            Some(f) => Some(Box::new(f.clone())),
            None => panic!(
                "Internal interpreter error! Could not find loxFunction with id {}.",
                id
            ),
        },
        Value::LoxClass(_, id) => match interpreter.lox_classes.get(id) {
            Some(cls) => Some(Box::new(cls.clone())),
            None => panic!(
                "Internal interpreter error! Could not find loxClass with id {}.",
                id
            ),
        },
        _ => None,
    }
}

#[derive(Debug)]
pub enum Type {
    Number,
    String,
    Bool,
    NilType,
    NativeFunction,
    LoxFunction,
    LoxClass,
    LoxInstance,
}

pub fn type_of(val: &Value) -> Type {
    match val {
        Value::Number(_) => Type::Number,
        Value::String(_) => Type::String,
        Value::Bool(_) => Type::Bool,
        Value::Nil => Type::NilType,
        Value::NativeFunction(_) => Type::NativeFunction,
        Value::LoxFunction(_, _) => Type::LoxFunction,
        Value::LoxClass(_, _) => Type::LoxClass,
        Value::LoxInstance(_, _) => Type::LoxInstance,
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "'{}'", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::NativeFunction(func) => write!(f, "NativeFunction({})", func.name),
            Value::LoxFunction(sym, _) => write!(f, "LoxFunction({})", sym.name),
            Value::LoxClass(sym, _) => write!(f, "LoxClass({})", sym.name),
            Value::LoxInstance(sym, _) => write!(f, "LoxInstance({})", sym.name),
        }
    }
}

pub fn interpret(stmts: &[expr::Stmt]) -> Result<String, String> {
    let mut interpreter = Interpreter {
        ..Default::default()
    };
    interpreter.interpret(stmts)?;

    Ok(interpreter.output.join("\n"))
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    line: usize,
    col: i64,
}

#[derive(Debug, Default, Clone)]
pub struct Environment {
    enclosing: Option<Box<Environment>>,
    // SourceLocation is the location of a declaration
    venv: HashMap<String, (Option<Value>, SourceLocation)>,
}

pub enum LookupResult<'a> {
    Ok(&'a Value),
    UndefButDeclared(SourceLocation),
    UndefAndNotDeclared,
}

impl Environment {
    pub fn with_enclosing(enclosing: Environment) -> Environment {
        Environment {
            enclosing: Some(Box::new(enclosing)),
            venv: HashMap::new(),
        }
    }

    pub fn define(&mut self, sym: expr::Symbol, maybe_val: Option<Value>) {
        self.venv.insert(
            sym.name,
            (
                maybe_val,
                SourceLocation {
                    line: sym.line,
                    col: sym.col,
                },
            ),
        );
    }

    pub fn lookup(&self, sym: &expr::Symbol) -> LookupResult {
        match self.venv.get(&sym.name) {
            Some((maybe_val, defn_source_location)) => match maybe_val {
                Some(val) => LookupResult::Ok(&val),
                None => LookupResult::UndefButDeclared(SourceLocation {
                    line: defn_source_location.line,
                    col: defn_source_location.col,
                }),
            },
            None => LookupResult::UndefAndNotDeclared,
        }
    }

    pub fn get(&self, sym: &expr::Symbol) -> Result<&Value, String> {
        match self.lookup(&sym) {
            LookupResult::Ok(val) => Ok(&val),
            LookupResult::UndefButDeclared(source_location) => Err(format!(
                "Use of undefined variable {} at line={},col={}.\
                \nNote: {} was previously declared at line={},col={}, \
                but was never defined.",
                &sym.name, sym.line, sym.col, &sym.name, source_location.line, source_location.col
            )),
            LookupResult::UndefAndNotDeclared => match &self.enclosing {
                Some(enclosing) => enclosing.get(sym),
                None => Err(format!(
                    "Use of undefined variable {} at line={},col-{}.\nNote: {} was never declared.",
                    &sym.name, sym.line, sym.col, &sym.name
                )),
            },
        }
    }

    pub fn assign(&mut self, sym: expr::Symbol, val: &Value) -> Result<(), String> {
        if self.venv.contains_key(&sym.name) {
            self.define(sym, Some(val.clone()));
            return Ok(());
        }

        match &mut self.enclosing {
            Some(enclosing) => enclosing.assign(sym, val),
            None => Err(format!(
                "attempting to assign to undeclared variable at line={},col={}",
                sym.line, sym.col
            )),
        }
    }
}

pub struct Interpreter {
    pub counter: u64,
    pub lox_functions: HashMap<u64, LoxFunction>,
    pub lox_instances: HashMap<u64, LoxInstance>,
    pub lox_classes: HashMap<u64, LoxClass>,
    pub env: Environment,
    pub globals: Environment,
    pub retval: Option<Value>,
    pub output: Vec<String>,
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        let mut globals_venv = HashMap::new();
        globals_venv.insert(
            String::from("clock"),
            (
                Some(Value::NativeFunction(NativeFunction {
                    name: String::from("clock"),
                    arity: 0,
                    callable: |_| {
                        let start = SystemTime::now();
                        let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap();

                        Ok(Value::Number(since_the_epoch.as_millis() as f64))
                    },
                })),
                SourceLocation {
                    line: 1337,
                    col: 1337,
                },
            ),
        );

        let globals = Environment {
            enclosing: None,
            venv: globals_venv,
        };

        Interpreter {
            counter: 0,
            lox_functions: HashMap::new(),
            lox_instances: HashMap::new(),
            lox_classes: HashMap::new(),
            env: Default::default(),
            globals,
            retval: None,
            output: Vec::new(),
        }
    }
}

impl Interpreter {
    pub fn interpret(&mut self, stmts: &[expr::Stmt]) -> Result<(), String> {
        for stmt in stmts.iter() {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn alloc_id(&mut self) -> u64 {
        let res = self.counter;
        self.counter += 1;
        res
    }

    fn create_instance(&mut self, class_name: &expr::Symbol, class_id: u64) -> Value {
        let inst_id = self.alloc_id();
        let inst = LoxInstance {
            class_name: class_name.clone(),
            class_id,
            fields: HashMap::new(),
        };
        self.lox_instances.insert(inst_id, inst);
        Value::LoxInstance(class_name.clone(), inst_id)
    }

    pub fn execute(&mut self, stmt: &expr::Stmt) -> Result<(), String> {
        if self.retval.is_some() {
            return Ok(());
        }

        match stmt {
            expr::Stmt::Expr(e) => match self.interpret_expr(e) {
                Ok(_) => Ok(()),
                Err(err) => Err(err),
            },
            expr::Stmt::ClassDecl(sym, stmt_methods) => {
                let class_id = self.alloc_id();
                self.env
                    .define(sym.clone(), Some(Value::LoxClass(sym.clone(), class_id)));

                let mut methods = HashMap::new();
                for method in stmt_methods.iter() {
                    let func_id = self.alloc_id();

                    methods.insert(method.name.name.clone(), func_id);

                    let lox_function = LoxFunction {
                        name: method.name.clone(),
                        parameters: method.params.clone(),
                        body: method.body.clone(),
                        closure: self.env.clone(),
                    };

                    self.lox_functions.insert(func_id, lox_function);
                }

                let cls = LoxClass {
                    name: sym.clone(),
                    id: class_id,
                    methods: methods,
                };

                self.lox_classes.insert(class_id, cls);
                Ok(())
            }
            expr::Stmt::FunDecl(expr::FunDecl {
                name,
                params: parameters,
                body,
            }) => {
                let func_id = self.alloc_id();
                self.env.define(
                    name.clone(),
                    Some(Value::LoxFunction(name.clone(), func_id)),
                );

                println!("defined {} with id {}.", name.name, func_id);

                let lox_function = LoxFunction {
                    name: name.clone(),
                    parameters: parameters.clone(),
                    body: body.clone(),
                    closure: self.env.clone(),
                };

                self.lox_functions.insert(func_id, lox_function);

                Ok(())
            }
            expr::Stmt::If(cond, if_true, maybe_if_false) => {
                if Interpreter::is_truthy(&self.interpret_expr(cond)?) {
                    return Ok(self.execute(if_true)?);
                }
                if let Some(if_false) = maybe_if_false {
                    return Ok(self.execute(if_false)?);
                }
                Ok(())
            }
            expr::Stmt::Print(e) => match self.interpret_expr(e) {
                Ok(val) => {
                    self.output.push(format!("{}", val));
                    Ok(())
                }
                Err(err) => Err(err),
            },
            expr::Stmt::VarDecl(sym, maybe_expr) => {
                let maybe_val = match maybe_expr {
                    Some(expr) => Some(self.interpret_expr(expr)?),
                    None => None,
                };
                self.env.define(sym.clone(), maybe_val);
                Ok(())
            }
            expr::Stmt::Block(stmts) => {
                self.env = Environment::with_enclosing(self.env.clone());

                for stmt in stmts.iter() {
                    self.execute(stmt)?;
                }

                if let Some(enclosing) = self.env.enclosing.clone() {
                    self.env = *enclosing
                } else {
                    // TODO: how to do this without a runtime check?
                    panic!("impossible");
                }

                Ok(())
            }
            expr::Stmt::While(cond, body) => {
                while Interpreter::is_truthy(&self.interpret_expr(cond)?) {
                    self.execute(body)?;
                }
                Ok(())
            }
            expr::Stmt::Return(_, maybe_res) => {
                self.retval = Some(if let Some(res) = maybe_res {
                    self.interpret_expr(res)?
                } else {
                    Value::Nil
                });
                Ok(())
            }
        }
    }

    pub fn lookup(&self, sym: &expr::Symbol) -> Result<&Value, String> {
        match self.env.get(sym) {
            Ok(val) => Ok(val),
            _ => self.globals.get(sym),
        }
    }

    pub fn interpret_expr(&mut self, expr: &expr::Expr) -> Result<Value, String> {
        match expr {
            expr::Expr::Literal(lit) => Ok(Interpreter::interpret_literal(lit)),
            expr::Expr::Unary(op, e) => self.interpret_unary(*op, e),
            expr::Expr::Binary(lhs, op, rhs) => self.interpret_binary(lhs, *op, rhs),
            expr::Expr::Call(callee, loc, args) => self.call(callee, loc, args),
            expr::Expr::Get(lhs, attr) => self.getattr(lhs, &attr.name),
            expr::Expr::Set(lhs, attr, rhs) => self.setattr(lhs, attr, rhs),
            expr::Expr::Grouping(e) => self.interpret_expr(e),
            expr::Expr::Variable(sym) => match self.lookup(sym) {
                Ok(val) => Ok(val.clone()),
                Err(err) => Err(err),
            },
            expr::Expr::Assign(sym, val_expr) => {
                let val = self.interpret_expr(val_expr)?;

                if let Err(err) = self.env.assign(sym.clone(), &val) {
                    return Err(err);
                }

                Ok(val)
            }
            expr::Expr::Logical(left_expr, expr::LogicalOp::Or, right_expr) => {
                let left = self.interpret_expr(left_expr)?;
                if Interpreter::is_truthy(&left) {
                    Ok(left)
                } else {
                    Ok(self.interpret_expr(right_expr)?)
                }
            }
            expr::Expr::Logical(left_expr, expr::LogicalOp::And, right_expr) => {
                let left = self.interpret_expr(left_expr)?;
                if !Interpreter::is_truthy(&left) {
                    Ok(left)
                } else {
                    Ok(self.interpret_expr(right_expr)?)
                }
            }
        }
    }

    fn getattr(&mut self, lhs: &expr::Expr, attr: &String) -> Result<Value, String> {
        let val = self.interpret_expr(lhs)?;
        match val {
            Value::LoxInstance(_, id) => match self.lox_instances.get(&id) {
                Some(inst) => inst.getattr(&attr, &self),
                None => panic!(
                    "Internal interpreter error: could not find an instance with id {}.",
                    id
                ),
            },
            _ => Err(format!(
                "Only LoxInstance values have attributes. Found {:?}.",
                type_of(&val)
            )),
        }
    }

    fn setattr(
        &mut self,
        lhs_exp: &expr::Expr,
        attr: &expr::Symbol,
        rhs_exp: &expr::Expr,
    ) -> Result<Value, String> {
        let lhs = self.interpret_expr(lhs_exp)?;
        let rhs = self.interpret_expr(rhs_exp)?;
        match lhs {
            Value::LoxInstance(_, id) => match self.lox_instances.get_mut(&id) {
                Some(inst) => {
                    inst.fields.insert(attr.name.clone(), rhs.clone());
                    Ok(rhs)
                }
                None => panic!(
                    "Internal interpreter error: could not find instance with id {}",
                    id
                ),
            },
            _ => Err(format!(
                "Only LoxInstance values have attributes. Found {:?}.",
                type_of(&lhs)
            )),
        }
    }

    fn call(
        &mut self,
        callee_expr: &expr::Expr,
        loc: &expr::SourceLocation,
        arg_exprs: &[expr::Expr],
    ) -> Result<Value, String> {
        let callee = self.interpret_expr(callee_expr)?;

        match as_callable(&self, &callee) {
            Some(callable) => {
                let maybe_args: Result<Vec<_>, _> = arg_exprs
                    .iter()
                    .map(|arg| self.interpret_expr(arg))
                    .collect();

                match maybe_args {
                    Ok(args) => {
                        if args.len() != callable.arity().into() {
                            Err(format!(
                                "Invalid call at line={},col={}: callee has arity {}, but \
                                         was called with {} arguments",
                                loc.line,
                                loc.col,
                                callable.arity(),
                                args.len()
                            ))
                        } else {
                            callable.call(self, &args)
                        }
                    }
                    Err(err) => Err(err),
                }
            }
            None => Err(format!(
                "value {:?} is not callable at line={},col={}",
                callee, loc.line, loc.col
            )),
        }
    }

    fn interpret_binary(
        &mut self,
        lhs_expr: &expr::Expr,
        op: expr::BinaryOp,
        rhs_expr: &expr::Expr,
    ) -> Result<Value, String> {
        let lhs = self.interpret_expr(lhs_expr)?;
        let rhs = self.interpret_expr(rhs_expr)?;

        match (&lhs, op.ty, &rhs) {
            (Value::Number(n1), expr::BinaryOpTy::Less, Value::Number(n2)) => {
                Ok(Value::Bool(n1 < n2))
            }
            (Value::Number(n1), expr::BinaryOpTy::LessEqual, Value::Number(n2)) => {
                Ok(Value::Bool(n1 <= n2))
            }
            (Value::Number(n1), expr::BinaryOpTy::Greater, Value::Number(n2)) => {
                Ok(Value::Bool(n1 > n2))
            }
            (Value::Number(n1), expr::BinaryOpTy::GreaterEqual, Value::Number(n2)) => {
                Ok(Value::Bool(n1 >= n2))
            }
            (Value::Number(n1), expr::BinaryOpTy::Plus, Value::Number(n2)) => {
                Ok(Value::Number(n1 + n2))
            }
            (Value::Number(n1), expr::BinaryOpTy::Minus, Value::Number(n2)) => {
                Ok(Value::Number(n1 - n2))
            }
            (Value::Number(n1), expr::BinaryOpTy::Star, Value::Number(n2)) => {
                Ok(Value::Number(n1 * n2))
            }
            (Value::Number(n1), expr::BinaryOpTy::Slash, Value::Number(n2)) => {
                if *n2 != 0.0 {
                    Ok(Value::Number(n1 / n2))
                } else {
                    Err(format!(
                        "division by zero at line={},col={}",
                        op.line, op.col
                    ))
                }
            }
            (Value::String(s1), expr::BinaryOpTy::Plus, Value::String(s2)) => {
                Ok(Value::String(format!("{}{}", s1, s2)))
            }
            (_, expr::BinaryOpTy::EqualEqual, _) => {
                Ok(Value::Bool(Interpreter::equals(&lhs, &rhs)))
            }
            (_, expr::BinaryOpTy::NotEqual, _) => Ok(Value::Bool(!Interpreter::equals(&lhs, &rhs))),
            _ => Err(format!(
                "invalid operands in binary operator {:?} of type {:?} and {:?} at line={},col={}",
                op.ty,
                type_of(&lhs),
                type_of(&rhs),
                op.line,
                op.col
            )),
        }
    }

    fn equals(lhs: &Value, rhs: &Value) -> bool {
        match (lhs, rhs) {
            (Value::Number(n1), Value::Number(n2)) => (n1 - n2).abs() < f64::EPSILON,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (Value::Nil, Value::Nil) => true,
            (_, _) => false,
        }
    }

    fn interpret_unary(&mut self, op: expr::UnaryOp, expr: &expr::Expr) -> Result<Value, String> {
        let val = self.interpret_expr(expr)?;

        match (op.ty, &val) {
            (expr::UnaryOpTy::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
            (expr::UnaryOpTy::Bang, _) => Ok(Value::Bool(!Interpreter::is_truthy(&val))),
            (_, Value::String(_)) => Err(format!(
                "invalid application of unary op {:?} to object of type String at line={},col={}",
                op.ty, op.line, op.col
            )),
            (_, Value::NativeFunction(_)) => Err(format!(
                "invalid application of unary op {:?} to object of type NativeFunction at line={},col={}",
                op.ty, op.line, op.col
            )),
            (_, Value::LoxFunction(_, _)) => Err(format!(
                "invalid application of unary op {:?} to object of type LoxFunction at line={},col={}",
                op.ty, op.line, op.col
            )),
            (_, Value::LoxClass(_, _)) => Err(format!(
                "invalid application of unary op {:?} to object of type LoxClass at line={},col={}",
                op.ty, op.line, op.col
            )),
            (_, Value::LoxInstance(class_name, _)) => Err(format!(
                "invalid application of unary op {:?} to object of type {:?} at line={},col={}",
                class_name.name, op.ty, op.line, op.col
            )),
            (expr::UnaryOpTy::Minus, Value::Bool(_)) => Err(format!(
                "invalid application of unary op {:?} to object of type Bool at line={},col={}",
                op.ty, op.line, op.col
            )),
            (_, Value::Nil) => Err(format!(
                "invalid application of unary op {:?} to nil at line={},col={}",
                op.ty, op.line, op.col
            )),
        }
    }

    fn is_truthy(val: &Value) -> bool {
        match val {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }

    fn interpret_literal(lit: &expr::Literal) -> Value {
        match lit {
            expr::Literal::Number(n) => Value::Number(*n),
            expr::Literal::String(s) => Value::String(s.clone()),
            expr::Literal::True => Value::Bool(true),
            expr::Literal::False => Value::Bool(false),
            expr::Literal::Nil => Value::Nil,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::interpreter;
    use crate::parser;
    use crate::scanner;

    fn evaluate(code: &str) -> Result<String, String> {
        let tokens = scanner::scan_tokens(code.to_string()).unwrap();
        let stmts = parser::parse(tokens)?;
        interpreter::interpret(&stmts)
    }

    #[test]
    fn test_fact() {
        fn fact(n: i32) -> i32 {
            if n <= 1 {
                return 1;
            }
            return n * fact(n - 1);
        };

        let res = evaluate(
            "fun fact(n) { \n\
               if (n <= 1) {\n\
                   return 1; \n\
               }\n\
               return n * fact(n - 1); \n\
             } \n\
             print fact(10); ",
        );
        match res {
            Ok(output) => assert_eq!(output, format!("{}", fact(10))),
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_invalid_binary_operands() {
        let res = evaluate("1 + \"string\";");

        match res {
            Ok(output) => panic!(output),
            Err(err) => assert!(err.starts_with("invalid operands in binary operator")),
        }
    }

    #[test]
    fn test_invalid_unary_operand() {
        let res = evaluate("-\"cat\";");

        match res {
            Ok(output) => panic!(output),
            Err(err) => {
                assert!(err
                    .starts_with("invalid application of unary op Minus to object of type String"))
            }
        }
    }

    #[test]
    fn return_not_enclosed_in_fundecl() {
        let res = evaluate("return 1;");

        match res {
            Ok(output) => panic!(output),
            Err(err) => assert!(err.starts_with("return statement not enclosed in a FunDecl at")),
        }
    }

    #[test]
    fn test_clock() {
        let res = evaluate("print clock();");

        match res {
            Ok(_) => {}
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_for() {
        let res = evaluate(
            "for (var i = 0; i < 5; i = i + 1) \n\
             { \n\
                 print(i); \n\
             }",
        );

        match res {
            Ok(output) => assert_eq!(output, "0\n1\n2\n3\n4"),
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_lox_funcs() {
        let res = evaluate(
            "fun sayHi(first, last) {\n\
               return \"Hi, \" + first + \" \" + last + \"!\";\n\
             }\n\
             \n\
             print sayHi(\"Dear\", \"Reader\");\n\
             \n\
             fun add(x,y,z) {\n\
                 return x + y + z;\n\
             }\n\
             \n\
             print add(1,2,3);",
        );

        match res {
            Ok(output) => assert_eq!(output, "'Hi, Dear Reader!'\n6"),
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_scopes() {
        let res = evaluate(
            "var a = \"global a\";\
                            var b = \"global b\";\n\
                            var c = \"global c\";\n\
                            {
                              var a = \"outer a\";\n\
                              var b = \"outer b\";\n\
                              {
                                var a = \"inner a\";\n\
                                print a;\n\
                                print b;\n\
                                print c;\n\
                              }
                              print a;\n\
                              print b;\n\
                              print c;\n\
                            }
                            print a;\n\
                            print b;\n\
                            print c;\n",
        );

        match res {
            Ok(output) => assert_eq!(
                output,
                "'inner a'\n\
                 'outer b'\n\
                 'global c'\n\
                 'outer a'\n\
                 'outer b'\n\
                 'global c'\n\
                 'global a'\n\
                 'global b'\n\
                 'global c'"
            ),
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_implicit_return_nil() {
        let res = evaluate("fun f() {} print f();");

        match res {
            Ok(output) => assert_eq!(output, "nil"),
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_closures_1() {
        let res = evaluate(
            "fun f(n) {\n\
               var m = 2;\n\
               fun g(p) {\n\
                 return p + m;\n\
               }\n\
               return g(n);\n\
             }\n\
             print f(1);",
        );

        match res {
            Ok(output) => assert_eq!(output, "3"),
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_closures_2() {
        let res = evaluate(
            "fun mkfun(n) {\n\
               fun f(m) {\n\
                 return m + n;\n\
                 }\n\
               return f;\n\
               }\n\
             print mkfun(2)(3);",
        );
        match res {
            Ok(output) => assert_eq!(output, "5"),
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_classes_1() {
        let res = evaluate(
            "class DevonshireCream {\n\
               serveOn() {\n\
                 return \"Scones\";\n\
               }\n\
             }\n\
             \n\
             print DevonshireCream;",
        );

        match res {
            Ok(output) => assert_eq!(output, "LoxClass(DevonshireCream)"),
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_classes_2() {
        let res = evaluate(
            "class DevonshireCream {\n\
               serveOn() {\n\
                 return \"Scones\";\n\
               }\n\
             }\n\
             \n\
             var inst = DevonshireCream();\n\
             print inst;",
        );

        match res {
            Ok(output) => assert_eq!(output, "LoxInstance(DevonshireCream)"),
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_setattr_1() {
        let res = evaluate(
            "class Foo {}\n\
             var foo = Foo();\n\
             foo.attr = 42;\n\
             print foo.attr;",
        );

        match res {
            Ok(output) => assert_eq!(output, "42"),
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_setattr_2() {
        let res = evaluate(
            "class Bar {}\n\
             class Foo {}\n\
             var foo = Foo();\n\
             foo.bar = Bar();\n\
             foo.bar.baz = \"baz\";\n\
             print foo.bar.baz;",
        );

        match res {
            Ok(output) => assert_eq!(output, "\'baz\'"),
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_methods_1() {
        let res = evaluate(
            "class Bacon {\
                eat() {\
                  print \"Crunch crunch crunch!\";\
                }\
              }\
              \
              Bacon().eat();",
        );

        match res {
            Ok(output) => assert_eq!(output, "\'Crunch crunch crunch!\'"),
            Err(err) => panic!(err),
        }
    }
}
