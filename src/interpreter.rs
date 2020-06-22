use std::collections::HashMap;

use crate::expr;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

#[derive(Debug)]
pub enum Type {
    Number,
    String,
    Bool,
    NilType,
}

pub fn type_of(val: &Value) -> Type {
    match val {
        Value::Number(_) => Type::Number,
        Value::String(_) => Type::String,
        Value::Bool(_) => Type::Bool,
        Value::Nil => Type::NilType,
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "'{}'", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
        }
    }
}

pub fn interpret(stmts: &[expr::Stmt]) -> Result<(), String> {
    let mut interpreter = Interpreter {
        ..Default::default()
    };
    interpreter.interpret(stmts)
}

#[derive(Debug, Clone)]
struct SourceLocation {
    line: usize,
    col: i64,
}

#[derive(Default, Clone)]
struct Environment {
    enclosing: Option<Box<Environment>>,
    // SourceLocation is the location of a declaration
    venv: HashMap<String, (Option<Value>, SourceLocation)>,
}

enum LookupResult<'a> {
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

#[derive(Default)]
struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn interpret(&mut self, stmts: &[expr::Stmt]) -> Result<(), String> {
        for stmt in stmts.iter() {
            self.execute(stmt)?;
        }
        Ok(())
    }

    pub fn execute(&mut self, stmt: &expr::Stmt) -> Result<(), String> {
        match stmt {
            expr::Stmt::Expr(e) => match self.interpret_expr(e) {
                Ok(_) => Ok(()),
                Err(err) => Err(err),
            },
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
                    println!("{}", val);
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
                    assert!(false, "impossible");
                }

                Ok(())
            }
        }
    }

    pub fn interpret_expr(&mut self, expr: &expr::Expr) -> Result<Value, String> {
        match expr {
            expr::Expr::Literal(lit) => Ok(Interpreter::interpret_literal(lit)),
            expr::Expr::Unary(op, e) => self.interpret_unary(*op, e),
            expr::Expr::Binary(lhs, op, rhs) => self.interpret_binary(lhs, *op, rhs),
            expr::Expr::Grouping(e) => self.interpret_expr(e),
            expr::Expr::Variable(sym) => match self.env.get(sym) {
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
            (_, _, _) => Err(format!(
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
