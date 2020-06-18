use crate::expr;
use std::fmt;

#[derive(Debug)]
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

pub fn interpret(stmts: &Vec<expr::Stmt>) -> Result<(), String> {
    for stmt in stmts.iter() {
        execute(stmt)?;
    }
    Ok(())
}

pub fn execute(stmt: &expr::Stmt) -> Result<(), String> {
    match stmt {
        expr::Stmt::Expr(e) => match interpret_expr(e) {
            Ok(_) => Ok(()),
            Err(err) => Err(err),
        },
        expr::Stmt::Print(e) => match interpret_expr(e) {
            Ok(val) => {
                println!("{}", val);
                Ok(())
            }
            Err(err) => Err(err),
        },
    }
}

pub fn interpret_expr(expr: &expr::Expr) -> Result<Value, String> {
    match expr {
        expr::Expr::Literal(lit) => Ok(interpret_literal(lit)),
        expr::Expr::Unary(op, e) => interpret_unary(*op, *&e),
        expr::Expr::Binary(lhs, op, rhs) => interpret_binary(*&lhs, *op, *&rhs),
        expr::Expr::Grouping(e) => interpret_expr(&*e),
    }
}

fn interpret_binary(
    lhs_expr: &expr::Expr,
    op: expr::BinaryOp,
    rhs_expr: &expr::Expr,
) -> Result<Value, String> {
    let lhs = interpret_expr(lhs_expr)?;
    let rhs = interpret_expr(rhs_expr)?;

    match (&lhs, op.ty, &rhs) {
        (Value::Number(n1), expr::BinaryOpTy::Less, Value::Number(n2)) => Ok(Value::Bool(n1 < n2)),
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
        (_, expr::BinaryOpTy::EqualEqual, _) => Ok(Value::Bool(equals(&lhs, &rhs))),
        (_, expr::BinaryOpTy::NotEqual, _) => Ok(Value::Bool(!equals(&lhs, &rhs))),
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
        (Value::Number(n1), Value::Number(n2)) => n1 == n2,
        (Value::String(s1), Value::String(s2)) => s1 == s2,
        (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
        (Value::Nil, Value::Nil) => true,
        (_, _) => false,
    }
}

fn interpret_unary(op: expr::UnaryOp, expr: &expr::Expr) -> Result<Value, String> {
    let val = interpret_expr(expr)?;

    match (op.ty, &val) {
        (expr::UnaryOpTy::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
        (expr::UnaryOpTy::Bang, _) => Ok(Value::Bool(!is_truthy(&val))),
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
