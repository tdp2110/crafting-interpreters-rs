use crate::expr;

#[derive(Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

pub fn interpret(expr: &expr::Expr) -> Result<Value, String> {
    match expr {
        expr::Expr::Literal(lit) => Ok(interpret_literal(lit)),
        expr::Expr::Unary(op, e) => interpret_unary(*op, *&e),
        expr::Expr::Binary(lhs, op, rhs) => interpret_binary(*&lhs, *op, *&rhs),
        expr::Expr::Grouping(e) => interpret(&*e),
    }
}

fn interpret_binary(
    lhs_expr: &expr::Expr,
    op: expr::BinaryOp,
    rhs_expr: &expr::Expr,
) -> Result<Value, String> {
    let lhs = interpret(lhs_expr)?;
    let rhs = interpret(rhs_expr)?;

    match (&lhs, op, &rhs) {
        (Value::Number(n1), expr::BinaryOp::Less, Value::Number(n2)) => Ok(Value::Bool(n1 < n2)),
        (Value::Number(n1), expr::BinaryOp::LessEqual, Value::Number(n2)) => {
            Ok(Value::Bool(n1 <= n2))
        }
        (Value::Number(n1), expr::BinaryOp::Greater, Value::Number(n2)) => Ok(Value::Bool(n1 > n2)),
        (Value::Number(n1), expr::BinaryOp::GreaterEqual, Value::Number(n2)) => {
            Ok(Value::Bool(n1 >= n2))
        }
        (Value::Number(n1), expr::BinaryOp::Plus, Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
        (Value::Number(n1), expr::BinaryOp::Minus, Value::Number(n2)) => Ok(Value::Number(n1 - n2)),
        (Value::Number(n1), expr::BinaryOp::Star, Value::Number(n2)) => Ok(Value::Number(n1 * n2)),
        (Value::Number(n1), expr::BinaryOp::Slash, Value::Number(n2)) => Ok(Value::Number(n1 / n2)), // What about exceptions!!!!??!?!?
        (Value::String(s1), expr::BinaryOp::Plus, Value::String(s2)) => {
            Ok(Value::String(format!("{}{}", s1, s2)))
        }
        (_, expr::BinaryOp::EqualEqual, _) => Ok(Value::Bool(equals(&lhs, &rhs))),
        (_, expr::BinaryOp::NotEqual, _) => Ok(Value::Bool(!equals(&lhs, &rhs))),
        (_, _, _) => Err(format!(
            "invalid operands in binary expression ({:?},{:?},{:?})",
            lhs, op, rhs
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
    let val = interpret(expr)?;

    match (op, val) {
        (expr::UnaryOp::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
        (expr::UnaryOp::Bang, Value::Number(n)) => Ok(Value::Bool(n == 0.0)),
        (_, Value::String(_)) => Err(format!(
            "invalid application of unary op {:?} to object of type string",
            op
        )),
        (expr::UnaryOp::Minus, Value::Bool(_)) => Err(format!(
            "invalid application of unary op {:?} to object of type Bool",
            op
        )),
        (expr::UnaryOp::Bang, Value::Bool(b)) => Ok(Value::Bool(!b)),
        (_, Value::Nil) => Err(format!("invalid application of unary op {:?} to nil", op)),
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
