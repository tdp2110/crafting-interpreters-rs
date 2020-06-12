#[allow(dead_code)]
pub enum Expr {
    Literal(Literal),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),
}

#[allow(dead_code)]
pub enum UnaryOp {
    Minus,
    Bang,
}

#[allow(dead_code)]
pub enum BinaryOp {
    EqualsEquals,
    NotEquals,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    Slash,
}

#[allow(dead_code)]
pub enum Literal {
    Number,
    String,
    True,
    False,
    Nil,
}
