use crate::expr;
use crate::scanner;

#[allow(dead_code)]
#[derive(Default)]
struct Parser {
    tokens: Vec<scanner::Token>,
    current: usize,
}

pub fn parse(tokens: Vec<scanner::Token>) -> Result<expr::Expr, String> {
    let mut p = Parser {
        tokens,
        ..Default::default()
    };
    let expr_or_err = p.parse();

    match expr_or_err {
        Ok(expr) => {
            if !p.is_at_end() {
                Err(format!("unexpected {:?}", p.tokens[p.current]))
            } else {
                Ok(expr)
            }
        }
        Err(err) => Err(err),
    }
}

/*
Recursive descent using the following grammar

expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
multiplication → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "false" | "true" | "nil"
               | "(" expression ")" ;
*/
impl Parser {
    pub fn parse(&mut self) -> Result<expr::Expr, String> {
        self.expression()
    }

    fn expression(&mut self) -> Result<expr::Expr, String> {
        self.equality()
    }

    fn comparison(&mut self) -> Result<expr::Expr, String> {
        let mut expr = self.addition()?;

        while self.match_one_of(vec![
            scanner::TokenType::Greater,
            scanner::TokenType::GreaterEqual,
            scanner::TokenType::Less,
            scanner::TokenType::LessEqual,
        ]) {
            let operator_token: scanner::TokenType = self.previous().ty;
            let right = Box::new(self.addition()?);
            let binop_maybe = Parser::op_token_to_binop(operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = expr::Expr::Binary(left, binop, right);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    fn addition(&mut self) -> Result<expr::Expr, String> {
        let mut expr = self.multiplication()?;

        while self.match_one_of(vec![scanner::TokenType::Minus, scanner::TokenType::Plus]) {
            let operator_token: scanner::TokenType = self.previous().ty;
            let right = Box::new(self.multiplication()?);
            let binop_maybe = Parser::op_token_to_binop(operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = expr::Expr::Binary(left, binop, right);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<expr::Expr, String> {
        let mut expr = self.unary()?;

        while self.match_one_of(vec![scanner::TokenType::Slash, scanner::TokenType::Star]) {
            let operator_token: scanner::TokenType = self.previous().ty;
            let right = Box::new(self.unary()?);
            let binop_maybe = Parser::op_token_to_binop(operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = expr::Expr::Binary(left, binop, right);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<expr::Expr, String> {
        if self.match_one_of(vec![scanner::TokenType::Bang, scanner::TokenType::Minus]) {
            let operator_token: scanner::TokenType = self.previous().ty;
            let right = Box::new(self.unary()?);
            let unary_op_maybe = Parser::op_token_to_unary_op(operator_token);

            return match unary_op_maybe {
                Ok(unary_op) => Ok(expr::Expr::Unary(unary_op, right)),
                Err(err) => Err(err),
            };
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<expr::Expr, String> {
        if self.matches(scanner::TokenType::False) {
            return Ok(expr::Expr::Literal(expr::Literal::False));
        }
        if self.matches(scanner::TokenType::True) {
            return Ok(expr::Expr::Literal(expr::Literal::True));
        }
        if self.matches(scanner::TokenType::Nil) {
            return Ok(expr::Expr::Literal(expr::Literal::Nil));
        }
        if self.matches(scanner::TokenType::Number) {
            match &self.previous().literal {
                Some(scanner::Literal::Number(n)) => {
                    return Ok(expr::Expr::Literal(expr::Literal::Number(*n)))
                }
                Some(l) => panic!(
                    "internal error in parser: when parsing number, found literal {:?}",
                    l
                ),
                None => panic!("internal error in parser: when parsing number, found no literal"),
            }
        }
        if self.matches(scanner::TokenType::String) {
            match &self.previous().literal {
                Some(scanner::Literal::Str(s)) => {
                    return Ok(expr::Expr::Literal(expr::Literal::String(s.clone())))
                }
                Some(l) => panic!(
                    "internal error in parser: when parsing string, found literal {:?}",
                    l
                ),
                None => panic!("internal error in parser: when parsing string, found no literal"),
            }
        }
        if self.matches(scanner::TokenType::LeftParen) {
            let expr = Box::new(self.expression()?);
            match self.consume(
                scanner::TokenType::RightParen,
                "Expected ')' after expression.",
            ) {
                Err(err) => return Err(err),
                _ => {}
            }
            return Ok(expr::Expr::Grouping(expr));
        }

        Err(format!("Expected expression at {:?}", self.peek()))
    }

    fn consume(
        &mut self,
        tok: scanner::TokenType,
        on_err_str: &str,
    ) -> Result<&scanner::Token, String> {
        if self.check(tok) {
            return Ok(self.advance());
        }
        Err(format!(
            "Expected token type {:?}, but found token {:?}: {}",
            tok,
            self.peek(),
            on_err_str
        ))
    }

    fn op_token_to_unary_op(tok: scanner::TokenType) -> Result<expr::UnaryOp, String> {
        match tok {
            scanner::TokenType::Minus => Ok(expr::UnaryOp::Minus),
            scanner::TokenType::Bang => Ok(expr::UnaryOp::Bang),
            _ => Err(format!("invalid token in unary op {:?}", tok)),
        }
    }

    fn equality(&mut self) -> Result<expr::Expr, String> {
        let expr = self.comparison()?;

        while self.match_one_of(vec![
            scanner::TokenType::BangEqual,
            scanner::TokenType::EqualEqual,
        ]) {
            let operator_token: scanner::TokenType = self.previous().ty;
            let right = Box::new(self.comparison()?);

            let binop_maybe = Parser::op_token_to_binop(operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    return Ok(expr::Expr::Binary(left, binop, right));
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    fn op_token_to_binop(tok: scanner::TokenType) -> Result<expr::BinaryOp, String> {
        match tok {
            scanner::TokenType::EqualEqual => Ok(expr::BinaryOp::EqualEqual),
            scanner::TokenType::BangEqual => Ok(expr::BinaryOp::NotEqual),
            scanner::TokenType::Less => Ok(expr::BinaryOp::Less),
            scanner::TokenType::LessEqual => Ok(expr::BinaryOp::LessEqual),
            scanner::TokenType::Greater => Ok(expr::BinaryOp::Greater),
            scanner::TokenType::GreaterEqual => Ok(expr::BinaryOp::GreaterEqual),
            scanner::TokenType::Plus => Ok(expr::BinaryOp::Plus),
            scanner::TokenType::Minus => Ok(expr::BinaryOp::Minus),
            scanner::TokenType::Star => Ok(expr::BinaryOp::Star),
            scanner::TokenType::Slash => Ok(expr::BinaryOp::Slash),
            _ => Err(format!("invalid token in binary operation {:?}", tok)),
        }
    }

    fn match_one_of(&mut self, types: Vec<scanner::TokenType>) -> bool {
        for ty in types.iter() {
            if self.matches(*ty) {
                return true;
            }
        }
        false
    }

    fn matches(&mut self, ty: scanner::TokenType) -> bool {
        if self.check(ty) {
            self.advance();
            return true;
        }
        false
    }

    fn check(&self, ty: scanner::TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().ty == ty
    }

    fn advance(&mut self) -> &scanner::Token {
        if !self.is_at_end() {
            self.current += 1
        }

        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().ty == scanner::TokenType::Eof
    }

    fn peek(&self) -> &scanner::Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &scanner::Token {
        &self.tokens[self.current - 1]
    }
}
