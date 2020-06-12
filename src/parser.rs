use crate::expr;
use crate::scanner;

#[allow(dead_code)]
#[derive(Default)]
pub struct Parser {
    tokens: Vec<scanner::Token>,
    current: usize,
    err: Option<String>,
}

impl Parser {
    fn expression(&mut self) -> expr::Expr {
        self.equality()
    }

    fn comparison(&mut self) -> expr::Expr {
        unimplemented!()
    }

    fn equality(&mut self) -> expr::Expr {
        let expr = self.comparison();

        while self.match_tokens(vec![
            scanner::TokenType::BangEqual,
            scanner::TokenType::EqualEqual,
        ]) {
            let operator_token: scanner::TokenType = self.previous().ty;
            let right = Box::new(self.comparison());

            let binopMaybe = self.op_token_to_binop(operator_token);

            match binopMaybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    return expr::Expr::Binary(left, binop, right);
                }
                Err(err) => {
                    self.err = Some(err);
                }
            }
        }
        expr
    }

    fn op_token_to_binop(&self, tok: scanner::TokenType) -> Result<expr::BinaryOp, String> {
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

    fn match_tokens(&mut self, types: Vec<scanner::TokenType>) -> bool {
        for ty in types.iter() {
            if self.check(*ty) {
                self.advance();
                return true;
            }
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
        self.peek().ty == scanner::TokenType::Eof || self.err.is_some()
    }

    fn peek(&self) -> &scanner::Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &scanner::Token {
        &self.tokens[self.current - 1]
    }
}
