use crate::bytecode;
use crate::scanner;

#[derive(Default)]
pub struct Compiler {
    tokens: Vec<scanner::Token>,
    current_chunk: bytecode::Chunk,
    current: usize,
}

impl Compiler {
    pub fn compile(&mut self, input: String) -> Result<bytecode::Chunk, String> {
        match scanner::scan_tokens(input) {
            Ok(tokens) => {
                self.tokens = tokens;
                self.current_chunk = bytecode::Chunk::default();
                self.expression()?;
                Ok(std::mem::replace(
                    &mut self.current_chunk,
                    bytecode::Chunk::default(),
                ))
            }
            Err(err) => Err(err),
        }
    }

    fn expression(&mut self) -> Result<(), String> {
        unimplemented!();
    }

    fn grouping(&mut self) -> Result<(), String> {
        self.expression()?;
        match self.consume(
            scanner::TokenType::RightParen,
            "Expected ')' after expression.",
        ) {
            Ok(_) => Ok(()),
            Err(err) => Err(err),
        }
    }

    fn number(&mut self) -> Result<(), String> {
        let current_tok = &self.tokens[self.current];
        let lineno = current_tok.line;
        match current_tok.literal {
            Some(scanner::Literal::Number(n)) => {
                self.emit_constant(n, lineno);
                Ok(())
            }
            _ => Err(format!(
                "Expected number at line={},col={}",
                current_tok.line, current_tok.col
            )),
        }
    }

    fn unary(&mut self) -> Result<(), String> {
        let operator = self.previous().clone();

        self.expression()?;

        match operator.ty {
            scanner::TokenType::Minus => {
                self.emit_op(bytecode::Op::Negate, operator.line);
                Ok(())
            }
            _ => Err(format!(
                "Invalid token in unary op {:?} at line={},col={}",
                operator.ty, operator.line, operator.col
            )),
        }
    }

    fn emit_constant(&mut self, n: f64, lineno: usize) {
        let const_idx = self.current_chunk.add_constant(n);
        self.emit_op(bytecode::Op::Constant(const_idx), lineno);
    }

    fn emit_op(&mut self, op: bytecode::Op, lineno: usize) {
        self.current_chunk.code.push((op, bytecode::Lineno(lineno)))
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
            "Expected token {:?}, but found token {:?} at line={},col={}: {}",
            tok,
            self.peek().ty,
            self.peek().line,
            self.peek().col,
            on_err_str
        ))
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

    fn previous(&self) -> &scanner::Token {
        &self.tokens[self.current - 1]
    }

    fn is_at_end(&self) -> bool {
        self.peek().ty == scanner::TokenType::Eof
    }

    fn peek(&self) -> &scanner::Token {
        &self.tokens[self.current]
    }
}
