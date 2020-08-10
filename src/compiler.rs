use crate::bytecode;
use crate::scanner;

#[derive(Default)]
pub struct Compiler {
    tokens: Vec<scanner::Token>,
    current_chunk: bytecode::Chunk,
    current: usize,
}

#[derive(Eq, PartialEq, PartialOrd, Copy, Clone, Debug)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

#[derive(Debug, Copy, Clone)]
enum ParseFn {
    Grouping,
    Unary,
    Binary,
    Number,
    Literal,
}

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl Compiler {
    pub fn compile(&mut self, input: String) -> Result<bytecode::Chunk, String> {
        match scanner::scan_tokens(input) {
            Ok(tokens) => {
                self.tokens = tokens;
                self.current_chunk = bytecode::Chunk::default();
                self.expression()?;
                Ok(std::mem::take(&mut self.current_chunk))
            }
            Err(err) => Err(err),
        }
    }

    fn expression(&mut self) -> Result<(), String> {
        self.parse_precedence(Precedence::Assignment)
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
        let tok = self.previous().clone();

        match tok.literal {
            Some(scanner::Literal::Number(n)) => {
                self.emit_number(n, tok.line);
                Ok(())
            }
            _ => panic!(
                "Expected number at line={},col={}. current token {:?}",
                tok.line, tok.col, tok
            ),
        }
    }

    fn literal(&mut self) -> Result<(), String> {
        let tok = self.previous().clone();

        match tok.ty {
            scanner::TokenType::Nil => {
                self.emit_op(bytecode::Op::Nil, tok.line);
                Ok(())
            }
            scanner::TokenType::True => {
                self.emit_op(bytecode::Op::True, tok.line);
                Ok(())
            }
            scanner::TokenType::False => {
                self.emit_op(bytecode::Op::False, tok.line);
                Ok(())
            }
            _ => {
                panic!("shouldn't get in literal with tok = {:?}.", tok);
            }
        }
    }

    fn binary(&mut self) -> Result<(), String> {
        let operator = self.previous().clone();

        let rule = Compiler::get_rule(operator.ty);

        self.parse_precedence(Compiler::next_precedence(rule.precedence))?;

        match operator.ty {
            scanner::TokenType::Plus => {
                self.emit_op(bytecode::Op::Add, operator.line);
                Ok(())
            }
            scanner::TokenType::Minus => {
                self.emit_op(bytecode::Op::Subtract, operator.line);
                Ok(())
            }
            scanner::TokenType::Star => {
                self.emit_op(bytecode::Op::Multiply, operator.line);
                Ok(())
            }
            scanner::TokenType::Slash => {
                self.emit_op(bytecode::Op::Divide, operator.line);
                Ok(())
            }
            _ => Err(format!(
                "Invalid token {:?} in binary expression at line={},col={}.",
                operator.ty, operator.line, operator.col
            )),
        }
    }

    fn unary(&mut self) -> Result<(), String> {
        let operator = self.previous().clone();

        self.parse_precedence(Precedence::Unary)?;

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

    fn emit_number(&mut self, n: f64, lineno: usize) {
        let const_idx = self.current_chunk.add_constant(n);
        self.emit_op(bytecode::Op::Constant(const_idx), lineno);
    }

    fn emit_op(&mut self, op: bytecode::Op, lineno: usize) {
        self.current_chunk.code.push((op, bytecode::Lineno(lineno)))
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), String> {
        self.advance();
        println!("parse_precedence {:?} {:?}", self.previous(), precedence);

        match Compiler::get_rule(self.previous().ty).prefix {
            Some(parse_fn) => self.apply_parse_fn(parse_fn)?,
            None => return Err(self.error("Expected expression.")),
        }

        while precedence <= Compiler::get_rule(self.current().ty).precedence {
            self.advance();
            match Compiler::get_rule(self.previous().ty).infix {
                Some(parse_fn) => self.apply_parse_fn(parse_fn)?,
                None => panic!("could not find infix rule to apply"),
            }
        }

        Ok(())
    }

    fn error(&self, what: &str) -> String {
        let tok = self.previous();
        format!("{} at line={},col={}.", what, tok.line, tok.col)
    }

    fn apply_parse_fn(&mut self, parse_fn: ParseFn) -> Result<(), String> {
        match parse_fn {
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
            ParseFn::Number => self.number(),
            ParseFn::Literal => self.literal(),
        }
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

    fn current(&self) -> &scanner::Token {
        &self.tokens[self.current]
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

    fn next_precedence(precedence: Precedence) -> Precedence {
        match precedence {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => panic!("primary has no next precedence!"),
        }
    }

    fn get_rule(operator: scanner::TokenType) -> ParseRule {
        match operator {
            scanner::TokenType::LeftParen => ParseRule {
                prefix: Some(ParseFn::Grouping),
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::RightParen => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::LeftBrace => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::RightBrace => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Comma => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Dot => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Minus => ParseRule {
                prefix: Some(ParseFn::Unary),
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Term,
            },
            scanner::TokenType::Plus => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Term,
            },
            scanner::TokenType::Semicolon => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::Term,
            },
            scanner::TokenType::Slash => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Factor,
            },
            scanner::TokenType::Star => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Factor,
            },
            scanner::TokenType::Bang => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::BangEqual => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Equal => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::EqualEqual => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Greater => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::GreaterEqual => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Less => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::LessEqual => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Identifier => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::String => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Number => ParseRule {
                prefix: Some(ParseFn::Number),
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::And => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Class => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Else => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::False => ParseRule {
                prefix: Some(ParseFn::Literal),
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::For => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Fun => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::If => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Nil => ParseRule {
                prefix: Some(ParseFn::Literal),
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Or => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Print => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Return => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Super => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::This => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::True => ParseRule {
                prefix: Some(ParseFn::Literal),
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Var => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::While => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::Eof => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }
}
