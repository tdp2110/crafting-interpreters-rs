#[derive(Debug)]
pub enum TokenType {
  // Single-character tokens.
  LeftParen, RightParen, LeftBrace, RightBrace,
  Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

  // One or two character tokens.
  Bang, BangEqual,
  Equal, EqualEqual,
  Greater, GreaterEqual,
  Less, LessEqual,

  // Literals.
  Identifier, String, Number,

  // Keywords.
  And, Class, Else, False, Fun, For, If, Nil, Or,
  Print, Return, Super, This, True, Var, While,

  Eof
}

#[derive(Debug)]
pub enum Literal {
    Identifier(String),
    Str(String),
    Number(f64)
}

#[derive(Debug)]
pub struct Token {
    ty: TokenType,
    lexeme: String,
    literal: Option<Literal>,
    line: usize
}

pub fn scan_tokens(input: String) -> Result<Vec<Token>, String> {
    let mut scanner: Scanner = Default::default();

    scanner.scan_tokens(input);

    match scanner.err {
        Some(err) => Err(err),
        None => Ok(scanner.tokens)
    }
}

struct Scanner {
    source: String,
    tokens: Vec<Token>,
    err: Option<String>,
    start: usize,
    current: usize,
    line: usize
}

impl Default for Scanner {
    fn default () -> Scanner {
        Scanner {
            source: String::new(),
            tokens: Vec::new(),
            err: None,
            start: 0,
            current: 0,
            line: 1
        }
    }
}

impl Scanner {
    fn scan_tokens(&mut self, input: String) {
        while self.not_done() {
            self.start = self.current;
            self.scan_token();
        }

        match self.err {
            Some(_) => return,
            None => self.tokens.push(Token { ty: TokenType::Eof,
                                             lexeme: String::new(),
                                             literal: None,
                                             line: self.line })

        }
    }

    fn advance(&mut self) -> char{
        unimplemented!()
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            c => self.err = Some(format!("scanner can't handle {}", c))
        }
        unimplemented!()
    }

    fn add_token(&mut self, token_type: TokenType) {
        let text = String::from_utf8(
            self.source.as_bytes()[self.start..self.current].to_vec()
        ).unwrap();

        self.tokens.push(Token { ty: token_type,
                                 lexeme: text,
                                 literal: None,
                                 line: self.line })
    }

    fn not_done(&self) -> bool {
        unimplemented!()
    }
}
