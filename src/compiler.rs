use crate::bytecode;
use crate::scanner;
use crate::syntax_extensions;

#[derive(Debug)]
struct Local {
    name: scanner::Token,
    depth: i64,
    is_captured: bool,
}

struct ClassCompiler {
    has_superclass: bool,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum FunctionType {
    Function,
    Script,
    Method,
    Initializer,
}

pub struct Compiler {
    tokens: Vec<scanner::Token>,
    token_idx: usize,
    levels: Vec<Level>,
    level_idx: usize,
    current_class: Option<ClassCompiler>,
    extensions: syntax_extensions::Extensions,
}

impl Default for Compiler {
    fn default() -> Compiler {
        Compiler {
            tokens: Default::default(),
            token_idx: 0,
            levels: vec![Default::default()],
            level_idx: 0,
            current_class: None,
            extensions: Default::default(),
        }
    }
}

pub struct Level {
    function: bytecode::Function,
    function_type: FunctionType,
    locals: Vec<Local>,
    scope_depth: i64,
    upvals: Vec<bytecode::UpvalueLoc>,
}

impl Default for Level {
    fn default() -> Level {
        Level {
            function: Default::default(),
            function_type: FunctionType::Script,
            locals: vec![Local {
                name: scanner::Token {
                    ty: scanner::TokenType::Identifier,
                    lexeme: Default::default(),
                    literal: Some(scanner::Literal::Identifier("".to_string())),
                    line: 0,
                    col: -1,
                },
                depth: 0,
                is_captured: false,
            }],
            scope_depth: 0,
            upvals: Default::default(),
        }
    }
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
    String,
    Variable,
    And,
    Or,
    Call,
    Dot,
    This,
    Super,
    List,
    Subscript,
}

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

enum Resolution {
    Local(usize),
    Global,
    Upvalue(usize),
}

impl Compiler {
    pub fn compile(
        input: String,
        extensions: syntax_extensions::Extensions,
    ) -> Result<bytecode::Function, String> {
        let mut compiler = Compiler {
            extensions,
            ..Default::default()
        };
        match scanner::scan_tokens(input) {
            Ok(tokens) => {
                compiler.tokens = tokens;

                while !compiler.is_at_end() {
                    compiler.declaration()?;
                }

                compiler.emit_return();

                Ok(std::mem::take(&mut compiler.current_level_mut().function))
            }
            Err(err) => Err(err),
        }
    }

    fn declaration(&mut self) -> Result<(), String> {
        if self.matches(scanner::TokenType::Class) {
            self.class_decl()
        } else if self.matches(scanner::TokenType::Fun) {
            self.fun_decl()
        } else if self.matches(scanner::TokenType::Var) {
            self.var_decl()
        } else {
            self.statement()
        }
    }

    fn class_decl(&mut self) -> Result<(), String> {
        self.consume(scanner::TokenType::Identifier, "Expected class name.")?;
        let class_name_tok = self.previous().clone();
        let class_name = String::from_utf8(class_name_tok.clone().lexeme).unwrap();
        let name_constant = self.identifier_constant(class_name.clone());
        let line = self.previous().line;
        self.emit_op(bytecode::Op::Class(name_constant), line);
        self.define_variable(name_constant);

        let mut saved_class_compiler = None;

        std::mem::swap(&mut saved_class_compiler, &mut self.current_class);
        self.current_class = Some(ClassCompiler {
            has_superclass: false,
        });

        if self.matches(scanner::TokenType::Less) {
            self.consume(scanner::TokenType::Identifier, "Expected superclass name.")?;
            self.variable(false)?;

            if Compiler::identifiers_equal(&class_name_tok.literal, &self.previous().literal) {
                return Err(format!(
                    "A class cannot inherit from itself. Class name = {}, line,col={},{}.",
                    class_name,
                    self.previous().line,
                    self.previous().col
                ));
            }

            self.begin_scope();
            self.add_local(Compiler::synthetic_token("super"));
            self.define_variable(0);

            self.named_variable(class_name_tok.clone(), false)?;
            self.emit_op(bytecode::Op::Inherit, self.previous().line);

            if let Some(current_class) = &mut self.current_class {
                current_class.has_superclass = true;
            } else {
                panic!()
            }
        }

        self.named_variable(class_name_tok, false)?;

        self.consume(
            scanner::TokenType::LeftBrace,
            "Expected '{' before class body.",
        )?;
        loop {
            if self.check(scanner::TokenType::RightBrace) || self.check(scanner::TokenType::Eof) {
                break;
            }

            self.method()?;
        }
        self.consume(
            scanner::TokenType::RightBrace,
            "Expected '}' after class body.",
        )?;
        self.emit_op(bytecode::Op::Pop, self.previous().line);

        if let Some(current_class) = &self.current_class {
            if current_class.has_superclass {
                self.end_scope();
            }
        }

        std::mem::swap(&mut self.current_class, &mut saved_class_compiler);

        Ok(())
    }

    fn method(&mut self) -> Result<(), String> {
        self.consume(scanner::TokenType::Identifier, "Expected method name.")?;
        let method_name = if let Some(scanner::Literal::Identifier(method_name)) =
            &self.previous().literal.clone()
        {
            method_name.clone()
        } else {
            panic!(
                "expected identifier when parsing method, found {:?}",
                self.previous()
            );
        };

        let constant = self.identifier_constant(method_name.clone());

        let function_type = if method_name == "init" {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        };

        self.function(function_type)?;

        self.emit_op(bytecode::Op::Method(constant), self.previous().line);

        Ok(())
    }

    fn fun_decl(&mut self) -> Result<(), String> {
        let global_idx = self.parse_variable("Expected function name.")?;
        self.mark_initialized();
        self.function(FunctionType::Function)?;
        self.define_variable(global_idx);
        Ok(())
    }

    fn function(&mut self, function_type: FunctionType) -> Result<(), String> {
        let level = Level {
            function_type,
            function: bytecode::Function {
                name: if let Some(scanner::Literal::Identifier(funname)) = &self.previous().literal
                {
                    funname.clone()
                } else {
                    panic!("expected identifier");
                },
                ..Default::default()
            },
            ..Default::default()
        };
        self.push_level(level);

        if function_type != FunctionType::Function {
            let local = self.current_level_mut().locals.first_mut().unwrap();
            local.name.literal = Some(scanner::Literal::Identifier("this".to_string()));
        }

        self.begin_scope();
        self.consume(
            scanner::TokenType::LeftParen,
            "Expected '(' after function name.",
        )?;

        if !self.check(scanner::TokenType::RightParen) {
            loop {
                self.current_function_mut().arity += 1;
                let param_const_idx = self.parse_variable("Expected parameter name")?;
                self.define_variable(param_const_idx);

                if !self.matches(scanner::TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(
            scanner::TokenType::RightParen,
            "Expected ')' after parameter list.",
        )?;

        self.consume(
            scanner::TokenType::LeftBrace,
            "Expected '{' before function body.",
        )?;
        self.block()?;
        self.emit_return();

        let function = std::mem::take(&mut self.current_level_mut().function);
        let upvals = std::mem::take(&mut self.current_level_mut().upvals);
        self.pop_level();
        let const_idx = self
            .current_chunk()
            .add_constant(bytecode::Constant::Function(bytecode::Closure {
                function,
                upvalues: Vec::new(),
            }));
        self.emit_op(
            bytecode::Op::Closure(const_idx, upvals),
            self.previous().line,
        );

        Ok(())
    }

    fn var_decl(&mut self) -> Result<(), String> {
        let global_idx = self.parse_variable("Expected variable name.")?;

        if self.matches(scanner::TokenType::Equal) {
            self.expression()?;
        } else {
            let line = self.previous().line;
            self.emit_op(bytecode::Op::Nil, line)
        }

        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ';' after variable declaration",
        )?;

        self.define_variable(global_idx);
        Ok(())
    }

    fn mark_initialized(&mut self) -> bool {
        let scope_depth = self.scope_depth();
        if scope_depth > 0 {
            if let Some(last) = self.locals_mut().last_mut() {
                last.depth = scope_depth;
            } else {
                panic!("expected nonempty locals!");
            }
            true
        } else {
            false
        }
    }

    fn define_variable(&mut self, global_idx: usize) {
        if self.mark_initialized() {
            return;
        }
        let line = self.previous().line;
        self.emit_op(bytecode::Op::DefineGlobal(global_idx), line);
    }

    fn declare_variable(&mut self) -> Result<(), String> {
        //global variables are implicitly declared
        if self.scope_depth() == 0 {
            return Ok(());
        }

        let name = self.previous().clone();

        let has_redeclaration = self.locals().iter().rev().any(|local| {
            local.depth != -1
                && local.depth == self.scope_depth()
                && Compiler::identifiers_equal(&local.name.literal, &name.literal)
        });
        if has_redeclaration {
            return Err(format!(
                "Redeclaration of variable {} in the same scope.",
                String::from_utf8(name.lexeme).unwrap()
            ));
        }

        self.add_local(name);
        Ok(())
    }

    fn identifiers_equal(id1: &Option<scanner::Literal>, id2: &Option<scanner::Literal>) -> bool {
        match (id1, id2) {
            (
                Some(scanner::Literal::Identifier(name1)),
                Some(scanner::Literal::Identifier(name2)),
            ) => name1 == name2,
            _ => {
                panic!(
                    "expected identifier in `identifiers_equal` but found {:?} and {:?}.",
                    id1, id2
                );
            }
        }
    }

    fn identifier_equal(id1: &Option<scanner::Literal>, name2: &str) -> bool {
        match id1 {
            Some(scanner::Literal::Identifier(name1)) => name1 == name2,
            _ => {
                panic!(
                    "expected identifier in `identifier_equal` but found {:?}.",
                    id1
                );
            }
        }
    }

    fn synthetic_token(text: &str) -> scanner::Token {
        scanner::Token {
            ty: scanner::TokenType::Identifier,
            lexeme: text.as_bytes().to_vec(),
            literal: Some(scanner::Literal::Identifier(String::from(text))),
            line: 0,
            col: -1,
        }
    }

    fn add_local(&mut self, name: scanner::Token) {
        self.locals_mut().push(Local {
            name,
            depth: -1, // declare undefined
            is_captured: false,
        });
    }

    fn parse_variable(&mut self, error_msg: &str) -> Result<usize, String> {
        self.consume(scanner::TokenType::Identifier, error_msg)?;
        self.declare_variable()?;

        if self.scope_depth() > 0 {
            return Ok(0);
        }

        if let Some(scanner::Literal::Identifier(name)) = &self.previous().literal.clone() {
            Ok(self.identifier_constant(name.clone()))
        } else {
            panic!(
                "expected identifier when parsing variable, found {:?}",
                self.previous()
            );
        }
    }

    fn identifier_constant(&mut self, name: String) -> usize {
        self.current_chunk().add_constant_string(name)
    }

    fn statement(&mut self) -> Result<(), String> {
        if self.matches(scanner::TokenType::Print) {
            self.print_statement()?;
        } else if self.matches(scanner::TokenType::For) {
            self.for_statement()?;
        } else if self.matches(scanner::TokenType::If) {
            self.if_statement()?;
        } else if self.matches(scanner::TokenType::Return) {
            self.return_statement()?;
        } else if self.matches(scanner::TokenType::While) {
            self.while_statement()?;
        } else if self.matches(scanner::TokenType::LeftBrace) {
            self.begin_scope();
            self.block()?;
            self.end_scope();
        } else {
            self.expression_statement()?;
        }
        Ok(())
    }

    fn return_statement(&mut self) -> Result<(), String> {
        if self.function_type() == FunctionType::Script {
            return Err("Cannot return from top-level code.".to_string());
        }

        if self.matches(scanner::TokenType::Semicolon) {
            self.emit_return();
        } else {
            self.expression()?;
            self.consume(
                scanner::TokenType::Semicolon,
                "Expected ';' after return value.",
            )?;
            self.emit_op(bytecode::Op::Return, self.previous().line);
        }
        Ok(())
    }

    fn for_statement(&mut self) -> Result<(), String> {
        self.begin_scope();
        self.consume(scanner::TokenType::LeftParen, "Expected '(' after 'for'.")?;
        if self.matches(scanner::TokenType::Semicolon) {
        } else if self.matches(scanner::TokenType::Var) {
            self.var_decl()?;
        } else {
            self.expression_statement()?;
        }

        let mut loop_start = self.current_chunk().code.len();

        // condition
        let mut maybe_exit_jump = None;
        if !self.matches(scanner::TokenType::Semicolon) {
            self.expression()?;
            self.consume(
                scanner::TokenType::Semicolon,
                "Expected ';' after loop condition",
            )?;
            maybe_exit_jump = Some(self.emit_jump(bytecode::Op::JumpIfFalse(/*placeholder*/ 0)));
            self.emit_op(bytecode::Op::Pop, self.previous().line);
        }
        let maybe_exit_jump = maybe_exit_jump;

        // increment
        if !self.matches(scanner::TokenType::RightParen) {
            let body_jump = self.emit_jump(bytecode::Op::Jump(/*placeholder*/ 0));

            let increment_start = self.current_chunk().code.len() + 1;
            self.expression()?;
            self.emit_op(bytecode::Op::Pop, self.previous().line);
            self.consume(
                scanner::TokenType::RightParen,
                "Expected ')' after for clauses.",
            )?;

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement()?;

        self.emit_loop(loop_start);

        if let Some(exit_jump) = maybe_exit_jump {
            self.patch_jump(exit_jump);
            self.emit_op(bytecode::Op::Pop, self.previous().line);
        }

        self.end_scope();

        Ok(())
    }

    fn while_statement(&mut self) -> Result<(), String> {
        let loop_start = self.current_chunk().code.len();
        self.consume(scanner::TokenType::LeftParen, "Expected '(' after 'while'.")?;
        self.expression()?;
        self.consume(
            scanner::TokenType::RightParen,
            "Expected ')' after condition.",
        )?;

        let exit_jump = self.emit_jump(bytecode::Op::JumpIfFalse(/*placeholder*/ 0));

        self.emit_op(bytecode::Op::Pop, self.previous().line);
        self.statement()?;

        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_op(bytecode::Op::Pop, self.previous().line);
        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize) {
        let offset = self.current_chunk().code.len() - loop_start + 2;
        self.emit_op(bytecode::Op::Loop(offset), self.previous().line);
    }

    fn if_statement(&mut self) -> Result<(), String> {
        self.consume(scanner::TokenType::LeftParen, "Expected '(' after 'if'.")?;
        self.expression()?;
        self.consume(
            scanner::TokenType::RightParen,
            "Expected ')' after condition.",
        )?;

        let then_jump = self.emit_jump(bytecode::Op::JumpIfFalse(/*placeholder value*/ 0));
        self.emit_op(bytecode::Op::Pop, self.previous().line);
        self.statement()?;
        let else_jump = self.emit_jump(bytecode::Op::Jump(/*placeholder value*/ 0));

        self.patch_jump(then_jump);
        self.emit_op(bytecode::Op::Pop, self.previous().line);

        if self.matches(scanner::TokenType::Else) {
            self.statement()?;
        }
        self.patch_jump(else_jump);

        Ok(())
    }

    fn patch_jump(&mut self, jump_location: usize) {
        let true_jump = self.current_chunk().code.len() - jump_location - 1;
        let (maybe_jump, lineno) = &self.current_chunk().code[jump_location];
        if let bytecode::Op::JumpIfFalse(_) = maybe_jump {
            self.current_chunk().code[jump_location] =
                (bytecode::Op::JumpIfFalse(true_jump), *lineno);
        } else if let bytecode::Op::Jump(_) = maybe_jump {
            self.current_chunk().code[jump_location] = (bytecode::Op::Jump(true_jump), *lineno);
        } else {
            panic!(
                "attempted to patch a jump but didn't find a jump! Found {:?}.",
                maybe_jump
            );
        }
    }

    fn emit_jump(&mut self, op: bytecode::Op) -> usize {
        self.emit_op(op, self.previous().line);
        self.current_chunk().code.len() - 1
    }

    fn block(&mut self) -> Result<(), String> {
        while !self.check(scanner::TokenType::RightBrace) && !self.check(scanner::TokenType::Eof) {
            self.declaration()?;
        }

        if let Err(err) = self.consume(scanner::TokenType::RightBrace, "Expected '}' after block") {
            Err(err)
        } else {
            Ok(())
        }
    }

    fn begin_scope(&mut self) {
        self.current_level_mut().scope_depth += 1
    }

    fn end_scope(&mut self) {
        self.current_level_mut().scope_depth -= 1;

        let mut pop_count = 0;
        for local in self.locals().iter().rev() {
            if local.depth > self.scope_depth() {
                pop_count += 1;
            } else {
                break;
            }
        }
        let pop_count = pop_count;

        let line = self.previous().line;

        for _ in 0..pop_count {
            let local = self.locals_mut().pop().unwrap();

            if local.is_captured {
                self.emit_op(bytecode::Op::CloseUpvalue, line);
            } else {
                self.emit_op(bytecode::Op::Pop, line);
            }
        }
    }

    fn expression_statement(&mut self) -> Result<(), String> {
        self.expression()?;
        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ';' after expression.",
        )?;
        let line = self.previous().line;
        self.emit_op(bytecode::Op::Pop, line);
        Ok(())
    }

    fn print_statement(&mut self) -> Result<(), String> {
        self.expression()?;
        self.consume(scanner::TokenType::Semicolon, "Expected ';' after value.")?;
        self.emit_op(bytecode::Op::Print, self.previous().clone().line);
        Ok(())
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

    fn expression(&mut self) -> Result<(), String> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn grouping(&mut self, _can_assign: bool) -> Result<(), String> {
        self.expression()?;

        if let Err(err) = self.consume(
            scanner::TokenType::RightParen,
            "Expected ')' after expression.",
        ) {
            Err(err)
        } else {
            Ok(())
        }
    }

    fn number(&mut self, _can_assign: bool) -> Result<(), String> {
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

    fn literal(&mut self, _can_assign: bool) -> Result<(), String> {
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

    fn variable(&mut self, can_assign: bool) -> Result<(), String> {
        let tok = self.previous().clone();
        self.named_variable(tok, can_assign)
    }

    fn named_variable(&mut self, tok: scanner::Token, can_assign: bool) -> Result<(), String> {
        let name = match tok.ty {
            scanner::TokenType::Identifier => {
                if let Some(scanner::Literal::Identifier(n)) = tok.literal.clone() {
                    Some(n)
                } else {
                    None
                }
            }
            scanner::TokenType::This => Some("this".to_string()),
            _ => None,
        }
        .unwrap();

        let get_op: bytecode::Op;
        let set_op: bytecode::Op;

        match self.resolve_variable(&name) {
            Ok(Resolution::Local(idx)) => {
                get_op = bytecode::Op::GetLocal(idx);
                set_op = bytecode::Op::SetLocal(idx);
            }
            Ok(Resolution::Global) => {
                let idx = self.identifier_constant(name);
                get_op = bytecode::Op::GetGlobal(idx);
                set_op = bytecode::Op::SetGlobal(idx);
            }
            Ok(Resolution::Upvalue(idx)) => {
                get_op = bytecode::Op::GetUpval(idx);
                set_op = bytecode::Op::SetUpval(idx);
            }
            Err(err) => {
                return Err(err);
            }
        }

        if can_assign && self.matches(scanner::TokenType::Equal) {
            self.expression()?;
            self.emit_op(set_op, tok.line);
        } else {
            self.emit_op(get_op, tok.line);
        }
        Ok(())
    }

    fn resolve_variable(&mut self, name: &str) -> Result<Resolution, String> {
        if let Some(idx) = self.resolve_local(name)? {
            return Ok(Resolution::Local(idx));
        }
        if let Some(idx) = self.resolve_upval(name)? {
            return Ok(Resolution::Upvalue(idx));
        }

        Ok(Resolution::Global)
    }

    fn resolve_upval(&mut self, name: &str) -> Result<Option<usize>, String> {
        if self.level_idx < 1 {
            return Ok(None);
        }

        let prev_level_idx = self.level_idx - 1;

        if let Some(local_idx) =
            Compiler::resolve_local_static(&self.levels[prev_level_idx], name, self.previous())?
        {
            self.levels[prev_level_idx].locals[local_idx].is_captured = true;

            return Ok(Some(self.add_upval(bytecode::UpvalueLoc::Local(local_idx))));
        }

        self.level_idx -= 1;

        if let Some(upval_idx) = self.resolve_upval(name)? {
            self.level_idx += 1; // couldn't figure out how to satisfy borrow checker with scopeguard!
            return Ok(Some(
                self.add_upval(bytecode::UpvalueLoc::Upvalue(upval_idx)),
            ));
        }
        self.level_idx += 1;

        Ok(None)
    }

    fn add_upval(&mut self, upvalue: bytecode::UpvalueLoc) -> usize {
        if let Some(res) = self
            .current_level()
            .upvals
            .iter()
            .position(|query_upval| *query_upval == upvalue)
        {
            return res;
        }

        self.current_level_mut().upvals.push(upvalue);
        self.current_level().upvals.len() - 1
    }

    fn resolve_local(&self, name: &str) -> Result<Option<usize>, String> {
        Compiler::resolve_local_static(self.current_level(), name, self.previous())
    }

    fn resolve_local_static(
        level: &Level,
        name: &str,
        prev_tok: &scanner::Token,
    ) -> Result<Option<usize>, String> {
        for (idx, local) in level.locals.iter().rev().enumerate() {
            if Compiler::identifier_equal(&local.name.literal, name) {
                if local.depth == -1 {
                    return Err(Compiler::error_at_tok(
                        "Cannot read local variable in its own initializer.",
                        prev_tok,
                    ));
                }
                return Ok(Some(level.locals.len() - 1 - idx));
            }
        }
        Ok(None)
    }

    fn string(&mut self, _can_assign: bool) -> Result<(), String> {
        let tok = self.previous().clone();

        match tok.literal {
            Some(scanner::Literal::Str(s)) => {
                let const_idx = self.current_chunk().add_constant_string(s);
                self.emit_op(bytecode::Op::Constant(const_idx), tok.line);
                Ok(())
            }
            _ => panic!("expected literal when parsing string"),
        }
    }

    fn binary(&mut self, _can_assign: bool) -> Result<(), String> {
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
            scanner::TokenType::BangEqual => {
                self.emit_op(bytecode::Op::Equal, operator.line);
                self.emit_op(bytecode::Op::Not, operator.line);
                Ok(())
            }
            scanner::TokenType::EqualEqual => {
                self.emit_op(bytecode::Op::Equal, operator.line);
                Ok(())
            }
            scanner::TokenType::Greater => {
                self.emit_op(bytecode::Op::Greater, operator.line);
                Ok(())
            }
            scanner::TokenType::GreaterEqual => {
                self.emit_op(bytecode::Op::Less, operator.line);
                self.emit_op(bytecode::Op::Not, operator.line);
                Ok(())
            }
            scanner::TokenType::Less => {
                self.emit_op(bytecode::Op::Less, operator.line);
                Ok(())
            }
            scanner::TokenType::LessEqual => {
                self.emit_op(bytecode::Op::Greater, operator.line);
                self.emit_op(bytecode::Op::Not, operator.line);
                Ok(())
            }
            _ => Err(format!(
                "Invalid token {:?} in binary expression at line={},col={}.",
                operator.ty, operator.line, operator.col
            )),
        }
    }

    fn and(&mut self, _can_assign: bool) -> Result<(), String> {
        let end_jump = self.emit_jump(bytecode::Op::JumpIfFalse(/*placeholder*/ 0));
        self.emit_op(bytecode::Op::Pop, self.previous().line);
        self.parse_precedence(Precedence::And)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn or(&mut self, _can_assign: bool) -> Result<(), String> {
        let else_jump = self.emit_jump(bytecode::Op::JumpIfFalse(/*placeholder*/ 0));
        let end_jump = self.emit_jump(bytecode::Op::Jump(/*placeholder*/ 0));

        self.patch_jump(else_jump);
        self.emit_op(bytecode::Op::Pop, self.previous().line);

        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn call(&mut self, _can_assign: bool) -> Result<(), String> {
        let arg_count = self.argument_list()?;
        self.emit_op(bytecode::Op::Call(arg_count), self.previous().line);
        Ok(())
    }

    fn super_(&mut self, _can_assign: bool) -> Result<(), String> {
        let tok = self.previous().clone();
        match &self.current_class {
            None => {
                return Err(format!(
                    "Can't use 'super' outside of a class (line,col={},{})",
                    tok.line, tok.col
                ))
            }
            Some(class) => {
                if !class.has_superclass {
                    return Err(format!(
                        "Can't use 'super' in a class with no superclass (line,col={},{})",
                        tok.line, tok.col
                    ));
                }
            }
        }
        self.consume(
            scanner::TokenType::Dot,
            "Expected '.' after 'super' keyword.",
        )?;
        self.consume(
            scanner::TokenType::Identifier,
            "Expected superclass method name.",
        )?;

        let method_name = if let Some(scanner::Literal::Identifier(method_name)) =
            &self.previous().literal.clone()
        {
            method_name.clone()
        } else {
            panic!()
        };

        self.named_variable(Compiler::synthetic_token("this"), false)?;

        let op = if self.matches(scanner::TokenType::LeftParen) {
            let arg_count = self.argument_list()?;
            self.named_variable(Compiler::synthetic_token("super"), false)?;
            bytecode::Op::SuperInvoke(method_name, arg_count)
        } else {
            self.named_variable(Compiler::synthetic_token("super"), false)?;
            bytecode::Op::GetSuper(self.identifier_constant(method_name))
        };
        self.emit_op(op, self.previous().line);
        Ok(())
    }

    fn this(&mut self, _can_assign: bool) -> Result<(), String> {
        let tok = self.previous().clone();
        if self.current_class.is_none() {
            return Err(format!(
                "Cannot use 'this' outside of class at line={}, col={}",
                tok.line, tok.col
            ));
        }

        self.variable(false)
    }

    fn dot(&mut self, can_assign: bool) -> Result<(), String> {
        self.consume(
            scanner::TokenType::Identifier,
            "Expected property name after '.'.",
        )?;
        let property_name = String::from_utf8(self.previous().clone().lexeme).unwrap();
        let property_constant = self.identifier_constant(property_name.clone());
        let op = if can_assign && self.matches(scanner::TokenType::Equal) {
            self.expression()?;
            bytecode::Op::SetProperty(property_constant)
        } else if self.matches(scanner::TokenType::LeftParen) {
            let arg_count = self.argument_list()?;
            bytecode::Op::Invoke(property_name, arg_count)
        } else {
            bytecode::Op::GetProperty(property_constant)
        };
        self.emit_op(op, self.previous().line);
        Ok(())
    }

    fn subscr(&mut self, _can_assign: bool) -> Result<(), String> {
        if !self.extensions.lists {
            return Err(format!("Unexpected '[' at line {}", self.previous().line));
        }

        self.expression()?;
        self.consume(
            scanner::TokenType::RightBracket,
            "Expected ] after subscript",
        )?;
        self.emit_op(bytecode::Op::Subscr, self.previous().line);
        Ok(())
    }

    fn list(&mut self, _can_assign: bool) -> Result<(), String> {
        if !self.extensions.lists {
            return Err(format!("Unexpected '[' at line {}", self.previous().line));
        }

        let arg_count = self.list_elements()?;
        self.emit_op(bytecode::Op::BuildList(arg_count), self.previous().line);
        Ok(())
    }

    fn list_elements(&mut self) -> Result<usize, String> {
        let mut num_elements: usize = 0;
        if !self.check(scanner::TokenType::RightBracket) {
            loop {
                self.expression()?;
                num_elements += 1;
                if !self.matches(scanner::TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(scanner::TokenType::RightBracket, "Expected ']'.")?;
        Ok(num_elements)
    }

    fn argument_list(&mut self) -> Result<u8, String> {
        let mut arg_count: u8 = 0;
        if !self.check(scanner::TokenType::RightParen) {
            loop {
                self.expression()?;
                arg_count += 1;
                if !self.matches(scanner::TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(
            scanner::TokenType::RightParen,
            "Expected ')' after argument list.",
        )?;
        Ok(arg_count)
    }

    fn unary(&mut self, _can_assign: bool) -> Result<(), String> {
        let operator = self.previous().clone();

        self.parse_precedence(Precedence::Unary)?;

        match operator.ty {
            scanner::TokenType::Minus => {
                self.emit_op(bytecode::Op::Negate, operator.line);
                Ok(())
            }
            scanner::TokenType::Bang => {
                self.emit_op(bytecode::Op::Not, operator.line);
                Ok(())
            }
            _ => Err(format!(
                "Invalid token in unary op {:?} at line={},col={}",
                operator.ty, operator.line, operator.col
            )),
        }
    }

    fn emit_number(&mut self, n: f64, lineno: usize) {
        let const_idx = self.current_chunk().add_constant_number(n);
        self.emit_op(bytecode::Op::Constant(const_idx), lineno);
    }

    fn emit_op(&mut self, op: bytecode::Op, lineno: usize) {
        self.current_chunk()
            .code
            .push((op, bytecode::Lineno(lineno)))
    }

    fn emit_return(&mut self) {
        let op = match self.current_level().function_type {
            FunctionType::Initializer => bytecode::Op::GetLocal(0),
            _ => bytecode::Op::Nil,
        };

        self.emit_op(op, self.previous().line);
        self.emit_op(bytecode::Op::Return, self.previous().line);
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), String> {
        self.advance();

        let can_assign = precedence <= Precedence::Assignment;

        match Compiler::get_rule(self.previous().ty).prefix {
            Some(parse_fn) => self.apply_parse_fn(parse_fn, can_assign)?,
            None => {
                return Err(self.error("Expected expression."));
            }
        }

        while precedence <= Compiler::get_rule(self.peek().ty).precedence {
            self.advance();
            match Compiler::get_rule(self.previous().ty).infix {
                Some(parse_fn) => self.apply_parse_fn(parse_fn, can_assign)?,
                None => panic!("could not find infix rule to apply tok = {:?}", self.peek()),
            }
        }

        if can_assign && self.matches(scanner::TokenType::Equal) {
            if let Some((bytecode::Op::Subscr, _)) = self.current_chunk().code.last() {
                self.fixup_subscript_to_setitem()?;
            } else {
                return Err(self.error("Invalid assignment target"));
            }
        }

        Ok(())
    }

    fn fixup_subscript_to_setitem(&mut self) -> Result<(), String> {
        self.current_chunk().code.pop(); // pop the subscript op
        self.expression()?; // consume right hand side
        self.emit_op(bytecode::Op::SetItem, self.previous().line);
        Ok(())
    }

    fn error(&self, what: &str) -> String {
        Compiler::error_at_tok(what, self.previous())
    }

    fn error_at_tok(what: &str, prev_tok: &scanner::Token) -> String {
        format!("{} at line={},col={}.", what, prev_tok.line, prev_tok.col)
    }

    fn apply_parse_fn(&mut self, parse_fn: ParseFn, can_assign: bool) -> Result<(), String> {
        match parse_fn {
            ParseFn::Grouping => self.grouping(can_assign),
            ParseFn::Unary => self.unary(can_assign),
            ParseFn::Binary => self.binary(can_assign),
            ParseFn::Number => self.number(can_assign),
            ParseFn::Literal => self.literal(can_assign),
            ParseFn::String => self.string(can_assign),
            ParseFn::Variable => self.variable(can_assign),
            ParseFn::And => self.and(can_assign),
            ParseFn::Or => self.or(can_assign),
            ParseFn::Call => self.call(can_assign),
            ParseFn::Dot => self.dot(can_assign),
            ParseFn::This => self.this(can_assign),
            ParseFn::Super => self.super_(can_assign),
            ParseFn::List => self.list(can_assign),
            ParseFn::Subscript => self.subscr(can_assign),
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

    fn advance(&mut self) -> &scanner::Token {
        if !self.is_at_end() {
            self.token_idx += 1
        }

        self.previous()
    }

    fn previous(&self) -> &scanner::Token {
        &self.tokens[self.token_idx - 1]
    }

    fn is_at_end(&self) -> bool {
        self.peek().ty == scanner::TokenType::Eof
    }

    fn peek(&self) -> &scanner::Token {
        &self.tokens[self.token_idx]
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
                infix: Some(ParseFn::Call),
                precedence: Precedence::Call,
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
            scanner::TokenType::LeftBracket => ParseRule {
                prefix: Some(ParseFn::List),
                infix: Some(ParseFn::Subscript),
                precedence: Precedence::Call,
            },
            scanner::TokenType::RightBracket => ParseRule {
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
                infix: Some(ParseFn::Dot),
                precedence: Precedence::Call,
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
                precedence: Precedence::None,
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
                prefix: Some(ParseFn::Unary),
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::BangEqual => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Equality,
            },
            scanner::TokenType::Equal => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::EqualEqual => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Equality,
            },
            scanner::TokenType::Greater => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Comparison,
            },
            scanner::TokenType::GreaterEqual => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Comparison,
            },
            scanner::TokenType::Less => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Comparison,
            },
            scanner::TokenType::LessEqual => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Comparison,
            },
            scanner::TokenType::Identifier => ParseRule {
                prefix: Some(ParseFn::Variable),
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::String => ParseRule {
                prefix: Some(ParseFn::String),
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
                infix: Some(ParseFn::And),
                precedence: Precedence::And,
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
                infix: Some(ParseFn::Or),
                precedence: Precedence::Or,
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
                prefix: Some(ParseFn::Super),
                infix: None,
                precedence: Precedence::None,
            },
            scanner::TokenType::This => ParseRule {
                prefix: Some(ParseFn::This),
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
            scanner::TokenType::Lambda => unimplemented!(),
            scanner::TokenType::Eof => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }

    fn current_chunk(&mut self) -> &mut bytecode::Chunk {
        &mut self.current_level_mut().function.chunk
    }

    fn current_level(&self) -> &Level {
        &self.levels[self.level_idx]
    }

    fn current_level_mut(&mut self) -> &mut Level {
        &mut self.levels[self.level_idx]
    }

    fn push_level(&mut self, level: Level) {
        self.levels.push(level);
        self.level_idx += 1;
    }

    fn pop_level(&mut self) {
        self.levels.pop();
        self.level_idx -= 1;
    }

    fn current_function_mut(&mut self) -> &mut bytecode::Function {
        &mut self.current_level_mut().function
    }

    fn function_type(&self) -> FunctionType {
        self.current_level().function_type
    }

    fn scope_depth(&self) -> i64 {
        self.current_level().scope_depth
    }

    fn locals(&self) -> &Vec<Local> {
        &self.current_level().locals
    }

    fn locals_mut(&mut self) -> &mut Vec<Local> {
        &mut self.current_level_mut().locals
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::*;

    fn check_error(code: &str, f: &dyn Fn(&str) -> ()) {
        let func_or_err =
            Compiler::compile(String::from(code), syntax_extensions::Extensions::default());

        match func_or_err {
            Ok(_) => panic!(),
            Err(err) => f(&err),
        }
    }

    #[test]
    fn test_compiles_1() {
        Compiler::compile(
            String::from("print 42 * 12;"),
            syntax_extensions::Extensions::default(),
        )
        .unwrap();
    }

    #[test]
    fn test_compiles_2() {
        Compiler::compile(
            String::from("print -2 * 3 + (-4 / 2);"),
            syntax_extensions::Extensions::default(),
        )
        .unwrap();
    }

    #[test]
    fn test_var_decl_compiles_1() {
        Compiler::compile(
            String::from("var x = 2;"),
            syntax_extensions::Extensions::default(),
        )
        .unwrap();
    }

    #[test]
    fn test_var_decl_implicit_nil() {
        Compiler::compile(
            String::from("var x;"),
            syntax_extensions::Extensions::default(),
        )
        .unwrap();
    }

    #[test]
    fn test_var_reading_2() {
        Compiler::compile(
            String::from("var x; print x;"),
            syntax_extensions::Extensions::default(),
        )
        .unwrap();
    }

    #[test]
    fn test_var_reading_3() {
        Compiler::compile(
            String::from("var x; print x * 2 + x;"),
            syntax_extensions::Extensions::default(),
        )
        .unwrap();
    }

    #[test]
    fn test_this_outside_method_1() {
        check_error("print this;", &|err: &str| {
            assert!(err.starts_with("Cannot use 'this' outside of class"))
        })
    }

    #[test]
    fn test_this_outside_method_2() {
        check_error("fun foo() {print this;}", &|err: &str| {
            assert!(err.starts_with("Cannot use 'this' outside of class"))
        })
    }

    #[test]
    fn test_self_ineritance_is_error() {
        check_error("class A < A {}", &|err: &str| {
            assert!(err.starts_with("A class cannot inherit from itself."))
        })
    }

    #[test]
    fn test_cant_use_super_outside_class() {
        check_error("fun f() { super.bar(); }", &|err: &str| {
            assert!(err.starts_with("Can't use 'super' outside of a class"))
        })
    }

    #[test]
    fn test_cant_use_super_in_class_with_no_superclass() {
        check_error("class Foo { bar() { super.bar(); } }", &|err: &str| {
            assert!(err.starts_with("Can't use 'super' in a class with no superclass"))
        })
    }

    #[test]
    fn test_setitem_illegal_target_globals() {
        let func_or_err = Compiler::compile(
            String::from(
                "var x = 2;\n\
             var y = 3;\n\
             x * y = 5;",
            ),
            syntax_extensions::Extensions::default(),
        );

        match func_or_err {
            Ok(_) => panic!("expected compile error"),
            Err(err) => assert!(err.starts_with("Invalid assignment target")),
        }
    }

    #[test]
    fn test_setitem_illegal_target_locals() {
        let func_or_err = Compiler::compile(
            String::from(
                "{\n\
               var x = 2;\n\
               var y = 3;\n\
               x * y = 5;\n\
             }\n",
            ),
            syntax_extensions::Extensions::default(),
        );

        match func_or_err {
            Ok(_) => panic!("expected compile error"),
            Err(err) => assert!(err.starts_with("Invalid assignment target")),
        }
    }

    #[test]
    fn test_redeclaration_of_locals_is_error() {
        let func_or_err = Compiler::compile(
            String::from(
                "{\n\
               var x = 2;\n\
               var x = 3;\n\
             }",
            ),
            syntax_extensions::Extensions::default(),
        );

        match func_or_err {
            Ok(_) => panic!("expected compile error"),
            Err(err) => assert!(err.starts_with("Redeclaration of variable")),
        }
    }
}
