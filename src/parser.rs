use core::panic;

use crate::expr::ExprEnum;
use crate::stmt::{Function, StmtEnum};
use crate::token::Token;
use crate::token_type::TokenType;
use crate::value::Value;
use crate::{expr, stmt, Lox};

pub struct Parser {
    pub tokens: Vec<Token>,
    pub curr: usize,
    pub var_counter: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            curr: 0,
            var_counter: 0,
        }
    }

    pub fn parse(&mut self) -> Vec<Box<StmtEnum>> {
        let mut statements: Vec<Box<StmtEnum>> = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration());
        }
        statements
    }

    pub fn expression(&mut self) -> ExprEnum {
        self.assignment()
    }

    pub fn declaration(&mut self) -> Box<StmtEnum> {
        //try {
        let token: &Token = self.peek();
        match token.token_type {
            TokenType::Class => {
                self.advance();
                self.class_declaration()
            }
            TokenType::Fun => {
                self.advance();
                self.function("function")
            }
            TokenType::Var => {
                self.advance();
                self.var_declaration()
            }
            _ => self.statement(),
        }
        //} catch (ParseError error) {
        //    synchronize();
        //    return null;
        //}
    }

    pub fn class_declaration(&mut self) -> Box<StmtEnum> {
        let name = self
            .consume(&TokenType::Identifier("".into()), "Expect class name.")
            .clone();

        let superclass = match &self.peek().token_type {
            &TokenType::Less => {
                self.advance();
                let token =
                    self.consume(&TokenType::Identifier("".into()), "Expect superclass name.");
                Some(expr::Var {
                    name: token.clone(),
                    id: self.var_count(),
                })
            }
            _ => None,
        };

        self.consume(&TokenType::LeftBrace, "Expect '{' before class body.");

        let mut methods: Vec<Box<Function>> = Vec::new();

        loop {
            if self.check(&TokenType::RightBrace) || self.is_at_end() {
                break;
            } else {
                let function = match *self.function("method") {
                    StmtEnum::Function(x) => x,
                    _ => panic!("Function should return functions, check the code."),
                };
                methods.push(function)
            }
        }

        self.consume(&TokenType::RightBrace, "Expect '}' after class body.");

        Box::new(StmtEnum::Class(Box::new(stmt::Class {
            name,
            superclass,
            methods,
        })))
    }

    pub fn statement(&mut self) -> Box<StmtEnum> {
        let token: &Token = self.peek();
        match token.token_type {
            TokenType::For => {
                self.advance();
                self.for_statement()
            }
            TokenType::If => {
                self.advance();
                self.if_statement()
            }
            TokenType::Print => {
                self.advance();
                self.print_statement()
            }
            TokenType::Return => self.return_statement(),
            TokenType::While => {
                self.advance();
                self.while_statement()
            }
            TokenType::LeftBrace => {
                self.advance();
                Box::new(StmtEnum::Block(Box::new(stmt::Block {
                    statements: self.block(),
                })))
            }
            _ => self.expression_statement(),
        }
    }

    pub fn for_statement(&mut self) -> Box<StmtEnum> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'for'.");
        let token = self.peek();
        let initializer = match token.token_type {
            TokenType::SemiColon => {
                self.advance();
                None
            }
            TokenType::Var => {
                self.advance();
                Some(self.var_declaration())
            }
            _ => Some(self.expression_statement()),
        };

        let condition: ExprEnum = if self.check(&TokenType::SemiColon) {
            ExprEnum::Literal(Box::new(expr::Literal {
                value: Value::Bool(true),
            }))
        } else {
            self.expression()
        };

        self.consume(&TokenType::SemiColon, "Expect ';' after loop condition.");

        let mut increment = None;
        if !self.check(&TokenType::RightParen) {
            increment = Some(self.expression())
        }

        self.consume(&TokenType::RightParen, "Expect ')' after for clauses.");

        let mut body = self.statement();

        if let Some(x) = increment {
            body = Box::new(StmtEnum::Block(Box::new(stmt::Block {
                statements: vec![
                    body,
                    Box::new(StmtEnum::Expression(Box::new(stmt::Expression {
                        expression: x,
                    }))),
                ],
            })))
        }

        body = Box::new(StmtEnum::While(Box::new(stmt::While {
            condition,
            body: *body,
        })));

        if let Some(x) = initializer {
            body = Box::new(StmtEnum::Block(Box::new(stmt::Block {
                statements: vec![x, body],
            })))
        }

        body
    }

    pub fn if_statement(&mut self) -> Box<StmtEnum> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'if'.");
        let condition: ExprEnum = self.expression();
        self.consume(&TokenType::RightParen, "Expect ')' after 'if' condition.");
        let then_branch = *self.statement();
        let token = self.peek();
        let else_branch = match token.token_type {
            TokenType::Else => {
                self.advance();
                Some(*self.statement())
            }
            _ => None,
        };

        Box::new(StmtEnum::If(Box::new(stmt::If {
            condition,
            then_branch,
            else_branch,
        })))
    }

    pub fn print_statement(&mut self) -> Box<StmtEnum> {
        let value: ExprEnum = self.expression();
        self.consume(&TokenType::SemiColon, "Expect ';' after value.");
        Box::new(StmtEnum::Print(Box::new(stmt::Print { expression: value })))
    }

    pub fn return_statement(&mut self) -> Box<StmtEnum> {
        let keyword: Token = self.peek().clone();
        self.advance();
        let mut value = None;
        if !self.check(&TokenType::SemiColon) {
            value = Some(self.expression());
        }
        self.consume(&TokenType::SemiColon, "Expect ';' after return value.");
        Box::new(StmtEnum::Return(Box::new(stmt::Return { keyword, value })))
    }

    pub fn while_statement(&mut self) -> Box<StmtEnum> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'while'.");
        let condition = self.expression();
        self.consume(&TokenType::RightParen, "Expect ')' after condition.");
        let body = *self.statement();
        Box::new(StmtEnum::While(Box::new(stmt::While { condition, body })))
    }

    pub fn var_declaration(&mut self) -> Box<StmtEnum> {
        let name: Token = self
            .consume(&TokenType::Identifier("".into()), "Expect variable name.")
            .clone();

        let token: &Token = self.peek();

        let initializer: ExprEnum = match token.token_type {
            TokenType::Equal => {
                self.advance();
                self.expression()
            }
            _ => ExprEnum::Literal(Box::new(expr::Literal { value: Value::Nil })),
        };

        self.consume(
            &TokenType::SemiColon,
            "Expect ';' after variable declaration.",
        );

        Box::new(StmtEnum::Var(Box::new(stmt::Var { name, initializer })))
    }

    pub fn expression_statement(&mut self) -> Box<StmtEnum> {
        let value: ExprEnum = self.expression();
        self.consume(&TokenType::SemiColon, "Expect ';' after value.");
        Box::new(StmtEnum::Expression(Box::new(stmt::Expression {
            expression: value,
        })))
    }

    pub fn function(&mut self, kind: &str) -> Box<StmtEnum> {
        let name: Token = self
            .consume(
                &TokenType::Identifier("".into()),
                &format!("Expect {} name.", kind),
            )
            .clone();
        self.consume(
            &TokenType::LeftParen,
            &format!("Expect '(' after {} name.", kind),
        );
        let mut params: Vec<Token> = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    let _ = &self.error(self.peek(), "Can't have more than 255 parameters.");
                }
                params.push(
                    self.consume(&TokenType::Identifier("".into()), "Expect parameter name.")
                        .clone(),
                );
                if !self.check(&TokenType::Comma) {
                    break;
                }
                self.advance();
            }
        }
        self.consume(&TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(
            &TokenType::LeftBrace,
            &format!("Expect '{{' before {} body.", kind),
        );
        Box::new(StmtEnum::Function(Box::new(stmt::Function {
            name,
            params,
            body: self.block(),
        })))
    }

    pub fn block(&mut self) -> Vec<Box<StmtEnum>> {
        let mut statements: Vec<Box<StmtEnum>> = Vec::new();

        loop {
            let token = self.peek();
            match token.token_type {
                TokenType::RightBrace => break,
                TokenType::Eof => break,
                _ => statements.push(self.declaration()),
            }
        }

        self.consume(&TokenType::RightBrace, "Expect '}' after block.");
        statements
    }

    pub fn assignment(&mut self) -> ExprEnum {
        let expr: ExprEnum = self.or();

        let token: &Token = self.peek();
        match token.token_type {
            TokenType::Equal => {
                let equals: Token = token.clone();
                self.advance();
                let value: ExprEnum = self.assignment();
                match expr {
                    ExprEnum::Var(x) => {
                        let name: Token = x.name;
                        ExprEnum::Assign(Box::new(expr::Assign {
                            name,
                            value,
                            id: self.var_count(),
                        }))
                    }
                    ExprEnum::Get(x) => {
                        let get = x;
                        ExprEnum::Set(Box::new(expr::Set {
                            object: get.object,
                            name: get.name,
                            value,
                        }))
                    }
                    _ => panic!("{} {}", equals, "Invalid assignment target."),
                }
            }
            _ => expr,
        }
    }

    pub fn or(&mut self) -> ExprEnum {
        let mut expr = self.and();

        while let TokenType::Or = self.peek().token_type {
            let operator: Token = self.peek().clone();
            self.advance();
            let right: ExprEnum = self.and();
            expr = ExprEnum::Logic(Box::new(expr::Logic {
                left: expr,
                operator,
                right,
            }))
        }

        expr
    }

    pub fn and(&mut self) -> ExprEnum {
        let mut expr: ExprEnum = self.equality();

        while let TokenType::And = self.peek().token_type {
            let operator: Token = self.peek().clone();
            self.advance();
            let right: ExprEnum = self.equality();
            expr = ExprEnum::Logic(Box::new(expr::Logic {
                left: expr,
                operator,
                right,
            }))
        }

        expr
    }

    pub fn equality(&mut self) -> ExprEnum {
        let mut expr: ExprEnum = self.comparison();
        loop {
            let token: &Token = self.peek();
            match &token.token_type {
                TokenType::BangEqual | TokenType::EqualEqual => {
                    let operator: Token = token.clone();
                    self.advance();
                    let right: ExprEnum = self.comparison();
                    expr = ExprEnum::Binary(Box::new(expr::Binary {
                        left: expr,
                        operator,
                        right,
                    }));
                }
                _ => break,
            }
        }
        expr
    }

    pub fn comparison(&mut self) -> ExprEnum {
        let mut expr: ExprEnum = self.term();
        loop {
            let token: &Token = self.peek();
            match &token.token_type {
                &TokenType::Greater
                | &TokenType::GreaterEqual
                | &TokenType::Less
                | &TokenType::LessEqual => {
                    let operator: Token = token.clone();
                    self.advance();
                    let right: ExprEnum = self.term();
                    expr = ExprEnum::Binary(Box::new(expr::Binary {
                        left: expr,
                        operator,
                        right,
                    }));
                }
                _ => break,
            }
        }
        expr
    }

    pub fn term(&mut self) -> ExprEnum {
        let mut expr: ExprEnum = self.factor();
        loop {
            let token: &Token = self.peek();
            match &token.token_type {
                &TokenType::Minus | &TokenType::Plus => {
                    let operator: Token = token.clone();
                    self.advance();
                    let right: ExprEnum = self.factor();
                    expr = ExprEnum::Binary(Box::new(expr::Binary {
                        left: expr,
                        operator,
                        right,
                    }));
                }
                _ => break,
            }
        }
        expr
    }

    pub fn factor(&mut self) -> ExprEnum {
        let mut expr: ExprEnum = self.unary();
        loop {
            let token: &Token = self.peek();
            match &token.token_type {
                TokenType::Slash | TokenType::Star => {
                    let operator: Token = token.clone();
                    self.advance();
                    let right: ExprEnum = self.unary();
                    expr = ExprEnum::Binary(Box::new(expr::Binary {
                        left: expr,
                        operator,
                        right,
                    }));
                }
                _ => break,
            }
        }
        expr
    }

    pub fn unary(&mut self) -> ExprEnum {
        let token: &Token = self.peek();
        match &token.token_type {
            TokenType::Bang | TokenType::Minus => {
                let operator: Token = token.clone();
                self.advance();
                let right: ExprEnum = self.unary();
                ExprEnum::Unary(Box::new(expr::Unary { operator, right }))
            }
            _ => self.call(),
        }
    }

    pub fn call(&mut self) -> ExprEnum {
        let mut expr: ExprEnum = self.primary();

        loop {
            let token: &Token = self.peek();
            match token.token_type {
                TokenType::LeftParen => {
                    self.advance();
                    expr = self.finish_call(expr)
                }
                TokenType::Dot => {
                    self.advance();
                    let name = self
                        .consume(
                            &TokenType::Identifier("".into()),
                            "Expect property name after '.'.",
                        )
                        .clone();
                    expr = ExprEnum::Get(Box::new(expr::Get { object: expr, name }));
                }
                _ => break,
            }
        }

        expr
    }

    pub fn finish_call(&mut self, callee: ExprEnum) -> ExprEnum {
        let mut arguments: Vec<ExprEnum> = Vec::new();

        loop {
            if arguments.len() >= 255 {
                self.error(self.peek(), "Can't have more than 255 arguments.")
            }

            if self.peek().token_type == TokenType::RightParen {
                break;
            }

            arguments.push(self.expression());

            match self.peek().token_type {
                TokenType::Comma => {
                    self.advance();
                }
                TokenType::RightParen => break,
                _ => panic!("Expected ',' or ')' after argument"),
            }
        }

        let paren: Token = self
            .consume(&TokenType::RightParen, "Expect ')' after arguments.")
            .clone();
        ExprEnum::Call(Box::new(expr::Call {
            callee,
            paren,
            arguments,
        }))
    }

    pub fn primary(&mut self) -> ExprEnum {
        let token: &Token = self.peek();
        match &token.token_type {
            TokenType::False => {
                self.advance();
                ExprEnum::Literal(Box::new(expr::Literal {
                    value: Value::Bool(false),
                }))
            }
            TokenType::True => {
                self.advance();
                ExprEnum::Literal(Box::new(expr::Literal {
                    value: Value::Bool(true),
                }))
            }
            TokenType::Nil => {
                self.advance();
                ExprEnum::Literal(Box::new(expr::Literal { value: Value::Nil }))
            }
            TokenType::String(x) => {
                let value: String = x.clone();
                self.advance();
                ExprEnum::Literal(Box::new(expr::Literal {
                    value: Value::String(value),
                }))
            }
            TokenType::Number(x) => {
                let value: f32 = *x;
                self.advance();
                ExprEnum::Literal(Box::new(expr::Literal {
                    value: Value::Number(value),
                }))
            }
            TokenType::LeftParen => {
                self.advance();
                let expression: ExprEnum = self.expression();
                self.consume(&TokenType::RightParen, "Expect ')' after expression.");
                ExprEnum::Grouping(Box::new(expr::Grouping { expression }))
            }
            TokenType::Identifier(_) => {
                let name: Token = token.clone();
                self.advance();
                ExprEnum::Var(Box::new(expr::Var {
                    name,
                    id: self.var_count(),
                }))
            }
            TokenType::Super => {
                let keyword: Token = token.clone();
                self.advance();
                self.consume(&TokenType::Dot, "Expect '.' after 'super'.");
                let method = self
                    .consume(
                        &TokenType::Identifier("".into()),
                        "Expect superclass method name.",
                    )
                    .clone();
                ExprEnum::Super(Box::new(expr::Super {
                    keyword,
                    method,
                    id: self.var_count(),
                }))
            }
            TokenType::This => {
                let keyword: Token = token.clone();
                self.advance();
                ExprEnum::This(Box::new(expr::This {
                    keyword,
                    id: self.var_count(),
                }))
            }
            _ => panic!("{}", &self.peek().token_type),
        }
    }

    pub fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(&self.peek().token_type) == std::mem::discriminant(token_type)
    }

    pub fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.curr += 1;
        }
        self.previous()
    }

    pub fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    pub fn peek(&self) -> &Token {
        self.tokens.get(self.curr).unwrap()
    }

    pub fn previous(&self) -> &Token {
        self.tokens.get(self.curr - 1).unwrap()
    }

    pub fn consume(&mut self, token_type: &TokenType, message: &str) -> &Token {
        if self.check(token_type) {
            self.advance()
        } else {
            Lox::error_token(self.peek(), message);
            panic!()
        }
    }

    pub fn var_count(&mut self) -> usize {
        self.var_counter += 1;
        self.var_counter
    }

    pub fn error(&self, token: &Token, message: &str) {
        Lox::error_token(token, message)
    }

    pub fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::SemiColon {
                return;
            }
            match self.peek().token_type {
                TokenType::Class => (),
                TokenType::Fun => (),
                TokenType::Var => (),
                TokenType::For => (),
                TokenType::If => (),
                TokenType::While => (),
                TokenType::Print => (),
                TokenType::Return => (),
                _ => (),
            }
            self.advance();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    fn check_parse(code: &str, expected: &str) {
        let mut scanner: Scanner = Scanner::new(code.into());
        scanner.scan_tokens();

        let mut parser: Parser = Parser::new(scanner.tokens);
        for statement in parser.parse() {
            assert_eq!(statement.to_string(), expected);
        }
    }

    #[test]
    fn parse_short_expr() {
        check_parse(
            "2 + 3 * 5 / (1 + 2) > 7;",
            "(> (+ 2 (/ (* 3 5) (group (+ 1 2)))) 7)",
        )
    }
}
