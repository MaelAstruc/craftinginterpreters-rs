use core::panic;

use crate::{Lox};
use crate::expr::{Expr, Binary, Grouping, Literal, Unary, Value};
use crate::token::Token;
use crate::token_type::TokenType;

pub struct Parser {
    pub tokens: Vec<Token>,
    pub curr: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {tokens: tokens, curr: 0}
    }

    pub fn parse(&mut self) -> Box<dyn Expr> {
        self.expression()
    }

    pub fn expression(&mut self) -> Box<dyn Expr> {
        self.equality()
    }

    pub fn equality(&mut self) -> Box<dyn Expr> {
        let mut expr: Box<dyn Expr> = self.comparison();
        loop {
            let token: &Token = self.peek();
            match &token.token_type {
                TokenType::BangEqual | TokenType::EqualEqual => {
                    let operator: Token = token.clone();
                    self.advance();
                    let right: Box<dyn Expr> = self.comparison();
                    expr = Box::new(Binary {left: expr, operator, right});
                },
                TokenType::Eof | _ => break
            }
        }
        return expr
    }

    pub fn comparison(&mut self) -> Box<dyn Expr> {
        let mut expr: Box<dyn Expr> = self.term();
        loop {
            let token: &Token = self.peek();
            match &token.token_type {
                &TokenType::Greater | &TokenType::GreaterEqual | &TokenType::Less | &TokenType::LessEqual => {
                    let operator: Token = token.clone();
                    self.advance();
                    let right: Box<dyn Expr> = self.term();
                    expr = Box::new(Binary {left: expr, operator, right});
                },
                TokenType::Eof | _ => break
            }
        }
        return expr
    }

    pub fn term(&mut self) -> Box<dyn Expr> {
        let mut expr: Box<dyn Expr> = self.factor();
        loop {
            let token: &Token = self.peek();
            match &token.token_type {
                &TokenType::Minus | &TokenType::Plus => {
                    let operator: Token = token.clone();
                    self.advance();
                    let right: Box<dyn Expr> = self.factor();
                    expr = Box::new(Binary {left: expr, operator, right});
                },
                TokenType::Eof | _ => break
            }
        }
    return expr
    }

    pub fn factor(&mut self) -> Box<dyn Expr> {
        let mut expr: Box<dyn Expr> = self.unary();
        loop {
            let token: &Token = self.peek();
            match &token.token_type {
                TokenType::Slash | TokenType::Star => {
                    let operator: Token = token.clone();
                    self.advance();
                    let right: Box<dyn Expr> = self.unary();
                    expr = Box::new(Binary {left: expr, operator, right});
                },
                TokenType::Eof | _ => break
            }
        }
        return expr
    }
    
    pub fn unary(&mut self) -> Box<dyn Expr> {
        let token: &Token = self.peek();
        match &token.token_type {
            TokenType::Bang | TokenType::Minus => {
                let operator: Token = token.clone();
                self.advance();
                let right: Box<dyn Expr> = self.unary();
                return Box::new(Unary {operator, right});
            },
            TokenType::Eof | _ => return self.primary(),
        }
    }

    pub fn primary(&mut self) -> Box<dyn Expr> {
        let token: &Token = self.peek();
        match &token.token_type {
            TokenType::False => {
                self.advance();
                return Box::new(Literal {value: Some(Value::Bool(false))})
                },
            TokenType::True => {
                self.advance();
                return Box::new(Literal {value: Some(Value::Bool(true))})
                },
            TokenType::Nil => {
                self.advance();
                return Box::new(Literal {value: Some(Value::None(None::<u8>))})
                },
            TokenType::Number(x) | TokenType::String(x) => {
                let value: String = x.clone();
                self.advance();
                return Box::new(Literal {value: Some(Value::String(value))})
                },
            TokenType::LeftParen => {
                self.advance();
                let expression: Box<dyn Expr> = self.expression();
                self.consume(&TokenType::RightParen, "Expect ')' after expression.");
                return Box::new(Grouping {expression})
                },
            TokenType::Eof | _ => panic!("{}", &self.peek().token_type)
        }
    }

    pub fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.peek().token_type == token_type
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

    pub fn consume (&mut self, token_type: &TokenType, message: &str) -> &Token {
        if self.check(token_type) {
            self.advance()
        }
        else {
            Lox::error_token(self.peek(), message);
            panic!()
        }
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
                _ => ()
            }
            self.advance();
        }
    }

}