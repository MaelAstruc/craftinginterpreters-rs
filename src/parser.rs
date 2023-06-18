use core::panic;

use crate::Lox;
use crate::expr::{Expr, Binary, Grouping, Literal, Unary};
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
        let mut expr = self.comparison();
        while self.match_token(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator: Token = self.previous().clone();
            let right = self.comparison();
            expr = Box::new(Binary {left: expr, operator, right});
        }
        return expr
    }

    pub fn comparison(&mut self) -> Box<dyn Expr> {
        let mut expr = self.term();

        while self.match_token(&[TokenType::Greater, TokenType::GreaterEqual, TokenType:: Less, TokenType::LessEqual]) {
            let operator: Token = self.previous().clone();
            let right = self.term();
            expr = Box::new(Binary {left: expr, operator, right});
        }
    return expr
    }

    pub fn term(&mut self) -> Box<dyn Expr> {
        let mut expr = self.factor();
        while self.match_token(&[TokenType::Minus, TokenType::Plus]) {
            let operator: Token = self.previous().clone();
            let right = self.factor();
            expr = Box::new(Binary {left: expr, operator, right});
        }
    return expr
    }

    pub fn factor(&mut self) -> Box<dyn Expr> {
        let mut expr = self.unary();
        while self.match_token(&[TokenType::Slash, TokenType::Star]) {
            let operator: Token = self.previous().clone();
            let right = self.unary();
            expr = Box::new(Binary {left: expr, operator, right});
        }
    return expr
    }
    
    pub fn unary(&mut self) -> Box<dyn Expr> {
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let operator: Token = self.previous().clone();
            let right = self.unary();
            return Box::new(Unary {operator, right});
        }
        return self.primary()
    }

    pub fn primary(&mut self) -> Box<dyn Expr> {
        if self.match_token(&[TokenType::False]) {
            return Box::new(Literal {value: Some(false)})
        }
        if self.match_token(&[TokenType::True]) {
            return Box::new(Literal {value: Some(true)})
        }
        if self.match_token(&[TokenType::Nil]) {
            return Box::new(Literal {value: None::<u8>})
        }
        if self.match_token(&[TokenType::Number, TokenType::String]) {
            return Box::new(Literal {value: self.previous().literal.clone()})
        }
        if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression();
            self.consume(&TokenType::RightParen, "Expect ')' after expression.");
            return Box::new(Grouping {expression: expr})
        }
        else {
            panic!("{}", &self.peek().types)
        }
    }

    pub fn match_token(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(token_type) {
                self.advance();
                return true
            }
        }
        return false
    }

    pub fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.peek().types == token_type
    }

    pub fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.curr += 1;
        }
        self.previous()
    }

    pub fn is_at_end(&self) -> bool {
        self.peek().types == TokenType::Eof
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
            if self.previous().types == TokenType::SemiColon {
                return;
            }
            match self.peek().types {
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