use core::panic;

use crate::{Lox, stmt, expr};
use crate::expr::{ExprEnum, Binary, Grouping, Literal, Unary};
use crate::stmt::{Stmt, Print, Expression};
use crate::token::Token;
use crate::token_type::TokenType;
use crate::value::Value;

pub struct Parser {
    pub tokens: Vec<Token>,
    pub curr: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {tokens: tokens, curr: 0}
    }

    pub fn parse(&mut self) -> Vec<Box<dyn Stmt>> {
        let mut statements: Vec<Box<dyn Stmt>> = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration());
        }
        return statements
    }

    pub fn expression(&mut self) -> ExprEnum {
        self.assignment()
    }

    pub fn declaration(&mut self) -> Box<dyn Stmt> {
        //try {
        let token: &Token = self.peek();
        if token.token_type == TokenType::Var {
            self.advance();
            return self.var_declaration();
        } // consider changing to match
        return self.statement();
        //} catch (ParseError error) {
        //    synchronize();
        //    return null;
        //}
    }

    pub fn statement(&mut self) -> Box<dyn Stmt> {
        let token: &Token = self.peek();
        match token.token_type {
            TokenType::For => {
                self.advance();
                self.for_statement()
            },
            TokenType::If => {
                self.advance();
                self.if_statement()
            },
            TokenType::Print => {
                self.advance();
                self.print_statement()
            },
            TokenType::While => {
                self.advance();
                self.while_statement()
            },
            TokenType::LeftBrace => {
                self.advance();
                Box::new(stmt::Block { statements: self.block() })
            },
            _ => self.expression_statement()
        }
    }

    pub fn for_statement(&mut self) -> Box<dyn Stmt> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'for'.");
        let token = self.peek();
        let initializer = match token.token_type {
            TokenType::SemiColon => {
                self.advance();
                None
            },
            TokenType::Var => {
                self.advance();
                Some(self.var_declaration())
            },
            _ => {
                self.advance();
                Some(self.expression_statement())
            }
        };

        let condition: ExprEnum = if self.check(&TokenType::SemiColon) {
            ExprEnum::Literal(Box::new(expr::Literal {value: Value::Bool(true) }))
        }
        else {
            self.expression()
        };

        self.consume(&TokenType::SemiColon, "Expect ';' after loop condition.");
        
        let mut increment = None;
        if !self.check(&TokenType::RightParen) {
            increment = Some(self.expression()) 
        }

        self.consume(&TokenType::RightParen, "Expect ')' after for clauses.");

        let mut body = self.statement();

        match increment {
            Some(x) => {
                body = Box::new(stmt::Block {statements: vec![body, Box::new(stmt::Expression { expression: x } )]})
            },
            None => ()
        }

        body = Box::new(stmt::While { condition, body });

        match initializer {
            Some(x) => {
                body = Box::new(stmt::Block {statements: vec![x, body]})
            },
            None => ()
        }

        return body
    }

    pub fn if_statement(&mut self) -> Box<dyn Stmt> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'if'.");
        let condition: ExprEnum = self.expression();
        self.consume(&TokenType::RightParen, "Expect ')' after 'if' condition.");
        let then_branch: Box<dyn Stmt> = self.statement();
        let token = self.peek();
        let else_branch: Option<Box<dyn Stmt>> = match token.token_type {
            TokenType::Else => Some(self.statement()),
            _ => None
        };
        return Box::new(stmt::If { condition, then_branch, else_branch})
    }

    pub fn print_statement(&mut self) -> Box<dyn Stmt> {
        let value: ExprEnum = self.expression();
        self.consume(&TokenType::SemiColon, "Expect ';' after value.");
        Box::new(Print {expression: value})
    }

    pub fn while_statement(&mut self) -> Box<dyn Stmt> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'while'.");
        let condition = self.expression();
        self.consume(&TokenType::RightParen, "Expect ')' after condition.");
        let body = self.statement();
    
        return Box::new(stmt::While { condition, body });
    }

    pub fn var_declaration(&mut self) -> Box<dyn Stmt> {
        let name: Token = self.consume(&TokenType::Identifier("".into()), "Expect variable name.").clone();

        let token: &Token = self.peek();

        let initializer: ExprEnum = match token.token_type {
            TokenType::Equal => {
                self.advance();
                self.expression()
            },
            _ => ExprEnum::Literal(Box::new(Literal {value: Value::Nil}))
        };

        self.consume(&TokenType::SemiColon, "Expect ';' after variable declaration.");

        return Box::new(stmt::Var { name: name, initializer });
    }

    pub fn expression_statement(&mut self) -> Box<dyn Stmt> {
        let value: ExprEnum = self.expression();
        self.consume(&TokenType::SemiColon, "Expect ';' after value.");
        Box::new(Expression {expression: value})
    }

    pub fn block(&mut self) -> Vec<Box<dyn Stmt>> {
        let mut statements: Vec<Box<dyn Stmt>> = Vec::new();
    
        loop {
            let token = self.peek();
            match token.token_type {
                TokenType::RightBrace => break,
                TokenType::Eof => break,
                _ => statements.push(self.declaration())
            }
        }

        self.consume(&TokenType::RightBrace, "Expect '}' after block.".into());
        return statements;
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
                        return ExprEnum::Assign(Box::new(expr::Assign {name, value}));
                    },
                    _ => panic!("{} {}", equals, "Invalid assignment target.")
                }
            },
            _ => return expr
        }
    }

    pub fn or(&mut self) -> ExprEnum {
        let mut expr = self.and();

        loop {
            match self.peek().token_type {
                TokenType::Or => {
                    let operator: Token = self.peek().clone();
                    self.advance();
                    let right: ExprEnum = self.and();
                    expr = ExprEnum::Logic(Box::new(expr::Logic { left: expr, operator, right }))
                },
                _ => break
            }
        }

        return expr
    }

    pub fn and(&mut self) -> ExprEnum {
        let mut expr: ExprEnum = self.equality();

        loop {
            match self.peek().token_type {
                TokenType::And => {
                    let operator: Token = self.peek().clone();
                    self.advance();
                    let right: ExprEnum = self.equality();
                    expr = ExprEnum::Logic(Box::new( expr::Logic {left: expr, operator, right} ))
                },
                _ => break
            }
        }

        return expr
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
                    expr = ExprEnum::Binary(Box::new(Binary {left: expr, operator, right}));
                },
                _ => break
            }
        }
        return expr
    }

    pub fn comparison(&mut self) -> ExprEnum {
        let mut expr: ExprEnum = self.term();
        loop {
            let token: &Token = self.peek();
            match &token.token_type {
                &TokenType::Greater | &TokenType::GreaterEqual | &TokenType::Less | &TokenType::LessEqual => {
                    let operator: Token = token.clone();
                    self.advance();
                    let right: ExprEnum = self.term();
                    expr = ExprEnum::Binary(Box::new(Binary {left: expr, operator, right}));
                },
                _ => break
            }
        }
        return expr
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
                    expr = ExprEnum::Binary(Box::new(Binary {left: expr, operator, right}));
                },
                _ => break
            }
        }
        return expr
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
                    expr = ExprEnum::Binary(Box::new(Binary {left: expr, operator, right}));
                },
                _ => break
            }
        }
        return expr
    }
    
    pub fn unary(&mut self) -> ExprEnum {
        let token: &Token = self.peek();
        match &token.token_type {
            TokenType::Bang | TokenType::Minus => {
                let operator: Token = token.clone();
                self.advance();
                let right: ExprEnum = self.unary();
                return ExprEnum::Unary(Box::new(Unary {operator, right}));
            },
            _ => return self.primary(),
        }
    }

    pub fn primary(&mut self) -> ExprEnum {
        let token: &Token = self.peek();
        match &token.token_type {
            TokenType::False => {
                self.advance();
                return ExprEnum::Literal(Box::new(Literal {value: Value::Bool(false)}))
                },
            TokenType::True => {
                self.advance();
                return ExprEnum::Literal(Box::new(Literal {value: Value::Bool(true)}))
                },
            TokenType::Nil => {
                self.advance();
                return ExprEnum::Literal(Box::new(Literal {value: Value::Nil}))
                },
            TokenType::String(x) => {
                let value: String = x.clone();
                self.advance();
                return ExprEnum::Literal(Box::new(Literal {value: Value::String(value)}))
                },
            TokenType::Number(x) => {
                let value: f32 = x.clone();
                self.advance();
                return ExprEnum::Literal(Box::new(Literal {value: Value::Number(value)}))
                },
            TokenType::LeftParen => {
                self.advance();
                let expression: ExprEnum = self.expression();
                self.consume(&TokenType::RightParen, "Expect ')' after expression.");
                return ExprEnum::Grouping(Box::new(Grouping {expression}))
                },
            TokenType::Identifier(_) => {
                let name: Token = token.clone();
                self.advance();
                return ExprEnum::Var(Box::new(expr::Var {name}))
            }
            _ => panic!("{}", &self.peek().token_type)
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


#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    
    fn check_parse(code: &str, expected: &str) {
        let mut scanner : Scanner = Scanner::new(code.into());
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
            "(> (+ 2 (/ (* 3 5) (group (+ 1 2)))) 7)"
        )
    }
}
