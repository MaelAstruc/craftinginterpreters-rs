use crate::token::Token;
use crate::token_type::TokenType;

pub struct Scanner {
    pub source: String,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: u32
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        let tokens: Vec<Token> = Vec::new();
        Scanner {source: source, tokens: tokens, start: 0, current: 0, line: 1}
    }

    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        let last_token = Token::new(TokenType::Eof, "".to_string(), "".to_string(), self.line);
        self.tokens.push(last_token);
    }

    fn scan_token(&mut self) {
        let c: char = self.advance();
        match c {
            '(' => self.add_token(TokenType::LeftParen, "".to_string()),
            ')' => self.add_token(TokenType::RightParen, "".to_string()),
            '{' => self.add_token(TokenType::LeftBrace, "".to_string()),
            '}' => self.add_token(TokenType::RightBrace, "".to_string()),
            ',' => self.add_token(TokenType::Comma, "".to_string()),
            '.' => self.add_token(TokenType::Dot, "".to_string()),
            '-' => self.add_token(TokenType::Minus, "".to_string()),
            '+' => self.add_token(TokenType::Plus, "".to_string()),
            ';' => self.add_token(TokenType::SemiColon, "".to_string()),
            '*' => self.add_token(TokenType::Star, "".to_string()),
            '!' =>
                if self.match_char('=') {self.add_token(TokenType::BangEqual, "".to_string()) }
                else {self.add_token(TokenType::Bang, "".to_string())},
            '=' =>
                if self.match_char('=') { self.add_token(TokenType::EqualEqual, "".to_string()) }
                else {self.add_token(TokenType::Equal, "".to_string())},
            '<' =>
                if self.match_char('=') { self.add_token(TokenType::LessEqual, "".to_string()) }
                else {self.add_token(TokenType::Less, "".to_string())},
            '>' =>
                if self.match_char('=') { self.add_token(TokenType::GreaterEqual, "".to_string()) }
                else {self.add_token(TokenType::Greater, "".to_string())},
            '/' =>
                if self.match_char('/') { while self.peek() != '\n' && self.is_at_end() { self.advance(); } }
                else { self.add_token(TokenType::Slash, "".to_string()) },
            ' ' => (),
            'r' => (),
            't' => (),
            '\n' => self.line += 1,
            _ => ()
        }
    }

    fn match_char(&mut self, expected: char) ->bool {
        if self.is_at_end() {
            return false
        }
        if self.source.chars().nth(self.current).unwrap() != expected {
            return false
        }
        self.current += 1;
        return true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {return '\0'}
        return self.source.chars().nth(self.current).unwrap()
    }

    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }

    fn advance(&mut self) -> char {
        let next_char: char = self.source.chars().nth(self.current).unwrap();
        self.current  += 1;
        return next_char
    }

    fn add_token(&mut self, types: TokenType, literal: String) {
        let lexeme: String = self.source[self.start..self.current].to_string();
        let new_token = Token {types, lexeme, literal, line: self.line};
        self.tokens.push(new_token)
    }
}