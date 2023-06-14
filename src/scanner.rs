use crate::token::Token;
use crate::token_type::TokenType;
use crate::Lox;

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
        let last_token = Token::new(TokenType::Eof, "".to_string(), None, self.line);
        self.tokens.push(last_token);
    }

    fn scan_token(&mut self) {
        let c: char = self.advance();
        match c {
            '(' => self.add_token(TokenType::LeftParen, None),
            ')' => self.add_token(TokenType::RightParen, None),
            '{' => self.add_token(TokenType::LeftBrace, None),
            '}' => self.add_token(TokenType::RightBrace, None),
            ',' => self.add_token(TokenType::Comma, None),
            '.' => self.add_token(TokenType::Dot, None),
            '-' => self.add_token(TokenType::Minus, None),
            '+' => self.add_token(TokenType::Plus, None),
            ';' => self.add_token(TokenType::SemiColon, None),
            '*' => self.add_token(TokenType::Star, None),
            '!' =>
                if self.match_char('=') {self.add_token(TokenType::BangEqual, None) }
                else {self.add_token(TokenType::Bang, None)},
            '=' =>
                if self.match_char('=') { self.add_token(TokenType::EqualEqual, None) }
                else {self.add_token(TokenType::Equal, None)},
            '<' =>
                if self.match_char('=') { self.add_token(TokenType::LessEqual, None) }
                else {self.add_token(TokenType::Less, None)},
            '>' =>
                if self.match_char('=') { self.add_token(TokenType::GreaterEqual, None) }
                else {self.add_token(TokenType::Greater, None)},
            '/' =>
                if self.match_char('/') { while self.peek() != '\n' && self.is_at_end() { self.advance(); } }
                else { self.add_token(TokenType::Slash, None) },
            ' ' => (),
            'r' => (),
            't' => (),
            '\n' => self.line += 1,
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            _ => ()
        }
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() { self.advance(); };
        let value: String = self.source[(self.start)..(self.current)].to_string();
        match value.as_str() {
            "and" => self.add_token(TokenType::And, None),
            "class" => self.add_token(TokenType::Class, None),
            "else" => self.add_token(TokenType::Else, None),
            "false" => self.add_token(TokenType::False, None),
            "for" => self.add_token(TokenType::For, None),
            "fun" => self.add_token(TokenType::Fun, None),
            "if" => self.add_token(TokenType::If, None),
            "nil" => self.add_token(TokenType::Nil, None),
            "or" => self.add_token(TokenType::Or, None),
            "print" => self.add_token(TokenType::Print, None),
            "return" => self.add_token(TokenType::Return, None),
            "super" => self.add_token(TokenType::Super, None),
            "this" => self.add_token(TokenType::This, None),
            "true" => self.add_token(TokenType::True, None),
            "var" => self.add_token(TokenType::Var, None),
            "while" => self.add_token(TokenType::While, None),
            _ => ()
        };
      }
    
    fn number(&mut self) {
        while self.peek().is_numeric() { self.advance(); };
        if self.peek() == '.' && self.peek_next().is_numeric() {
            self.advance();
            while self.peek().is_numeric() { self.advance(); };
        }
        let value: String = self.source[(self.start)..(self.current)].to_string();
        self.add_token(TokenType::Number, Some(value))
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

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() { return '\0' }
        return self.source.chars().nth(self.current + 1).unwrap()
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' { self.line += 1}
            self.advance();
        }
        if self.is_at_end() {
            Lox::error(self.line, "Unterminated string.");
            return;
        }
        self.advance();
        let value: String = self.source[(self.start+1)..(self.current-1)].to_string();
        self.add_token(TokenType::String, Some(value))
   }

    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }

    fn advance(&mut self) -> char {
        let next_char: char = self.source.chars().nth(self.current).unwrap();
        self.current  += 1;
        return next_char
    }

    fn add_token(&mut self, types: TokenType, literal: Option<String>) {
        let lexeme: String = self.source[self.start..self.current].to_string();
        let new_token: Token = Token {types, lexeme, literal: literal, line: self.line};
        self.tokens.push(new_token)
    }
}