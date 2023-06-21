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
        let last_token = Token::new(TokenType::Eof, "".to_string(), self.line);
        self.tokens.push(last_token);
    }

    fn scan_token(&mut self) {
        let c: char = self.advance();
        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::SemiColon),
            '*' => self.add_token(TokenType::Star),
            '!' =>
                if self.match_char('=') {self.add_token(TokenType::BangEqual) }
                else {self.add_token(TokenType::Bang)},
            '=' =>
                if self.match_char('=') { self.add_token(TokenType::EqualEqual) }
                else {self.add_token(TokenType::Equal)},
            '<' =>
                if self.match_char('=') { self.add_token(TokenType::LessEqual) }
                else {self.add_token(TokenType::Less)},
            '>' =>
                if self.match_char('=') { self.add_token(TokenType::GreaterEqual) }
                else {self.add_token(TokenType::Greater)},
            '/' =>
                if self.match_char('/') { while self.peek() != '\n' && self.is_at_end() { self.advance(); } }
                else { self.add_token(TokenType::Slash) },
            ' ' => (),
            '\r' => (),
            '\t' => (),
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
            "and" => self.add_token(TokenType::And),
            "class" => self.add_token(TokenType::Class),
            "else" => self.add_token(TokenType::Else),
            "false" => self.add_token(TokenType::False),
            "for" => self.add_token(TokenType::For),
            "fun" => self.add_token(TokenType::Fun),
            "if" => self.add_token(TokenType::If),
            "nil" => self.add_token(TokenType::Nil),
            "or" => self.add_token(TokenType::Or),
            "print" => self.add_token(TokenType::Print),
            "return" => self.add_token(TokenType::Return),
            "super" => self.add_token(TokenType::Super),
            "this" => self.add_token(TokenType::This),
            "true" => self.add_token(TokenType::True),
            "var" => self.add_token(TokenType::Var),
            "while" => self.add_token(TokenType::While),
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
        let float: f32 = value.parse::<f32>().unwrap();
        self.add_token(TokenType::Number(float))
   }

    fn match_char(&mut self, expected: char) -> bool {
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
        self.add_token(TokenType::String(value))
   }

    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }

    fn advance(&mut self) -> char {
        let next_char: char = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        return next_char
    }

    fn add_token(&mut self, token_type: TokenType) {
        let lexeme: String = self.source[self.start..self.current].to_string();
        let new_token: Token = Token {token_type, lexeme, line: self.line};
        self.tokens.push(new_token)
    }
}