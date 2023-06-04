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
   }

    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }
}