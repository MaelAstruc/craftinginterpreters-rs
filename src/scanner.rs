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

    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }
}