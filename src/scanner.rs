use crate::token::Token;

pub struct Scanner<T> {
    pub source: String,
    pub tokens: Vec<Token<T>>
}

impl<T> Scanner<T> {
    pub fn new(source: String) -> Scanner<T> {
        let tokens: Vec<Token<T>> = Vec::new();
        Scanner {source: source, tokens: tokens}
    }
}