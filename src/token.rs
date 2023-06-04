use crate::token_type::TokenType;

pub struct Token<T> {
    pub types: TokenType,
    pub lexeme: String,
    pub literal: T,
    pub line: u32
}

impl<T: std::fmt::Display> Token<T> {
    pub fn new(types: TokenType, lexeme: String, literal: T, line:u32) -> Token<T> {
        Token {
            types: types,
            lexeme: lexeme,
            literal: literal,
            line: line
        }
    }

    pub fn to_string(&self) -> String {
        format!("{} {} {}", &self.types, &self.lexeme, &self.literal)
    }
}