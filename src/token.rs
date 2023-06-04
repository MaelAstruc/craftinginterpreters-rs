use crate::token_type::TokenType;

pub struct Token {
    pub types: TokenType,
    pub lexeme: String,
    pub literal: String,
    pub line: u32
}

impl Token {
    pub fn new(types: TokenType, lexeme: String, literal: String, line:u32) -> Token {
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
