use crate::token_type::TokenType;

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: u32
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line:u32) -> Token {
        Token {
            token_type: token_type,
            lexeme: lexeme,
            line: line
        }
    }

    pub fn to_string(&self) -> String {
        match &self.token_type {
            TokenType::Identifier(x) => format!("{} {} {}", &self.token_type, &self.lexeme, x),
            TokenType::String(x) => format!("{} {} {}", &self.token_type, &self.lexeme, x),
            TokenType::Number(x) => format!("{} {} {}", &self.token_type, &self.lexeme, x),
            _ => format!("{} {} {}", &self.token_type, &self.lexeme, &self.token_type),
        }
    }
}
