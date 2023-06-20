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
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.token_type {
            TokenType::Identifier(x) => write!(f, "{} {} {}", &self.token_type, &self.lexeme, x),
            TokenType::String(x) => write!(f, "{} {} {}", &self.token_type, &self.lexeme, x),
            TokenType::Number(x) => write!(f, "{} {} {}", &self.token_type, &self.lexeme, x),
            _ => write!(f, "{} {} {}", &self.token_type, &self.lexeme, &self.token_type)
        }
      }
}