use crate::token_type::TokenType;

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<String>,
    pub line: u32
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, literal: Option<String>, line:u32) -> Token {
        Token {
            token_type: token_type,
            lexeme: lexeme,
            literal: literal,
            line: line
        }
    }

    pub fn to_string(&self) -> String {
        match &self.literal {
          Some(x) => format!("{} {} {}", &self.token_type, &self.lexeme, x),
          None => format!("{} {} None", &self.token_type, &self.lexeme)
        }
    }
}
