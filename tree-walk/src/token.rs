use crate::token_type::TokenType;

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: u32,
}

impl Token {}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.token_type {
            TokenType::Identifier(x) => {
                write!(f, "Idenfier({}) {} {}", &self.token_type, &self.lexeme, x)
            }
            TokenType::String(x) => {
                write!(f, "String({}) {} {}", &self.token_type, &self.lexeme, x)
            }
            TokenType::Number(x) => {
                write!(f, "Number({}) {} {}", &self.token_type, &self.lexeme, x)
            }
            _ => write!(
                f,
                "{} {} {}",
                &self.token_type, &self.lexeme, &self.token_type
            ),
        }
    }
}
