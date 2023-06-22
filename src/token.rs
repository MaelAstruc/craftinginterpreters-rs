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


#[cfg(test)]
mod tests {
    use crate::token::Token;
    use crate::token_type::TokenType;

    #[test]
    fn print_token() {
        let token: Token = Token::new(
            TokenType::String("String".into()),
            "lexeme".to_string(),
            1);
        assert_eq!(token.to_string(), "String lexeme String");
        let token: Token = Token::new(
            TokenType::And,
            "and".to_string(),
            1);
        assert_eq!(token.to_string(), "And and And");
    }
}