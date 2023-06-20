use std::fs;
use std::io;
use std::process;

pub mod expr;
pub mod token_type;
pub mod token;
pub mod parser;
pub mod scanner;

use crate::scanner::Scanner;
use crate::token::Token;
use crate::token_type::TokenType;
use crate::parser::Parser;

struct Lox {
    had_error: bool
}

impl Lox {
    fn main(&self, args: &mut [&str]) {
        if args.len() > 1 {
            println!("Usage: stata [script]")
        } else if args.len() == 1 {
            self.run_file(args[0])
        } else {
            self.run_prompt()
        }
    }

    pub fn run_file(&self, filepath: &str) {
        let code = fs::read_to_string(filepath).unwrap();
        self.run(code);
        if self.had_error {
            process::exit(65)
        }
    }

    pub fn run_prompt(&self) {
        let mut buffer: String = String::new();
        let stdin: io::Stdin = io::stdin();
        while stdin.read_line(&mut buffer).is_ok() {
            let trimmed: &str = buffer.trim_end();
            println!("> {}", trimmed);
            self.run(trimmed.to_string());
            buffer.clear();
        }
    }

    pub fn run(&self, code: String) {
        let mut scanner: Scanner = Scanner::new(code);
        scanner.scan_tokens();

        let mut parser = Parser::new(scanner.tokens);
        let expression = parser.parse();

        println!("{}", expression)
    }

    fn error(line: u32, message: &str) {
        Self::report(line, "", message)
    }

    fn error_token(token: &Token, message: &str) {
        if token.token_type == TokenType::Eof {
            Self::report(token.line, " at the end", &message)
        }
        else {
            let at: String = " at'".to_owned() + token.lexeme.as_ref() + "'";
            Self::report(token.line, at.as_str(), &message)
        }
    }

    fn report(line: u32, at: &str, message: &str) {
        println!("[line {}] Error {}: {}", line, at, message)
    }

}

fn main() {
    let mut input = ["C:/Users/Mael/Documents/Temp/test.lox"];
    let lox: Lox = Lox {
        had_error: false
    };
    lox.main(&mut input)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn print_token_type() {
        assert_eq!(TokenType::And.to_string(), "And")
    }

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

    #[test]
    fn scan_short_tokens() {
        let code: &str = "=+(){},;>";
        let mut scanner : Scanner = Scanner::new(code.into());
        scanner.scan_tokens();
        
        let tokens: Vec<TokenType> = vec![
            TokenType::Equal,
            TokenType::Plus,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::RightBrace,
            TokenType::Comma,
            TokenType::SemiColon,
            TokenType::Greater
        ];
        for i in 0..tokens.len() {
            assert_eq!(scanner.tokens[i].token_type, tokens[i]);
        }
    }

    #[test]
    fn parse_short_expr() {
        let code: &str = "// Test
            2 + 3 * 5 / (1 + 2) > 7";
        let mut scanner : Scanner = Scanner::new(code.into());
        scanner.scan_tokens();

        let mut parser: Parser = Parser::new(scanner.tokens);

        let expression: &str = "(> (+ 2 (/ (* 3 5) (group (+ 1 2)))) 7)";

        assert_eq!(parser.parse().to_string(), expression);
    }


}