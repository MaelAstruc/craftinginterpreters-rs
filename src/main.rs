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
use crate::expr::Expr;
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
        let code: String = fs::read_to_string(filepath).unwrap();
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

        let mut parser: Parser = Parser::new(scanner.tokens);
        let expression: Box<dyn Expr> = parser.parse();

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
    use std::f32::INFINITY;

    use super::*;
    use expr::Value;

    fn check_scan(code: &str, expected: Vec<TokenType>) {
        let mut scanner : Scanner = Scanner::new(code.into());
        scanner.scan_tokens();

        for i in 0..expected.len() {
            assert_eq!(scanner.tokens[i].token_type, expected[i]);
        }
    }

    fn check_parse(code: &str, expected: &str) {
        let mut scanner : Scanner = Scanner::new(code.into());
        scanner.scan_tokens();

        let mut parser: Parser = Parser::new(scanner.tokens);

        assert_eq!(parser.parse().to_string(), expected);
    }

    fn check_evaluate(code: &str, expected: Value) {
        let mut scanner : Scanner = Scanner::new(code.into());
        scanner.scan_tokens();

        let mut parser: Parser = Parser::new(scanner.tokens);

        let result: Option<Value> = parser.parse().evaluate();
        
        assert_eq!(result.unwrap(), expected);
    }

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
    fn test_scan_primitives() {
        check_scan(
            "\"Hello World !\"",
            vec![TokenType::String("Hello World !".into())]
        );
        check_scan(
            "nil",
            vec![TokenType::Nil]
        );
        check_scan(
            "1",
            vec![TokenType::Number(1.0)]
        );
        check_scan(
            "1.7",
            vec![TokenType::Number(1.7)]
        );
        check_scan(
            "true",
            vec![TokenType::True]
        );
        check_scan(
            "false",
            vec![TokenType::False]
        )
    }

    #[test]
    fn scan_short_tokens() {
        check_scan(
            "=+(){},;>",
            vec![
                TokenType::Equal,
                TokenType::Plus,
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::RightBrace,
                TokenType::Comma,
                TokenType::SemiColon,
                TokenType::Greater
            ]
        )
    }

    #[test]
    fn parse_short_expr() {
        check_parse(
            "// Test
                2 + 3 * 5 / (1 + 2) > 7",
            "(> (+ 2 (/ (* 3 5) (group (+ 1 2)))) 7)"
        )
    }

    #[test]
    fn evaluate_nil() {
        let code: &str = "nil";
        let mut scanner : Scanner = Scanner::new(code.into());
        scanner.scan_tokens();
        
        let mut parser: Parser = Parser::new(scanner.tokens);

        let result: Option<Value> = parser.parse().evaluate();
        
        let expected: Value = Value::None(None);

        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn evaluate_primary() {
        check_evaluate("true", Value::Bool(true));
        check_evaluate("false", Value::Bool(false));
        check_evaluate("1", Value::F32(1.0));
        check_evaluate("42", Value::F32(42.0));
        check_evaluate("1.37", Value::F32(1.37));
        check_evaluate("\"Hello World !\"", Value::String("Hello World !".into()));
        check_evaluate("\"\t Hello \r\n World !\"", Value::String("\t Hello \r\n World !".into()));
        check_evaluate("nil", Value::None(None))
    }

    #[test]
    fn evaluate_bool() {
        check_evaluate("!false", Value::Bool(true));
        check_evaluate("!1", Value::Bool(false));
        check_evaluate("!\"a\"", Value::Bool(false));
        check_evaluate("!nil", Value::Bool(false));
    }

    #[test]
    fn evaluate_grouping() {
        check_evaluate("(-1)", Value::F32(-1.0));
        check_evaluate("((-1))", Value::F32(-1.0))
    }

    #[test]
    fn evaluate_unary() {
        check_evaluate("!true", Value::Bool(false));
        check_evaluate("!!true", Value::Bool(true));
        check_evaluate("!!!true", Value::Bool(false));
        check_evaluate("-1", Value::F32(-1.0));
        check_evaluate("--1", Value::F32(1.0));
    }

    #[test]
    fn evaluate_binary() {
        check_evaluate("5-3", Value::F32(2.0));
        check_evaluate("5 - 3", Value::F32(2.0));
        check_evaluate("5--3", Value::F32(8.0));
        check_evaluate("5+3", Value::F32(8.0));
        check_evaluate("\"Hello\"+ \" \" + \"World\"", Value::String("Hello World".into()));
        check_evaluate("3*5", Value::F32(15.0));
        check_evaluate("3*0", Value::F32(0.0));
        check_evaluate("3/5", Value::F32(0.6));
        check_evaluate("3/0", Value::F32(INFINITY));
        check_evaluate("5>3", Value::Bool(true));
        check_evaluate("5>5", Value::Bool(false));
        check_evaluate("5>7", Value::Bool(false));
        check_evaluate("5>=3", Value::Bool(true));
        check_evaluate("5>=5", Value::Bool(true));
        check_evaluate("5>=7", Value::Bool(false));
        check_evaluate("5<3", Value::Bool(false));
        check_evaluate("5<5", Value::Bool(false));
        check_evaluate("5<7", Value::Bool(true));
        check_evaluate("5<=3", Value::Bool(false));
        check_evaluate("5<=5", Value::Bool(true));
        check_evaluate("5<=7", Value::Bool(true));
        check_evaluate("5==5", Value::Bool(true));
        check_evaluate("5==7", Value::Bool(false));
        check_evaluate("\"a\"==\"a\"", Value::Bool(true));
        check_evaluate("\"a\"==\"b\"", Value::Bool(false));
        check_evaluate("true==true", Value::Bool(true));
        check_evaluate("true==false", Value::Bool(false));
        check_evaluate("nil==nil", Value::Bool(true));
        check_evaluate("5!=5", Value::Bool(false));
        check_evaluate("5!=7", Value::Bool(true));
        check_evaluate("\"a\"!=\"a\"", Value::Bool(false));
        check_evaluate("\"a\"!=\"b\"", Value::Bool(true));
        check_evaluate("true!=true", Value::Bool(false));
        check_evaluate("true!=false", Value::Bool(true));
        check_evaluate("nil!=nil", Value::Bool(false));
        check_evaluate("1==true", Value::Bool(false));
        check_evaluate("1==\"a\"", Value::Bool(false));
        check_evaluate("1==nil", Value::Bool(false));
    }
}