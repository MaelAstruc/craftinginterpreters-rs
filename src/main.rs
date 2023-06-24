use std::fs;
use std::io;
use std::process;

pub mod expr;
pub mod interpreter;
pub mod parser;
pub mod runtime_error;
pub mod scanner;
pub mod token_type;
pub mod token;
pub mod value;

use interpreter::Interpreter;
use runtime_error::RuntimeError;

use crate::expr::Expr;
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::token::Token;
use crate::token_type::TokenType;

pub struct Lox {
    had_error: bool,
    had_runtime_error: bool
}

impl Lox {
    fn main(&mut self, args: &mut [&str]) {
        if args.len() > 1 {
            println!("Usage: stata [script]")
        } else if args.len() == 1 {
            self.run_file(args[0])
        } else {
            self.run_prompt()
        }
    }

    pub fn run_file(&mut self, filepath: &str) {
        let code: String = fs::read_to_string(filepath).unwrap();
        self.run(code);
        if self.had_error {
            process::exit(65)
        }
        if self.had_runtime_error {
            process::exit(75)
        }
    }

    pub fn run_prompt(&mut self) {
        let mut buffer: String = String::new();
        let stdin: io::Stdin = io::stdin();
        while stdin.read_line(&mut buffer).is_ok() {
            let trimmed: &str = buffer.trim_end();
            println!("> {}", trimmed);
            self.run(trimmed.to_string());
            buffer.clear();
        }
    }

    pub fn run(&mut self, code: String) {
        let mut scanner: Scanner = Scanner::new(code);
        scanner.scan_tokens();

        let mut parser: Parser = Parser::new(scanner.tokens);
        let expression: Box<dyn Expr> = parser.parse();

        Interpreter::interpret(self, expression)
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

    fn runtime_error(&mut self, error: RuntimeError) {
        println!("{} \n[line {}]", error.message, error.token.line);
        self.had_runtime_error = true;
    }

    fn report(line: u32, at: &str, message: &str) {
        println!("[line {}] Error {}: {}", line, at, message)
    }

}

fn main() {
    let mut input = ["C:/Users/Mael/Documents/Temp/test.lox"];
    let mut lox: Lox = Lox {
        had_error: false,
        had_runtime_error: false
    };
    lox.main(&mut input)
}
