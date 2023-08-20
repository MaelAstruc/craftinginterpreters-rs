use std::fs;
use std::io::Write;
use std::process;

pub mod callable;
pub mod environment;
pub mod expr;
pub mod interpreter;
pub mod parser;
pub mod resolver;
pub mod runtime_error;
pub mod scanner;
pub mod stmt;
pub mod token;
pub mod token_type;
pub mod value;

use interpreter::Interpreter;
use resolver::Resolver;
use runtime_error::RuntimeError;

use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::token::Token;
use crate::token_type::TokenType;

pub struct Lox {
    had_error: bool,
    had_runtime_error: bool,
}

impl Lox {
    fn main(&mut self, args: &Vec<String>) {
        let mut interpreter = Interpreter::new();
        match args.len() {
            1 => self.run_prompt(&mut interpreter),
            2 => self.run_file(args.get(1).unwrap(), &mut interpreter),
            _ => {
                println!("Usage: Lox [script]");
                process::exit(64)
            }
        }
    }

    pub fn run_file(&mut self, filepath: &String, interpreter: &mut Interpreter) {
        let code: String = fs::read_to_string(filepath).unwrap();
        self.run(code, interpreter);
        if self.had_error {
            process::exit(65)
        }
        if self.had_runtime_error {
            process::exit(75)
        }
    }

    pub fn run_prompt(&mut self, interpreter: &mut Interpreter) {
        let mut buffer: String = String::new();
        loop {
            print!("> ");
            std::io::stdout().flush().unwrap();
            match std::io::stdin().read_line(&mut buffer) {
                Ok(_) if buffer.trim_end().is_empty() => break,
                Ok(_) if buffer.trim_end().ends_with("///") => {
                    buffer = buffer.trim_end().trim_end_matches("///").to_string()
                }
                Ok(_) => {
                    self.run(buffer.trim_end().to_string(), interpreter);
                    buffer = String::new();
                }
                Err(_) => break,
            }
        }
    }

    pub fn run(&mut self, code: String, interpreter: &mut Interpreter) {
        let mut scanner: Scanner = Scanner::new(code);
        scanner.scan_tokens();

        let mut parser: Parser = Parser::new(scanner.tokens);
        let statements: Vec<Box<stmt::StmtEnum>> = parser.parse();

        if self.had_error {
            return;
        }

        let mut resolver = Resolver::new(interpreter);
        resolver.resolve(&statements);

        if self.had_error {
            return;
        }

        interpreter.interpret(self, statements);
    }

    fn error(line: u32, message: &str) {
        Self::report(line, "", message);
    }

    fn error_token(token: &Token, message: &str) {
        if token.token_type == TokenType::Eof {
            Self::report(token.line, " at the end", message);
        } else {
            let at = format!(" at '{}'", token.lexeme);
            Self::report(token.line, at.as_str(), message);
        }
    }

    fn runtime_error(&mut self, error: &RuntimeError) {
        println!("[line {}]\n{}", error.token.line, error.message);
        self.had_runtime_error = true;
    }

    fn report(line: u32, at: &str, message: &str) {
        println!("[line {line}] Error {at}: {message}");
    }
}

fn main() {
    let args = std::env::args().collect();
    let mut lox: Lox = Lox {
        had_error: false,
        had_runtime_error: false,
    };
    lox.main(&args);
}
