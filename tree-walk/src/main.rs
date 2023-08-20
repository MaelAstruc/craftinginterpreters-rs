use std::cell::RefCell;
use std::fs;
use std::io::Write;
use std::process;
use std::rc::Rc;

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
use parser::ParserError;
use resolver::Resolver;
use runtime_error::RuntimeError;

use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::token_type::TokenType;

pub struct Lox {
    had_error: bool,
    had_runtime_error: bool,
    log: Log,
}

impl Lox {
    pub fn new(log: Log) -> Self {
        Lox {
            had_error: false,
            had_runtime_error: false,
            log,
        }
    }

    fn main(&mut self, args: &Vec<String>) {
        let mut interpreter = Interpreter::new(self.log.clone());
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
                Ok(_) if buffer.starts_with("run ") => {
                    self.run_file(
                        &buffer.replace("run ", "").trim_end().to_string(),
                        interpreter,
                    );
                    buffer = String::new();
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
        if let Err(x) = scanner.scan_tokens() {
            self.error(x.line, &x.message)
        }

        let mut parser: Parser = Parser::new(scanner.tokens);
        let (statements, errors) = parser.parse();

        for error in errors {
            self.parser_error(&error)
        }

        if self.had_error {
            return;
        }

        let mut resolver = Resolver::new(interpreter);
        if let Err(x) = resolver.resolve(&statements) {
            self.runtime_error(&x)
        }

        if self.had_error {
            return;
        }

        interpreter.interpret(self, statements);
    }

    fn error(&mut self, line: u32, message: &str) {
        self.report(line, "", message);
    }

    fn parser_error(&mut self, error: &ParserError) {
        let token = &error.token;
        let message = &error.message;
        match token.token_type {
            TokenType::Eof => self.report(token.line, " at the end", message),
            _ => self.report(
                token.line,
                format!("at '{}'", token.lexeme).as_str(),
                message,
            ),
        }
    }

    fn runtime_error(&mut self, error: &RuntimeError) {
        self.print(format!("[line {}]\n{}", error.token.line, error.message));
        self.had_runtime_error = true;
    }

    fn report(&mut self, line: u32, at: &str, message: &str) {
        self.print(format!("[line {line}] Error {at}: {message}"));
    }

    fn print(&mut self, message: String) {
        self.log.print(message);
    }
}

#[derive(Clone)]
pub struct Log {
    debugging: bool,
    pub events: Rc<RefCell<Vec<String>>>,
}

impl Log {
    pub fn new(debugging: bool) -> Self {
        Self {
            debugging,
            events: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn print(&self, event: String) {
        if self.debugging {
            self.events.as_ref().borrow_mut().push(event)
        } else {
            println!("{event}");
        }
    }
}

fn main() {
    let args = std::env::args().collect();
    let log = Log::new(false);
    let mut lox: Lox = Lox::new(log);
    lox.main(&args);
}

#[cfg(test)]
mod test {
    use std::io::Write;

    use glob::glob;
    use regex::Regex;

    use crate::{interpreter::Interpreter, Log, Lox};

    macro_rules! test_folder {
        ($folder:tt, $name:tt) => {
            #[test]
            fn $name() {
                test_folder("../test/$folder");
            }
        };
    }

    #[test]
    fn test_main() {
        test_folder("../test/");
    }

    test_folder!(assignment, test_assignment);
    test_folder!(benchmark, test_benchmark);
    test_folder!(block, test_block);
    test_folder!(bool, test_bool);
    test_folder!(call, test_call);
    test_folder!(class, test_class);
    test_folder!(closure, test_closure);
    test_folder!(comments, test_comments);
    test_folder!(constructor, test_constructor);
    test_folder!(expressions, test_expressions);
    test_folder!(field, test_field);
    test_folder!(for, test_for);
    test_folder!(function, test_function);
    test_folder!(if, test_if);
    test_folder!(inheritance, test_inheritance);
    test_folder!(limit, test_limit);
    test_folder!(logical_operator, test_logical_operator);
    test_folder!(method, test_method);
    test_folder!(nil, test_nil);
    test_folder!(number, test_number);
    test_folder!(operator, test_operator);
    test_folder!(print, test_print);
    test_folder!(regression, test_regression);
    test_folder!(return, test_return);
    test_folder!(scanning, test_scanning);
    test_folder!(string, test_string);
    test_folder!(super, test_super);
    test_folder!(this, test_this);
    test_folder!(variable, test_variable);
    test_folder!(while, test_while);

    fn test_folder(folder_path: &str) {
        for entry in glob(&format!("{folder_path}/*.lox")).unwrap() {
            let entry = entry.unwrap();
            let filepath = entry.as_path().to_str().unwrap().to_string();

            test_file(&filepath);
        }
        let _ = std::io::stdout().flush();
    }

    fn test_file(filepath: &str) {
        let code: String = std::fs::read_to_string(filepath).unwrap();

        let expected_output_pattern = Regex::new(r"// expect: ?(.*)").unwrap();
        let expected_error_pattern = Regex::new(r"// (Error.*)").unwrap();
        let error_line_pattern = Regex::new(r"// \[((java|c) )?line (\d+)\] (Error.*)").unwrap();
        let expected_runtime_error_pattern = Regex::new(r"// expect runtime error: (.+)").unwrap();
        let syntax_error_pattern = Regex::new(r"\[.*line (\d+)\] (Error.+)").unwrap();
        let stack_trace_pattern = Regex::new(r"\[line (\d+)\]").unwrap();
        let non_test_pattern = Regex::new(r"// nontest").unwrap();

        let mut expected_outcomes: Vec<(usize, String)> = Vec::new();

        // Get list of expected outcomes
        for (id, line) in code.lines().enumerate() {
            if expected_output_pattern.is_match(line) {
                expected_outcomes.push((
                    id,
                    Regex::new(r".*// expect: ")
                        .unwrap()
                        .replace(line, "")
                        .to_string(),
                ));
                continue;
            }
            if expected_error_pattern.is_match(line) {
                expected_outcomes.push((
                    id,
                    Regex::new(r".*//").unwrap().replace(line, "").to_string(),
                ));
                continue;
            }
            if error_line_pattern.is_match(line) {
                expected_outcomes.push((
                    id,
                    Regex::new(r".*//").unwrap().replace(line, "").to_string(),
                ));
                continue;
            }
            if expected_runtime_error_pattern.is_match(line) {
                expected_outcomes.push((
                    id,
                    Regex::new(r".*// expect runtime error: ")
                        .unwrap()
                        .replace(line, "")
                        .to_string(),
                ));
                continue;
            }
            if syntax_error_pattern.is_match(line) {
                expected_outcomes.push((id, line.to_string()));
                continue;
            }
            if stack_trace_pattern.is_match(line) {
                expected_outcomes.push((id, line.to_string()));
                continue;
            }
            if non_test_pattern.is_match(line) {
                return;
            }
        }

        let log = Log::new(true);
        let mut lox = Lox::new(log.clone());

        let mut interpreter = Interpreter::new(log.clone());
        lox.run(code, &mut interpreter);

        assert_eq!(expected_outcomes.len(), log.events.as_ref().borrow().len());

        let len = expected_outcomes.len();

        for i in 0..len {
            let (line, expected) = expected_outcomes.get(i).unwrap();
            let expected = expected.trim();
            let found = log.events.as_ref().borrow();
            let found = found.get(i).unwrap().trim();
            assert_eq!(
                expected, found,
                "\n => Test file '{filepath}' line {line}: expect '{expected}', '{found}'"
            )
        }
    }
}
