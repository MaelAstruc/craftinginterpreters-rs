use std::cell::RefCell;
use std::rc::Rc;

use crate::callable::{LoxCallable, LoxClock};
use crate::environment::Environment;
use crate::runtime_error::RuntimeError;
use crate::stmt::{Stmt, StmtEnum};
use crate::token::Token;
use crate::value::Value;
use crate::Lox;

pub struct Interpreter {
    pub globals: Rc<RefCell<Environment>>,
    pub environment: Rc<RefCell<Environment>>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut globals = Environment::new(None);

        globals.define(
            "clock".into(),
            Value::Callable(LoxCallable::LoxClock(Rc::new(LoxClock {}))),
        );

        let globals: Rc<RefCell<Environment>> = Rc::new(RefCell::new(globals));
        Interpreter {
            globals: globals.clone(),
            environment: globals,
        }
    }

    pub fn interpret(&mut self, lox: &mut Lox, statements: Vec<Box<StmtEnum>>) {
        for statement in statements {
            match statement.execute(self.environment.clone()) {
                Ok(_) => (),
                Err(x) => lox.runtime_error(x),
            }
        }
    }

    pub fn check_operand(token: Token, message: &str, value: Value) -> RuntimeError {
        let message = format!("expected {message}, found {value}");
        RuntimeError { token, message }
    }

    pub fn check_operands(token: Token, message: &str, left: Value, right: Value) -> RuntimeError {
        panic!("{} {}, found {} and {}", message, token, left, right)
    }

    pub fn check_bool(value: &Value) -> &bool {
        match value {
            Value::Bool(x) => x,
            _ => &true,
        }
    }

    pub fn check_equal(left: Value, right: Value) -> bool {
        match (left, right) {
            (Value::Number(x), Value::Number(y)) => x == y,
            (Value::String(x), Value::String(y)) => x == y,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            (Value::Nil, Value::Nil) => true,
            (Value::Callable(_), Value::Callable(_)) => false,
            (Value::Bool(_), _) => false,
            (Value::Number(_), _) => false,
            (Value::String(_), _) => false,
            (Value::Nil, _) => false,
            (Value::Callable(_), _) => false,
        }
    }
}

/*
#[cfg(test)]
mod tests_evaluate {
    use std::f32::INFINITY;

    use crate::value::Value;
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    fn check_evaluate(code: &str, expected: Value) {
        let mut scanner : Scanner = Scanner::new(code.into());
        scanner.scan_tokens();

        let mut parser: Parser = Parser::new(scanner.tokens);

        let result: Value = match parser.parse().evaluate() {
            Ok(x) => x,
            Err(x) => panic!("{}", x)
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn evaluate_nil() {
        let code: &str = "nil";
        let mut scanner : Scanner = Scanner::new(code.into());
        scanner.scan_tokens();

        let mut parser: Parser = Parser::new(scanner.tokens);

        let result: Value = match parser.parse().evaluate() {
            Ok(x) => x,
            Err(x) => panic!("{}", x)
        };

        let expected: Value = Value::Nil;

        assert_eq!(result, expected);
    }

    #[test]
    fn evaluate_primary() {
        check_evaluate("true", Value::Bool(true));
        check_evaluate("false", Value::Bool(false));
        check_evaluate("1", Value::Number(1.0));
        check_evaluate("42", Value::Number(42.0));
        check_evaluate("1.37", Value::Number(1.37));
        check_evaluate("\"Hello World !\"", Value::String("Hello World !".into()));
        check_evaluate("\"\t Hello \r\n World !\"", Value::String("\t Hello \r\n World !".into()));
        check_evaluate("nil", Value::Nil)
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
        check_evaluate("(-1)", Value::Number(-1.0));
        check_evaluate("((-1))", Value::Number(-1.0))
    }

    #[test]
    fn evaluate_unary() {
        check_evaluate("!true", Value::Bool(false));
        check_evaluate("!!true", Value::Bool(true));
        check_evaluate("!!!true", Value::Bool(false));
        check_evaluate("-1", Value::Number(-1.0));
        check_evaluate("--1", Value::Number(1.0));
    }

    #[test]
    fn evaluate_binary() {
        check_evaluate("5-3", Value::Number(2.0));
        check_evaluate("5 - 3", Value::Number(2.0));
        check_evaluate("5--3", Value::Number(8.0));
        check_evaluate("5+3", Value::Number(8.0));
        check_evaluate("\"Hello\"+ \" \" + \"World\"", Value::String("Hello World".into()));
        check_evaluate("3*5", Value::Number(15.0));
        check_evaluate("3*0", Value::Number(0.0));
        check_evaluate("3/5", Value::Number(0.6));
        check_evaluate("3/0", Value::Number(INFINITY));
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
} */
