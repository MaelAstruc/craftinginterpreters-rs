use std::collections::HashMap;
use std::rc::Rc;
use std::time::SystemTime;

use crate::callable::{LoxCallable, LoxClock};
use crate::environment::{EnvRef, Environment};
use crate::runtime_error::{LoxError, RuntimeError};
use crate::stmt::{Stmt, StmtEnum};
use crate::token::Token;
use crate::value::Value;
use crate::{Log, Lox};

pub struct Interpreter {
    pub log: Log,
    pub environment: EnvRef,
    pub other_environment: Option<EnvRef>,
    pub globals: EnvRef,
    pub locals: HashMap<usize, usize>,
    pub begin_time: SystemTime,
}

impl Interpreter {
    pub fn new(log: Log) -> Interpreter {
        let mut globals = Environment::new(None);

        globals.define(
            "clock".into(),
            Value::Callable(LoxCallable::LoxClock(Rc::new(LoxClock {}))),
        );

        let globals: EnvRef = EnvRef::new(globals);

        Interpreter {
            log,
            environment: globals.clone(),
            other_environment: None,
            globals,
            locals: HashMap::new(),
            begin_time: SystemTime::now(),
        }
    }

    pub fn interpret(&mut self, lox: &mut Lox, statements: Vec<Box<StmtEnum>>) {
        for statement in statements {
            match statement.execute(self) {
                Ok(_) => (),
                Err(x) => match x {
                    LoxError::RuntimeError(y) => lox.runtime_error(&y),
                    LoxError::CallError(_) => unreachable!("Should be transformed in RuntimeError by Call | Super | This | Var expression"),
                    LoxError::Return(_) => unreachable!("What are you doing here ? The resolver shouldn't let return statement outside of functions"),
                },
            }
        }
    }

    pub fn resolve(&mut self, expr: usize, depth: usize) -> Result<(), RuntimeError> {
        self.locals.insert(expr, depth);
        Ok(())
    }

    pub fn print(&mut self, message: String) {
        self.log.print(message);
    }

    pub fn look_up_var(&mut self, name: &Token, expr_id: &usize) -> Result<Value, LoxError> {
        let distance = self.locals.get(expr_id);
        match distance {
            Some(x) => self.environment.deref_mut().get_at(*x, &name.lexeme),
            None => self.globals.deref_mut().get(name),
        }
    }

    pub fn check_operand(token: Token, message: &str, value: &Value) -> RuntimeError {
        let message = format!("expected {message}, found {value}");
        RuntimeError { token, message }
    }

    pub fn check_operands(
        token: &Token,
        message: &str,
        left: &Value,
        right: &Value,
    ) -> RuntimeError {
        RuntimeError {
            token: token.clone(),
            message: format!("{message}, found {left} and {right}"),
        }
    }

    pub fn check_bool(value: &Value) -> &bool {
        match value {
            Value::Bool(x) => x,
            _ => &true,
        }
    }

    pub fn check_equal(left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Number(x), Value::Number(y)) => (x - y).abs() < f32::EPSILON,
            (Value::String(x), Value::String(y)) => x == y,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            (Value::Nil, Value::Nil) => true,
            (Value::Callable(_), Value::Callable(_)) => false,
            (Value::LoxInstance(_), Value::LoxInstance(_)) => false,
            (_, _) => false,
        }
    }
}
