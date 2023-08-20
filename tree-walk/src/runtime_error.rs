use crate::{token::Token, value::Value};

pub enum LoxError {
    CallError(CallError),
    RuntimeError(RuntimeError),
    Return(Return),
}

impl LoxError {
    pub fn handle_call(result: Result<Value, LoxError>, token: &Token) -> Result<Value, LoxError> {
        match result {
            Err(LoxError::CallError(x)) => Err(x.to_runtime(token)),
            _ => result
        }
    }
}

pub struct CallError {
    message: String
}

impl CallError {
    pub fn new(message: String) -> Self {
        CallError{ message }
    }

    pub fn to_runtime (&self, token: &Token) -> LoxError {
        LoxError::RuntimeError(RuntimeError{ token: token.clone(), message: self.message.clone() })
    }
}

pub struct RuntimeError {
    pub token: Token,
    pub message: String,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Run time error")
    }
}

pub struct Return {
    pub value: Value,
}

impl Return {
    pub fn new(&self, value: Value) -> Self {
        Self { value }
    }
}

impl std::fmt::Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
