use crate::{token::Token, value::Value};

pub enum LoxError {
    RuntimeError(RuntimeError),
    Return(Return)
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
