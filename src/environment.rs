use std::collections::HashMap;

use crate::{value::Value, runtime_error::RuntimeError, token::Token};

pub struct Environment {
    values: HashMap<String, Value>
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new()
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }
    
    pub fn get(&self, name: Token) -> Result<Value, RuntimeError> {
        match self.values.get(&name.lexeme) {
            Some(x) => Ok(x.clone()),
            None => {
                let message = format!("Undefined variable '{}'.", name.lexeme);
                Err(RuntimeError {token: name, message: message})
            }

        }
    }

    pub fn assign(&mut self, name: Token, value: Value) -> Result<Value, RuntimeError>  {
        let result: Option<Value> = self.values.insert(name.lexeme.clone(), value);
        match result {
            Some(x) => Ok(x),
            None => Err(RuntimeError {token: name.clone(), message: format!("Undefined variable '{}'.", name.lexeme)})
        }
    }
}