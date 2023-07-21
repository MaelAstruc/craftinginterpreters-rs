use std::{collections::HashMap, rc::Rc, cell::RefCell};

use crate::{value::Value, runtime_error::RuntimeError, token::Token};

pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Environment {
        Environment {
            enclosing,
            values: HashMap::new()
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }
    
    pub fn get(&self, name: Token) -> Result<Value, RuntimeError> {
        if let Some(x) = self.values.get(&name.lexeme) {
            return Ok(x.clone())
        }
        if let Some(enclosing) = &self.enclosing {
            return enclosing.as_ref().borrow_mut().get(name)
        }
        let message = format!("Undefined variable '{}'.", name.lexeme);
        Err(RuntimeError {token: name, message})
    }

    pub fn assign(&mut self, name: Token, value: Value) -> Result<Value, RuntimeError>  {

        if let Some(x) = self.values.insert(name.lexeme.clone(), value.clone()) {
            return Ok(x)
        }    
        if let Some(enclosing) = &mut self.enclosing {
            return enclosing.as_ref().borrow_mut().assign(name, value)
        }
        let message = format!("Undefined variable '{}'.", name.lexeme);
        Err(RuntimeError {token: name, message})
    }
}