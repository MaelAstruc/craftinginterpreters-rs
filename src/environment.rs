use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    runtime_error::{LoxError, RuntimeError},
    token::Token,
    value::Value,
};

pub struct Environment {
    pub enclosing: Option<EnvRef>,
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new(enclosing: Option<EnvRef>) -> Environment {
        Environment {
            enclosing,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn ancestor(&self, distance: usize) -> EnvRef {
        let mut environment = self.enclosing.clone().expect("No enclosing environment.");
        for _ in 0..(distance - 1) {
            let next = environment
                .deref_mut()
                .enclosing
                .clone()
                .expect("No enclosing environment.");
            environment = next;
        }
        environment
    }

    pub fn get_at(&self, distance: usize, name: String) -> Result<Value, LoxError> {
        if distance == 0 {
            match self.values.get(&name) {
                Some(x) => return Ok(x.clone()),
                None => panic!("Cannot find value"),
            }
        }

        match self.ancestor(distance).deref_mut().values.get(&name) {
            Some(x) => Ok(x.clone()),
            None => panic!("Cannot find value"),
        }
    }

    pub fn get(&self, name: Token) -> Result<Value, LoxError> {
        if let Some(x) = self.values.get(&name.lexeme) {
            return Ok(x.clone());
        }
        if let Some(enclosing) = &self.enclosing {
            return enclosing.deref_mut().get(name);
        }
        let message = format!("Undefined variable '{}'.", name.lexeme);
        Err(LoxError::RuntimeError(RuntimeError {
            token: name,
            message,
        }))
    }

    pub fn assign_at(
        &mut self,
        distance: usize,
        name: Token,
        value: Value,
    ) -> Result<Value, LoxError> {
        if distance == 0 {
            match self.values.insert(name.lexeme, value) {
                Some(x) => return Ok(x),
                None => panic!("Cannot insert value"),
            }
        }

        match self
            .ancestor(distance)
            .deref_mut()
            .values
            .insert(name.lexeme, value)
        {
            Some(x) => Ok(x),
            None => panic!("Cannot insert value"),
        }
    }

    pub fn assign(&mut self, name: Token, value: Value) -> Result<Value, LoxError> {
        if let Some(x) = self.values.insert(name.lexeme.clone(), value.clone()) {
            return Ok(x);
        }
        if let Some(enclosing) = &mut self.enclosing {
            return enclosing.deref_mut().assign(name, value);
        }
        let message = format!("Undefined variable '{}'.", name.lexeme);
        Err(LoxError::RuntimeError(RuntimeError {
            token: name,
            message,
        }))
    }
}

#[derive(Clone)]
pub struct EnvRef {
    environment: Rc<RefCell<Environment>>,
}

impl EnvRef {
    pub fn new(environment: Environment) -> EnvRef {
        EnvRef {
            environment: Rc::new(RefCell::new(environment)),
        }
    }

    pub fn deref_mut(&self) -> std::cell::RefMut<'_, Environment> {
        self.environment.as_ref().borrow_mut()
    }
}
