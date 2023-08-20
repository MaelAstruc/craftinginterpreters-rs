use std::collections::HashMap;

use crate::{
    interpreter::Interpreter,
    runtime_error::RuntimeError,
    stmt::{self, Function, Stmt},
    token::Token,
};

#[derive(Clone)]
pub enum FunctionType {
    NONE,
    INITIALIZER,
    FUNCTION,
    METHOD,
}

#[derive(Clone)]
pub enum ClassType {
    NONE,
    CLASS,
    SUBCLASS,
}

pub struct Resolver<'a> {
    pub interpreter: &'a mut Interpreter,
    pub scopes: Vec<HashMap<String, bool>>,
    pub current_function: FunctionType,
    pub current_class: ClassType,
}

impl Resolver<'_> {
    pub fn new(interpreter: &mut Interpreter) -> Resolver {
        Resolver {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::NONE,
            current_class: ClassType::NONE,
        }
    }

    pub fn resolve(&mut self, statements: &Vec<Box<stmt::StmtEnum>>) -> Result<(), RuntimeError> {
        for statement in statements {
            statement.resolve(self)?
        }
        Ok(())
    }

    pub fn begin_scope(&mut self) {
        let scope: HashMap<String, bool> = HashMap::new();
        self.scopes.push(scope);
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare(&mut self, name: &Token) -> Result<(), RuntimeError> {
        match self.scopes.last_mut() {
            Some(x) => {
                if x.contains_key(&name.lexeme) {
                    return Err(RuntimeError {
                        token: name.clone(),
                        message: "Already a variable with this name in this scope.".into(),
                    });
                }
                x.insert(name.lexeme.clone(), false)
            }
            None => None,
        };
        Ok(())
    }

    pub fn define(&mut self, name: &Token) {
        match self.scopes.last_mut() {
            Some(x) => x.insert(name.lexeme.clone(), true),
            None => None,
        };
    }

    pub fn resolve_local(&mut self, expr: usize, name: &Token) -> Result<(), RuntimeError> {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                return self.interpreter.resolve(expr, i);
            }
        }
        Ok(())
    }

    pub fn resolve_function(
        &mut self,
        function: &Function,
        function_type: FunctionType,
    ) -> Result<(), RuntimeError> {
        let enclosing_function = self.current_function.clone();
        self.current_function = function_type;
        self.begin_scope();
        for param in &function.params {
            self.declare(param)?;
            self.define(param);
        }
        for stmt in &function.body {
            stmt.resolve(self)?;
        }
        self.end_scope();
        self.current_function = enclosing_function;
        Ok(())
    }
}
