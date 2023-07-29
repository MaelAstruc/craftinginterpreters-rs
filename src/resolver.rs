use std::collections::HashMap;

use crate::{interpreter::Interpreter, token::Token, stmt::{Function, Stmt, self}, Lox};

#[derive(Clone)]
pub enum FunctionType {
    NONE,
    FUNCTION
}

pub struct Resolver<'a> {
    pub interpreter: &'a mut Interpreter,
    pub scopes: Vec<HashMap<String, bool>>,
    pub current_function: FunctionType,
}

impl Resolver<'_> {
    pub fn new(interpreter: &mut Interpreter) -> Resolver {
        Resolver { interpreter, scopes: Vec::new(), current_function: FunctionType::NONE }
    }

    pub fn resolve(&mut self, statements: &Vec<Box<stmt::StmtEnum>>) {
        for statement in statements {
            statement.resolve(self)
        }
    }

    pub fn begin_scope(&mut self) {
        let scope: HashMap<String, bool> = HashMap::new();
        self.scopes.push(scope);
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare(&mut self, name: &Token) {
        match self.scopes.last_mut() {
            Some(x) => {
                if x.contains_key(&name.lexeme) {
                    Lox::error_token(name, "Already a variable with this name in this scope.");
                }
                x.insert(name.lexeme.clone(), false)
            },
            None => None
        };
    }

    pub fn define(&mut self, name: &Token) {
        match self.scopes.last_mut()  {
            Some(x) => {
                x.insert(name.lexeme.clone(), true)
            },
            None => None
        };
    }

    pub fn resolve_local(&mut self, expr: usize, name: Token) {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                self.interpreter.resolve(expr, self.scopes.len() - 1 - i);
                return;
            }
        }
    }
    
    pub fn resolve_function(&mut self, function: &Function, function_type: FunctionType) {
        let enclosing_function = self.current_function.clone();
        self.current_function = function_type;
        self.begin_scope();
        for param in &function.params {
            self.declare(param);
            self.define(param);
        }
        function.body.resolve(self);
        self.end_scope();
        self.current_function = enclosing_function;
    }
    
}
