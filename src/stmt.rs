use std::fmt;

use crate::interpreter::Interpreter;
use crate::make_expr; // from mod utils
use crate::expr::Expr;
use crate::runtime_error::RuntimeError;
use crate::token::Token;

pub trait Stmt: std::fmt::Display {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError>;
}

impl Stmt for Box<dyn Stmt> {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError> {
        (**self).execute(interpreter)
    }
}

make_expr!(Expression<T: Expr>, expression: T);

impl<T: Expr> Stmt for Expression<T> {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError> {
        match self.expression.evaluate(&mut interpreter.environment) {
            Ok(_) => Ok(()),
            Err(x) => Err(x)
        }
    } 
}

impl<T: Expr + fmt::Display> fmt::Display for Expression<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.expression)
    }
}

make_expr!(Print<T: Expr>, expression: T);

impl<T: Expr> Stmt for Print<T> {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError> {
        match self.expression.evaluate(&mut interpreter.environment) {
            Ok(x) => Ok(println!("{}", x)),
            Err(x) => Err(x)
        }
    }
}

impl<T: Expr + fmt::Display> fmt::Display for Print<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.expression)
    }
}

make_expr!(Var<T: Expr>, name: Token, initializer: T);

impl<T: Expr> Stmt for Var<T> {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError> {
        match self.initializer.evaluate(&mut interpreter.environment) {
            Ok(x) => Ok(interpreter.environment.define(self.name.lexeme.clone(), x)),
            Err(x) => Err(x)
        }
    }
}

impl<T: Expr + fmt::Display> fmt::Display for Var<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", &self.name, &self.initializer)
    }
}