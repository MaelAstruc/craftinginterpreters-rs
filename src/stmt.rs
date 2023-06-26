use std::fmt;

use crate::make_expr; // from mod utils
use crate::expr::Expr;
use crate::runtime_error::RuntimeError;

pub trait Stmt: std::fmt::Display {
    fn execute(&self) -> Result<(), RuntimeError>;
}

impl Stmt for Box<dyn Stmt> {
    fn execute(&self) -> Result<(), RuntimeError> {
        (**self).execute()
    }
}

make_expr!(Expression<T: Expr>, expression: T);

impl<T: Expr> Stmt for Expression<T> {
    fn execute(&self) -> Result<(), RuntimeError> {
        match self.expression.evaluate() {
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
    fn execute(&self) -> Result<(), RuntimeError> {
        match self.expression.evaluate() {
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
