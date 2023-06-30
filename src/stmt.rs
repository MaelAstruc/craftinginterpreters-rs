use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::environment::Environment;
use crate::make_expr; // from mod utils
use crate::expr::Expr;
use crate::runtime_error::RuntimeError;
use crate::token::Token;

pub trait Stmt: std::fmt::Display {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<(), RuntimeError>;
}

impl Stmt for Box<dyn Stmt> {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
        (**self).execute(environment)
    }
}

make_expr!(Expression<T: Expr>, expression: T);

impl<T: Expr> Stmt for Expression<T> {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
        match self.expression.evaluate(environment) {
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
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
        match self.expression.evaluate(environment) {
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
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
        match self.initializer.evaluate(environment.clone()) {
            Ok(x) => Ok(environment.as_ref().borrow_mut().define(self.name.lexeme.clone(), x)),
            Err(x) => Err(x)
        }
    }
}

impl<T: Expr + fmt::Display> fmt::Display for Var<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", &self.name, &self.initializer)
    }
}

make_expr!(Block, statements: Vec<Box<dyn Stmt>>);

impl Stmt for Block {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
        let block_env = Environment::new(Some(environment));
        let block_env_ref = Rc::new(RefCell::new(block_env));
        for statement in &self.statements {
            match statement.execute(block_env_ref.clone()) {
                Ok(_) => (),
                Err(x) => return Err(x)
            }
        }
        Ok(())
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut message = "".to_string();
        for statement in &self.statements {
            message += statement.to_string().as_ref();
        }
        write!(f, "{{ \n {} \n }}", message)
    }
}