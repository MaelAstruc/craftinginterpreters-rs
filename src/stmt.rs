use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::environment::Environment;
use crate::interpreter::Interpreter;
use crate::make_expr; // from mod utils
use crate::expr::{Expr, ExprEnum};
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

pub struct If<T: Stmt, U: Stmt> {
    pub condition: ExprEnum,
    pub then_branch: T,
    pub else_branch: Option<U>
}

impl<T: Stmt, U: Stmt> Stmt for If<T, U> {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
        match self.condition.evaluate(environment.clone()) {
            Ok(x) if *Interpreter::check_bool(&x) => {
                self.then_branch.execute(environment)
            },
            Ok(_) => {
                match &self.else_branch {
                    Some(x) => x.execute(environment),
                    None => Ok(())
                }
            },
            Err(x) => return Err(x)
        }
    }
}

impl<T: Stmt, U: Stmt> fmt::Display for If<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.else_branch {
            Some(x) =>write!(f, "if ({}) \n then {{\n{}\n}} \n else {{\n{}\n}}", &self.condition, &self.then_branch, x),
            None => write!(f, "if ({}) \n then {{\n{}\n}}", &self.condition, &self.then_branch)
        }
        
    }
}

pub struct While<T: Stmt> {
    pub condition: ExprEnum,
    pub body: T
}

impl<T: Stmt> Stmt for While<T> {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
        loop {
            match self.condition.evaluate(environment.clone()) {
                Ok(x) => {
                    if *Interpreter::check_bool(&x) {
                        match self.body.execute(environment.clone()) {
                            Ok(_) => (),
                            Err(x) => return Err(x)
                        }
                    }
                    else {
                        return Ok(())
                    }
                },
                Err(x) => return Err(x)
            }
        }
    }
}

impl<T: Stmt> fmt::Display for While<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "while {} {}", &self.condition, &self.body)
    }
}
