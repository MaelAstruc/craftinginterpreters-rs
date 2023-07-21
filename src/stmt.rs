use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::callable::{LoxFunction, LoxCallable};
use crate::environment::Environment;
use crate::interpreter::Interpreter;
use crate::expr::{Expr, ExprEnum};
use crate::runtime_error::RuntimeError;
use crate::token::Token;
use crate::value::Value;

#[derive(Clone)]
pub enum StmtEnum {
    Expression(Box<Expression>),
    Print(Box<Print>),
    Var(Box<Var>),
    Block(Box<Block>),
    Function(Box<Function>),
    If(Box<If>),
    While(Box<While>)
}

impl Stmt for StmtEnum {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        match self {
            StmtEnum::Block(x) => x.execute(environment),
            StmtEnum::Expression(x) => x.execute(environment),
            StmtEnum::Function(x) => x.execute(environment),
            StmtEnum::If(x) => x.execute(environment),
            StmtEnum::Print(x) => x.execute(environment),
            StmtEnum::Var(x) => x.execute(environment),
            StmtEnum::While(x) => x.execute(environment)
        }
    }
}

impl fmt::Display for StmtEnum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StmtEnum::Block(x) => write!(f, "{}", x),
            StmtEnum::Expression(x) => write!(f, "{}", x),
            StmtEnum::Function(x) => write!(f, "{}", x),
            StmtEnum::If(x) => write!(f, "{}", x),
            StmtEnum::Print(x) => write!(f, "{}", x),
            StmtEnum::Var(x) => write!(f, "{}", x),
            StmtEnum::While(x) => write!(f, "{}", x)
        }
    }
}

pub trait Stmt: std::fmt::Display {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError>;
}

#[derive(Clone)]
pub struct Expression {
    pub expression: ExprEnum
}

impl Stmt for Expression {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        match self.expression.evaluate(environment) {
            Ok(x) => Ok(x),
            Err(x) => Err(x)
        }
    } 
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.expression)
    }
}

#[derive(Clone)]
pub struct Print {
    pub expression: ExprEnum
}

impl Stmt for Print {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        match self.expression.evaluate(environment) {
            Ok(x) => {
                println!("{}", x);
                Ok(x)
            },
            Err(x) => Err(x)
        }
    }
}

impl fmt::Display for Print {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "print {}", &self.expression)
    }
}

#[derive(Clone)]
pub struct Var {
    pub name: Token,
    pub initializer: ExprEnum
}

impl Stmt for Var {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        match self.initializer.evaluate(environment.clone()) {
            Ok(x) => {
                environment.as_ref().borrow_mut().define(self.name.lexeme.clone(), x.clone());
                Ok(x)
            },
            Err(x) => return Err(x)
        }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", &self.name, &self.initializer)
    }
}

#[derive(Clone)]
pub struct Block {
    pub statements: Vec<Box<StmtEnum>>
}

impl Stmt for Block {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let block_env = Environment::new(Some(environment));
        let block_env_ref = Rc::new(RefCell::new(block_env));
        let mut value = Value::Nil;
        for statement in &self.statements {
            value = statement.execute(block_env_ref.clone())?;
        }
        Ok(value)
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut message = "\n".to_string();
        for statement in &self.statements {
            message += "\t";
            message += statement.to_string().as_ref();
            message += "\n";
        }
        write!(f, "{{ {} }}", message)
    }
}

#[derive(Clone)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Block
}

impl Stmt for Function {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let function = Value::Callable(LoxCallable::LoxFunction(Rc::new(LoxFunction{ declaration: self.clone() })));
        environment.as_ref().borrow_mut().define(self.name.lexeme.clone(), function);
        return Ok(Value::Nil);
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", &self.name, &self.body)
    }
}


#[derive(Clone)]
pub struct If {
    pub condition: ExprEnum,
    pub then_branch: StmtEnum,
    pub else_branch: Option<StmtEnum>
}

impl Stmt for If {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let mut value = Value::Nil;
        match self.condition.evaluate(environment.clone()) {
            Ok(x) if *Interpreter::check_bool(&x) => {
                value = self.then_branch.execute(environment)?;
            },
            Ok(_) => {
                match &self.else_branch {
                    Some(x) => value = x.execute(environment)?,
                    None => ()
                }
            },
            Err(x) => return Err(x)
        }
        Ok(value)
    }
}

impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.else_branch {
            Some(x) =>write!(f, "if ({}) \n then {{\n{}\n}} \n else {{\n{}\n}}", &self.condition, &self.then_branch, x),
            None => write!(f, "if ({}) \n then {{\n{}\n}}", &self.condition, &self.then_branch)
        }
        
    }
}

#[derive(Clone)]
pub struct While {
    pub condition: ExprEnum,
    pub body: StmtEnum
}

impl Stmt for While {
    fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
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
                        break
                    }
                },
                Err(x) => return Err(x)
            }
        }
        Ok(Value::Nil)
    }
}

impl fmt::Display for While {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "while {} {}", &self.condition, &self.body)
    }
}
