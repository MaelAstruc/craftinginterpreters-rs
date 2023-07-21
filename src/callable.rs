use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::environment::Environment;
use crate::runtime_error::RuntimeError;
use crate::stmt::Stmt;
use crate::stmt;
use crate::value::Value;

#[derive(Clone)]
pub enum LoxCallable {
    LoxFunction(Rc<LoxFunction>),
    LoxClock(Rc<LoxClock>)
}

pub trait Callable {
  fn arity(&self) -> usize;
  fn call(&self, environment: Rc<RefCell<Environment>>, arguments: Vec<Value>) -> Result<Value, RuntimeError>;
}

impl Callable for LoxCallable {
  fn arity(&self) -> usize {
    match self {
      LoxCallable::LoxFunction(x) => x.arity(),
      LoxCallable::LoxClock(x) => x.arity()
    }
  }

  fn call(&self, environment: Rc<RefCell<Environment>>, arguments: Vec<Value>) -> Result<Value, RuntimeError> {
    match self {
      LoxCallable::LoxFunction(x) => x.call(environment, arguments),
      LoxCallable::LoxClock(x) => x.call(environment, arguments)
    }
  }
}

impl fmt::Display for LoxCallable {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      LoxCallable::LoxFunction(x) => write!(f, "{}", x.to_owned()),
      LoxCallable::LoxClock(x) => write!(f, "{}", x.to_owned())
    }
  }
}

pub struct LoxFunction {
  pub declaration: stmt::Function
}

impl LoxFunction {
  pub fn new(&self, declaration: stmt::Function) -> Self {
    LoxFunction{
      declaration
    }
  }

  pub fn arity(&self) -> usize {
    self.declaration.params.len()
  }

  pub fn call(&self, environment: Rc<RefCell<Environment>>, arguments: Vec<Value>) -> Result<Value, RuntimeError> {
    let mut local = Environment::new(Some(environment));
    for (i, param) in self.declaration.params.iter().enumerate() {
      let name = &param.lexeme;
      let arg = arguments.get(i).unwrap();
      local.define(name.to_string(), arg.clone());
    }
    self.declaration.body.execute(Rc::new(RefCell::new(local)))
  }
}

impl fmt::Display for LoxFunction {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "<fn {}>", self.declaration.name.lexeme)
  }
}

pub struct LoxClock;

impl LoxClock {
  pub fn new(&self) -> Self {
    LoxClock
  }

  pub fn arity(&self) -> usize {
    0
  }

  pub fn call(&self, _environment: Rc<RefCell<Environment>>, _arguments: Vec<Value>) -> Result<Value, RuntimeError> {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
      Ok(x) => Ok(Value::Number(x.as_secs_f32())),
      Err(x) => panic!("{}", x)
  }
  }
}

impl fmt::Display for LoxClock {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "<native fn>")
  }
}
