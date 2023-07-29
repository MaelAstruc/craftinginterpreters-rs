use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::environment::Environment;
use crate::interpreter::Interpreter;
use crate::runtime_error::LoxError;
use crate::stmt;
use crate::stmt::Stmt;
use crate::value::Value;

#[derive(Clone)]
pub enum LoxCallable {
    LoxFunction(Rc<LoxFunction>),
    LoxClock(Rc<LoxClock>),
}

pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>)
        -> Result<Value, LoxError>;
}

impl Callable for LoxCallable {
    fn arity(&self) -> usize {
        match self {
            LoxCallable::LoxFunction(x) => x.arity(),
            LoxCallable::LoxClock(x) => x.arity(),
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, LoxError> {
        match self {
            LoxCallable::LoxFunction(x) => x.call(interpreter, arguments),
            LoxCallable::LoxClock(x) => x.call(interpreter, arguments),
        }
    }
}

impl fmt::Display for LoxCallable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxCallable::LoxFunction(x) => write!(f, "{}", x.to_owned()),
            LoxCallable::LoxClock(x) => write!(f, "{}", x.to_owned()),
        }
    }
}

pub struct LoxFunction {
    pub closure: Rc<RefCell<Environment>>,
    pub declaration: stmt::Function,
}

impl LoxFunction {
    pub fn new(&self, declaration: stmt::Function, closure: Rc<RefCell<Environment>>) -> Self {
        LoxFunction {
            closure,
            declaration,
        }
    }

    pub fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, LoxError> {
        interpreter.other_environment = Some(Rc::new(RefCell::new(Environment::new(Some(
            interpreter.environment.clone(),
        )))));
        for (i, param) in self.declaration.params.iter().enumerate() {
            let arg = arguments.get(i).unwrap();
            match &interpreter.other_environment {
                Some(x) => x
                    .as_ref()
                    .borrow_mut()
                    .define(param.lexeme.to_string(), arg.clone()),
                None => panic!("Impossible, we defined it above."),
            }
        }
        let result = self.declaration.body.execute(interpreter);
        interpreter.other_environment = None;
        match result {
            Ok(x) => Ok(x),
            Err(x) => match x {
                LoxError::RuntimeError(_) => Err(x),
                LoxError::Return(y) => Ok(y.value),
            },
        }
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

    pub fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<Value>,
    ) -> Result<Value, LoxError> {
        match SystemTime::now().duration_since(UNIX_EPOCH) {
            Ok(x) => Ok(Value::Number(x.as_secs_f32())),
            Err(x) => panic!("{}", x),
        }
    }
}

impl fmt::Display for LoxClock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native fn>")
    }
}
