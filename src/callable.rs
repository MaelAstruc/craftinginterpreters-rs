use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::environment::{EnvRef, Environment};
use crate::interpreter::Interpreter;
use crate::runtime_error::{LoxError, RuntimeError};
use crate::stmt;
use crate::stmt::Stmt;
use crate::token::Token;
use crate::value::Value;

#[derive(Clone)]
pub enum LoxCallable {
    LoxFunction(Rc<LoxFunction>),
    LoxClass(Rc<LoxClass>),
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
            LoxCallable::LoxClass(x) => x.arity(),
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
            LoxCallable::LoxClass(x) => x.call(interpreter, arguments),
            LoxCallable::LoxClock(x) => x.call(interpreter, arguments),
        }
    }
}

impl fmt::Display for LoxCallable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxCallable::LoxFunction(x) => write!(f, "{}", x.to_owned()),
            LoxCallable::LoxClass(x) => write!(f, "{}", x.to_owned()),
            LoxCallable::LoxClock(x) => write!(f, "{}", x.to_owned()),
        }
    }
}

#[derive(Clone)]
pub struct LoxFunction {
    pub closure: EnvRef,
    pub declaration: stmt::Function,
    pub is_initializer: bool,
}

impl LoxFunction {
    pub fn new(&self, declaration: stmt::Function, closure: EnvRef, is_initializer: bool) -> Self {
        LoxFunction {
            closure,
            declaration,
            is_initializer,
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
        let environment = EnvRef::new(Environment::new(Some(self.closure.clone())));

        for (i, param) in self.declaration.params.iter().enumerate() {
            let arg = arguments.get(i).unwrap();
            environment
                .deref_mut()
                .define(param.lexeme.to_string(), arg.clone());
        }

        let previous = interpreter.environment.clone();
        interpreter.environment = environment;
        let mut result = Ok(Value::Nil);
        for stmt in &self.declaration.body {
            result = stmt.execute(interpreter);
            if result.is_err() {
                break;
            }
        }
        interpreter.environment = previous;

        match result {
            Ok(_) => println!("no error"),
            Err(x) => match x {
                LoxError::RuntimeError(_) if self.is_initializer => {
                    return self.closure.deref_mut().get_at(0, "this")
                }
                LoxError::RuntimeError(_) => return Err(x),
                LoxError::Return(y) => return Ok(y.value),
            },
        };

        if self.is_initializer {
            return self.closure.deref_mut().get_at(0, "this");
        }

        Ok(Value::Nil)
    }

    pub fn bind(&self, instance: InstanceRef) -> LoxFunction {
        let mut environment = crate::environment::Environment::new(Some(self.closure.clone()));
        environment.define("this".into(), Value::LoxInstance(instance));
        LoxFunction {
            closure: EnvRef::new(environment),
            declaration: self.declaration.clone(),
            is_initializer: self.is_initializer,
        }
    }
}

impl fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fn {}>", self.declaration.name.lexeme)
    }
}

#[derive(Clone)]
pub struct LoxClass {
    pub name: String,
    pub superclass: Option<Rc<LoxClass>>,
    pub methods: HashMap<String, LoxFunction>,
}

impl LoxClass {
    pub fn new(
        name: String,
        superclass: Option<Rc<LoxClass>>,
        methods: HashMap<String, LoxFunction>,
    ) -> Self {
        LoxClass {
            name,
            superclass,
            methods,
        }
    }

    pub fn arity(&self) -> usize {
        match self.find_method("init".into()) {
            Some(x) => x.arity(),
            None => 0,
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, LoxError> {
        let instance = InstanceRef::new(LoxInstance::new(Rc::new((*self).clone())));
        if let Some(x) = self.find_method("init".into()) {
            x.bind(instance.clone()).call(interpreter, arguments)?;
        }
        Ok(Value::LoxInstance(instance))
    }

    pub fn find_method(&self, name: String) -> Option<&LoxFunction> {
        if let Some(x) = self.methods.get(&name) {
            return Some(x);
        }

        if let Some(x) = &self.superclass {
            return x.find_method(name);
        }

        None
    }
}

impl fmt::Display for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone)]
pub struct LoxInstance {
    pub klass: Rc<LoxClass>,
    pub fields: HashMap<String, Value>,
}

impl LoxInstance {
    pub fn new(klass: Rc<LoxClass>) -> Self {
        LoxInstance {
            klass,
            fields: HashMap::new(),
        }
    }
}

impl fmt::Display for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} instance", self.klass.name)
    }
}

#[derive(Clone)]
pub struct InstanceRef {
    pub instance: Rc<RefCell<LoxInstance>>,
}

impl InstanceRef {
    pub fn new(instance: LoxInstance) -> InstanceRef {
        InstanceRef {
            instance: Rc::new(RefCell::new(instance)),
        }
    }

    pub fn deref_mut(&self) -> std::cell::RefMut<'_, LoxInstance> {
        self.instance.as_ref().borrow_mut()
    }

    pub fn get(&self, name: Token) -> Result<Value, LoxError> {
        if let Some(x) = self.deref_mut().fields.get(&name.lexeme) {
            return Ok(x.clone());
        };
        if let Some(x) = self.deref_mut().klass.find_method(name.lexeme.clone()) {
            return Ok(Value::Callable(LoxCallable::LoxFunction(Rc::new(
                x.bind(self.clone()),
            ))));
        };

        Err(LoxError::RuntimeError(RuntimeError {
            token: name.clone(),
            message: format!("Undefined property '{}'.", name.lexeme),
        }))
    }

    pub fn set(&mut self, name: Token, value: Value) {
        self.deref_mut().fields.insert(name.lexeme, value);
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
        interpreter: &mut Interpreter,
        _arguments: Vec<Value>,
    ) -> Result<Value, LoxError> {
        match interpreter.begin_time.elapsed() {
            Ok(x) => Ok(Value::Number(x.as_millis() as f32)),
            Err(x) => panic!("{}", x),
        }
    }
}

impl fmt::Display for LoxClock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native fn>")
    }
}
