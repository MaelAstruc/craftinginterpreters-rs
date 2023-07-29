use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::environment::Environment;
use crate::Lox;
use crate::callable::{LoxCallable, LoxFunction};
use crate::expr::{Expr, ExprEnum};
use crate::interpreter::Interpreter;
use crate::resolver::{Resolver, FunctionType};
use crate::runtime_error::{self, LoxError};
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
    Return(Box<Return>),
    While(Box<While>),
}

impl Stmt for StmtEnum {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        match self {
            StmtEnum::Block(x) => x.execute(interpreter),
            StmtEnum::Expression(x) => x.execute(interpreter),
            StmtEnum::Function(x) => x.execute(interpreter),
            StmtEnum::If(x) => x.execute(interpreter),
            StmtEnum::Print(x) => x.execute(interpreter),
            StmtEnum::Return(x) => x.execute(interpreter),
            StmtEnum::Var(x) => x.execute(interpreter),
            StmtEnum::While(x) => x.execute(interpreter),
        }
    }

    fn resolve(&self, resolver: &mut Resolver) {
        match self {
            StmtEnum::Block(x) => x.resolve(resolver),
            StmtEnum::Expression(x) => x.resolve(resolver),
            StmtEnum::Function(x) => x.resolve(resolver),
            StmtEnum::If(x) => x.resolve(resolver),
            StmtEnum::Print(x) => x.resolve(resolver),
            StmtEnum::Return(x) => x.resolve(resolver),
            StmtEnum::Var(x) => x.resolve(resolver),
            StmtEnum::While(x) => x.resolve(resolver),
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
            StmtEnum::Return(x) => write!(f, "{}", x),
            StmtEnum::Var(x) => write!(f, "{}", x),
            StmtEnum::While(x) => write!(f, "{}", x),
        }
    }
}

pub trait Stmt: std::fmt::Display {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError>;
    fn resolve(&self, resolver: &mut Resolver);
}

#[derive(Clone)]
pub struct Expression {
    pub expression: ExprEnum,
}

impl Stmt for Expression {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        match self.expression.evaluate(interpreter) {
            Ok(x) => Ok(x),
            Err(x) => Err(x),
        }
    }

    fn resolve(&self, resolver: &mut Resolver) {
        self.expression.resolve(resolver);
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.expression)
    }
}

#[derive(Clone)]
pub struct Print {
    pub expression: ExprEnum,
}

impl Stmt for Print {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        match self.expression.evaluate(interpreter) {
            Ok(x) => {
                println!("{}", x);
                Ok(x)
            }
            Err(x) => Err(x),
        }
    }

    fn resolve(&self, resolver: &mut Resolver) {
        self.expression.resolve(resolver);
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
    pub initializer: ExprEnum,
}

impl Stmt for Var {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        match self.initializer.evaluate(interpreter) {
            Ok(x) => {
                interpreter
                    .environment
                    .as_ref()
                    .borrow_mut()
                    .define(self.name.lexeme.clone(), x.clone());
                Ok(x)
            }
            Err(x) => Err(x),
        }
    }

    fn resolve(&self, resolver: &mut Resolver) {
        resolver.declare(&self.name);
        self.initializer.resolve(resolver);
        resolver.define(&self.name);
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", &self.name, &self.initializer)
    }
}

#[derive(Clone)]
pub struct Block {
    pub statements: Vec<Box<StmtEnum>>,
}

impl Stmt for Block {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let previous = interpreter.environment.clone();
        interpreter.environment = match &interpreter.other_environment {
            Some(x) => x.clone(),
            None => Rc::new(RefCell::new(Environment::new(Some(previous.clone()))))
        };
        let mut value = Ok(Value::Nil);
        for statement in &self.statements {
            match statement.execute(interpreter) {
                Ok(x) => value = Ok(x),
                Err(x) => {
                    value = Err(x);
                    break;
                }
            }
        }
        interpreter.environment = previous;
        value
    }

    fn resolve(&self, resolver: &mut Resolver) {
        resolver.begin_scope();
        for statement in &self.statements {
            statement.resolve(resolver)
        }
        resolver.end_scope();
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
    pub body: Block,
}

impl Stmt for Function {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let function = Value::Callable(LoxCallable::LoxFunction(Rc::new(LoxFunction {
            closure: interpreter.environment.clone(),
            declaration: self.clone(),
        })));
        interpreter
            .environment
            .as_ref()
            .borrow_mut()
            .define(self.name.lexeme.clone(), function);
        Ok(Value::Nil)
    }

    fn resolve(&self, resolver: &mut Resolver) {
        resolver.declare(&self.name);
        resolver.define(&self.name);
        resolver.resolve_function(self, FunctionType::FUNCTION);
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
    pub else_branch: Option<StmtEnum>,
}

impl Stmt for If {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let mut value = Value::Nil;
        match self.condition.evaluate(interpreter) {
            Ok(x) if *Interpreter::check_bool(&x) => {
                value = self.then_branch.execute(interpreter)?;
            }
            Ok(_) => match &self.else_branch {
                Some(x) => value = x.execute(interpreter)?,
                None => (),
            },
            Err(x) => return Err(x),
        }
        Ok(value)
    }

    fn resolve(&self, resolver: &mut Resolver) {
        self.condition.resolve(resolver);
        self.then_branch.resolve(resolver);
        match &self.else_branch {
            Some(x) => x.resolve(resolver),
            None => ()
        }
    }
}

impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.else_branch {
            Some(x) => write!(
                f,
                "if ({}) \n then {{\n{}\n}} \n else {{\n{}\n}}",
                &self.condition, &self.then_branch, x
            ),
            None => write!(
                f,
                "if ({}) \n then {{\n{}\n}}",
                &self.condition, &self.then_branch
            ),
        }
    }
}

#[derive(Clone)]
pub struct Return {
    pub keyword: Token,
    pub value: Option<ExprEnum>,
}

impl Stmt for Return {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let value = match &self.value {
            Some(x) => match x.evaluate(interpreter) {
                Ok(y) => y,
                Err(y) => return Err(y),
            },
            None => Value::Nil,
        };
        Err(LoxError::Return(runtime_error::Return { value }))
    }

    fn resolve(&self, resolver: &mut Resolver) {
        if let FunctionType::NONE = resolver.current_function { Lox::error_token(&self.keyword, "Can't return from top-level code.") };
        match &self.value {
            Some(x) => x.resolve(resolver),
            None => ()
        }
    }
}

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.value {
            Some(x) => write!(f, "return {}", x),
            None => write!(f, "return nil"),
        }
    }
}

#[derive(Clone)]
pub struct While {
    pub condition: ExprEnum,
    pub body: StmtEnum,
}

impl Stmt for While {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        loop {
            match self.condition.evaluate(interpreter) {
                Ok(x) => {
                    if *Interpreter::check_bool(&x) {
                        match self.body.execute(interpreter) {
                            Ok(_) => (),
                            Err(x) => return Err(x),
                        }
                    } else {
                        break;
                    }
                }
                Err(x) => return Err(x),
            }
        }
        Ok(Value::Nil)
    }

    fn resolve(&self, resolver: &mut Resolver) {
        self.condition.resolve(resolver);
        self.body.resolve(resolver);
    }
}

impl fmt::Display for While {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "while {} {}", &self.condition, &self.body)
    }
}
