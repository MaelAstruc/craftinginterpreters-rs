use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::callable::{LoxCallable, LoxClass, LoxFunction};
use crate::environment::{EnvRef, Environment};
use crate::expr::{Expr, ExprEnum};
use crate::interpreter::Interpreter;
use crate::resolver::{ClassType, FunctionType, Resolver};
use crate::runtime_error::{self, LoxError, RuntimeError};
use crate::token::Token;
use crate::value::Value;
use crate::Lox;

#[derive(Clone)]
pub enum StmtEnum {
    Class(Box<Class>),
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
            StmtEnum::Class(x) => x.execute(interpreter),
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
            StmtEnum::Class(x) => x.resolve(resolver),
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
            StmtEnum::Class(x) => write!(f, "{}", x),
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
                    .deref_mut()
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
        interpreter.environment =
            EnvRef::new(Environment::new(Some(interpreter.environment.clone())));
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
pub struct Class {
    pub name: Token,
    pub superclass: Option<crate::expr::Var>,
    pub methods: Vec<Box<Function>>,
}

impl Stmt for Class {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let superclass = match &self.superclass {
            Some(x) => {
                match x.evaluate(interpreter)? {
                    Value::Callable(y) => {
                        match y {
                            LoxCallable::LoxClass(z) => Some(z),
                            _ => return Err(LoxError::RuntimeError(RuntimeError{
                                token: x.name.clone(),
                                message: "Superclass must be a class.".into()
                            }))
                        }
                    }
                    _ => return Err(LoxError::RuntimeError(RuntimeError{
                        token: x.name.clone(),
                        message: "Superclass must be a class.".into()
                    }))
                }
            }
            None => None
        };
        
        interpreter
            .environment
            .deref_mut()
            .define(self.name.lexeme.clone(), Value::Nil);
        
        if let Some(x) = superclass.clone() {
            interpreter.environment = EnvRef::new(Environment::new(Some(interpreter.environment.clone())));
            interpreter.environment.deref_mut().define(
                "super".into(),
                Value::Callable(LoxCallable::LoxClass(x))
            )
            // TO DO: Super class as Value Rc RefCell
        }

        let mut methods: HashMap<String, LoxFunction> = HashMap::new();
        for method in &self.methods {
            let function = LoxFunction {
                closure: interpreter.environment.clone(),
                declaration: (**method).clone(),
                is_initializer: method.name.lexeme == "init",
            };
            methods.insert(method.name.lexeme.clone(), function);
        }
        
        let klass = Value::Callable(LoxCallable::LoxClass(Rc::new(LoxClass {
            name: self.name.lexeme.clone(),
            superclass,
            methods,
        })));
        
        if self.superclass.is_some() {
            if let Some(x) = &interpreter.environment.clone().deref_mut().enclosing {
                interpreter.environment = x.clone();
            }
        }

        interpreter
            .environment
            .deref_mut()
            .assign(self.name.clone(), klass)?;
        
        Ok(Value::Nil)
    }

    fn resolve(&self, resolver: &mut Resolver) {
        let enclosing_class = resolver.current_class.clone();
        resolver.current_class = ClassType::CLASS;

        resolver.declare(&self.name);
        resolver.define(&self.name);

        if let Some(x) = &self.superclass {
            if x.name.lexeme == self.name.lexeme {
                Lox::error_token(&x.name,"A class can't inherit from itself.")
            }
            resolver.current_class = ClassType::SUBCLASS;
            x.resolve(resolver)
        }

        if self.superclass.is_some() {
            resolver.begin_scope();
            if let Some(x) = resolver.scopes.last_mut() {
                x.insert("super".into(), true);
            }
        }

        resolver.begin_scope();
        if let Some(x) = resolver.scopes.last_mut() {
            x.insert("this".into(), true);
        }

        for method in &self.methods {
            let declaration = match method.name.lexeme == "init" {
                true => FunctionType::INITIALIZER,
                false => FunctionType::METHOD,
            };
            resolver.resolve_function(method, declaration);
        }

        resolver.end_scope();

        if self.superclass.is_some() {
            resolver.end_scope();
        }
        
        resolver.current_class = enclosing_class;
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "class {}", self.name)
    }
}

#[derive(Clone)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Box<StmtEnum>>,
}

impl Stmt for Function {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let function = Value::Callable(LoxCallable::LoxFunction(Rc::new(LoxFunction {
            closure: interpreter.environment.clone(),
            declaration: self.clone(),
            is_initializer: false,
        })));
        interpreter
            .environment
            .deref_mut()
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
        write!(f, "{}", &self.name)
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
            None => (),
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
        if let FunctionType::NONE = resolver.current_function {
            Lox::error_token(&self.keyword, "Can't return from top-level code.")
        };
        match (&self.value, &resolver.current_function) {
            (Some(_), FunctionType::INITIALIZER) => {
                Lox::error_token(&self.keyword, "Can't return a value from an initializer.")
            }
            (Some(x), _) => x.resolve(resolver),
            (None, _) => (),
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
