use std::fmt;

use crate::callable::LoxCallable;
use crate::interpreter::Interpreter;
use crate::resolver::{ClassType, Resolver};
use crate::runtime_error::{LoxError, RuntimeError};
use crate::token::Token;
use crate::token_type::TokenType;
use crate::value::Value;
use crate::Lox;

#[derive(Clone)]
pub enum ExprEnum {
    Binary(Box<Binary>),
    Grouping(Box<Grouping>),
    Literal(Box<Literal>),
    Unary(Box<Unary>),
    Var(Box<Var>),
    Assign(Box<Assign>),
    Logic(Box<Logic>),
    Call(Box<Call>),
    Get(Box<Get>),
    Set(Box<Set>),
    Super(Box<Super>),
    This(Box<This>),
}

impl Expr for ExprEnum {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        match self {
            ExprEnum::Binary(x) => x.evaluate(interpreter),
            ExprEnum::Grouping(x) => x.evaluate(interpreter),
            ExprEnum::Literal(x) => x.evaluate(interpreter),
            ExprEnum::Unary(x) => x.evaluate(interpreter),
            ExprEnum::Var(x) => x.evaluate(interpreter),
            ExprEnum::Assign(x) => x.evaluate(interpreter),
            ExprEnum::Logic(x) => x.evaluate(interpreter),
            ExprEnum::Call(x) => x.evaluate(interpreter),
            ExprEnum::Get(x) => x.evaluate(interpreter),
            ExprEnum::Set(x) => x.evaluate(interpreter),
            ExprEnum::Super(x) => x.evaluate(interpreter),
            ExprEnum::This(x) => x.evaluate(interpreter),
        }
    }

    fn resolve(&self, resolver: &mut Resolver) {
        match self {
            ExprEnum::Binary(x) => x.resolve(resolver),
            ExprEnum::Grouping(x) => x.resolve(resolver),
            ExprEnum::Literal(x) => x.resolve(resolver),
            ExprEnum::Unary(x) => x.resolve(resolver),
            ExprEnum::Var(x) => x.resolve(resolver),
            ExprEnum::Assign(x) => x.resolve(resolver),
            ExprEnum::Logic(x) => x.resolve(resolver),
            ExprEnum::Call(x) => x.resolve(resolver),
            ExprEnum::Get(x) => x.resolve(resolver),
            ExprEnum::Set(x) => x.resolve(resolver),
            ExprEnum::Super(x) => x.resolve(resolver),
            ExprEnum::This(x) => x.resolve(resolver),
        }
    }
}

impl fmt::Display for ExprEnum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprEnum::Binary(x) => write!(f, "{}", x),
            ExprEnum::Grouping(x) => write!(f, "{}", x),
            ExprEnum::Literal(x) => write!(f, "{}", x),
            ExprEnum::Unary(x) => write!(f, "{}", x),
            ExprEnum::Var(x) => write!(f, "{}", x),
            ExprEnum::Assign(x) => write!(f, "{}", x),
            ExprEnum::Logic(x) => write!(f, "{}", x),
            ExprEnum::Call(x) => write!(f, "{}", x),
            ExprEnum::Get(x) => write!(f, "{}", x),
            ExprEnum::Set(x) => write!(f, "{}", x),
            ExprEnum::Super(x) => write!(f, "{}", x),
            ExprEnum::This(x) => write!(f, "{}", x),
        }
    }
}

pub trait Expr: std::fmt::Display {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError>;
    fn resolve(&self, resolver: &mut Resolver);
}

#[derive(Clone)]
pub struct Binary {
    pub left: ExprEnum,
    pub operator: Token,
    pub right: ExprEnum,
}

impl Expr for Binary {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let left: Value = self.left.evaluate(interpreter)?;
        let right: Value = self.right.evaluate(interpreter)?;
        let token: &Token = &self.operator;
        match token.token_type {
            TokenType::Minus => match (left, right) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x - y)),
                (x, y) => Err(LoxError::RuntimeError(Interpreter::check_operands(
                    token,
                    "expected two numbers",
                    x,
                    y,
                ))),
            },
            TokenType::Plus => match (left, right) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
                (Value::String(x), Value::String(y)) => Ok(Value::String(x + &y)),
                (x, y) => Err(LoxError::RuntimeError(Interpreter::check_operands(
                    token,
                    "expected two numbers or two strings",
                    x,
                    y,
                ))),
            },
            TokenType::Slash => match (left, right) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x / y)),
                (x, y) => Err(LoxError::RuntimeError(Interpreter::check_operands(
                    token,
                    "expected two numbers",
                    x,
                    y,
                ))),
            },
            TokenType::Star => match (left, right) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x * y)),
                (x, y) => Err(LoxError::RuntimeError(Interpreter::check_operands(
                    token,
                    "expected two numbers",
                    x,
                    y,
                ))),
            },
            TokenType::Greater => match (left, right) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x > y)),
                (x, y) => Err(LoxError::RuntimeError(Interpreter::check_operands(
                    token,
                    "expected two numbers",
                    x,
                    y,
                ))),
            },
            TokenType::GreaterEqual => match (left, right) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x >= y)),
                (x, y) => Err(LoxError::RuntimeError(Interpreter::check_operands(
                    token,
                    "expected two numbers",
                    x,
                    y,
                ))),
            },
            TokenType::Less => match (left, right) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x < y)),
                (x, y) => Err(LoxError::RuntimeError(Interpreter::check_operands(
                    token,
                    "expected two numbers",
                    x,
                    y,
                ))),
            },
            TokenType::LessEqual => match (left, right) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x <= y)),
                (x, y) => Err(LoxError::RuntimeError(Interpreter::check_operands(
                    token,
                    "expected two numbers",
                    x,
                    y,
                ))),
            },
            TokenType::EqualEqual => Ok(Value::Bool(Interpreter::check_equal(left, right))),
            TokenType::BangEqual => Ok(Value::Bool(!Interpreter::check_equal(left, right))),
            _ => panic!("Unexpected token: {}", token),
        }
    }

    fn resolve(&self, resolver: &mut Resolver) {
        self.left.resolve(resolver);
        self.right.resolve(resolver);
    }
}

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({} {} {})",
            &self.operator.lexeme, &self.left, &self.right
        )
    }
}

#[derive(Clone)]
pub struct Grouping {
    pub expression: ExprEnum,
}

impl Expr for Grouping {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        self.expression.evaluate(interpreter)
    }

    fn resolve(&self, resolver: &mut Resolver) {
        self.expression.resolve(resolver);
    }
}

impl fmt::Display for Grouping {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(group {})", &self.expression)
    }
}

#[derive(Clone)]
pub struct Literal {
    pub value: Value,
}

impl Expr for Literal {
    fn evaluate(&self, _interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        Ok(self.value.clone())
    }

    fn resolve(&self, _resolver: &mut Resolver) {}
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.value)
    }
}

#[derive(Clone)]
pub struct Unary {
    pub operator: Token,
    pub right: ExprEnum,
}

impl Expr for Unary {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let right: Value = match self.right.evaluate(interpreter) {
            Ok(x) => x,
            Err(x) => return Err(x),
        };
        let token = &self.operator;
        match &token.token_type {
            TokenType::Minus => match right {
                Value::Number(x) => Ok(Value::Number(-x)),
                x => Err(LoxError::RuntimeError(Interpreter::check_operand(
                    self.operator.clone(),
                    "expected a number",
                    x,
                ))),
            },
            TokenType::Bang => Ok(Value::Bool(!Interpreter::check_bool(&right))),
            _ => panic!(
                "Expected tokens: {}, found token ({})",
                "(-) or (!)",
                self.operator.clone()
            ),
        }
    }

    fn resolve(&self, resolver: &mut Resolver) {
        self.right.resolve(resolver);
    }
}

impl fmt::Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {})", &self.operator.lexeme, &self.right)
    }
}

#[derive(Clone)]
pub struct Var {
    pub name: Token,
    pub id: usize,
}

impl Expr for Var {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        interpreter.look_up_var(&self.name, &self.id)
    }

    fn resolve(&self, resolver: &mut Resolver) {
        match resolver.scopes.last() {
            Some(x) => match x.get(&self.name.lexeme) {
                Some(y) => match y {
                    true => resolver.resolve_local(self.id, self.name.clone()),
                    false => Lox::error_token(
                        &self.name,
                        "Can't read local variable in its own initializer.",
                    ),
                },
                None => resolver.resolve_local(self.id, self.name.clone()),
            },
            None => resolver.resolve_local(self.id, self.name.clone()),
        }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({})", &self.name.lexeme)
    }
}

#[derive(Clone)]
pub struct Assign {
    pub name: Token,
    pub value: ExprEnum,
    pub id: usize,
}

impl Expr for Assign {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let value = self.value.evaluate(interpreter)?;
        match interpreter.locals.get(&self.id) {
            Some(x) => interpreter
                .environment
                .deref_mut()
                .assign_at(*x, self.name.clone(), value),
            None => interpreter
                .globals
                .deref_mut()
                .assign(self.name.clone(), value),
        }
    }

    fn resolve(&self, resolver: &mut Resolver) {
        self.value.resolve(resolver);
        resolver.resolve_local(self.id, self.name.clone());
    }
}

impl fmt::Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", &self.name.lexeme, &self.value)
    }
}

#[derive(Clone)]
pub struct Logic {
    pub left: ExprEnum,
    pub operator: Token,
    pub right: ExprEnum,
}

impl Expr for Logic {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        match self.left.evaluate(interpreter) {
            Ok(left) => match self.operator.token_type {
                TokenType::Or if *Interpreter::check_bool(&left) => Ok(left),
                TokenType::And if !Interpreter::check_bool(&left) => Ok(left),
                _ => self.right.evaluate(interpreter),
            },
            Err(x) => Err(x),
        }
    }

    fn resolve(&self, resolver: &mut Resolver) {
        self.left.resolve(resolver);
        self.right.resolve(resolver);
    }
}

impl fmt::Display for Logic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {}", &self.left, &self.operator, &self.right)
    }
}

#[derive(Clone)]
pub struct Call {
    pub callee: ExprEnum,
    pub paren: Token,
    pub arguments: Vec<ExprEnum>,
}

impl Expr for Call {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let mut arguments: Vec<Value> = Vec::new();

        for argument in &self.arguments {
            arguments.push(argument.evaluate(interpreter)?)
        }
        match self.callee.evaluate(interpreter)? {
            Value::Callable(x) => match x {
                LoxCallable::LoxFunction(y) => {
                    if arguments.len() != y.arity() {
                        let message: String = format!(
                            "Expected {} arguments but got {}.",
                            y.arity(),
                            arguments.len()
                        );
                        return Err(LoxError::RuntimeError(RuntimeError {
                            token: self.paren.clone(),
                            message,
                        }));
                    }
                    y.call(interpreter, arguments)
                }
                LoxCallable::LoxClass(y) => {
                    if arguments.len() != y.arity() {
                        let message: String = format!(
                            "Expected {} arguments but got {}.",
                            y.arity(),
                            arguments.len()
                        );
                        return Err(LoxError::RuntimeError(RuntimeError {
                            token: self.paren.clone(),
                            message,
                        }));
                    }
                    y.call(interpreter, arguments)
                }
                LoxCallable::LoxClock(y) => {
                    if arguments.len() != y.arity() {
                        let message: String = format!(
                            "Expected {} arguments but got {}.",
                            y.arity(),
                            arguments.len()
                        );
                        return Err(LoxError::RuntimeError(RuntimeError {
                            token: self.paren.clone(),
                            message,
                        }));
                    }
                    y.call(interpreter, arguments)
                }
            },
            _ => panic!(),
        }
    }

    fn resolve(&self, resolver: &mut Resolver) {
        self.callee.resolve(resolver);
        for argument in &self.arguments {
            argument.resolve(resolver);
        }
    }
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut arguments: String = "".into();
        for arg in &self.arguments {
            arguments += &arg.to_string();
            arguments += ", ";
        }
        write!(f, "{}({})", self.callee, arguments)
    }
}

#[derive(Clone)]
pub struct Get {
    pub object: ExprEnum,
    pub name: Token,
}

impl Expr for Get {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let object = self.object.evaluate(interpreter)?;
        match object {
            Value::LoxInstance(x) => x.get(self.name.clone()),
            _ => Err(LoxError::RuntimeError(RuntimeError {
                token: self.name.clone(),
                message: "Only instances have properties.".into(),
            })),
        }
    }

    fn resolve(&self, resolver: &mut Resolver) {
        self.object.resolve(resolver);
    }
}

impl fmt::Display for Get {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.object, self.name)
    }
}

#[derive(Clone)]
pub struct Set {
    pub object: ExprEnum,
    pub name: Token,
    pub value: ExprEnum,
}

impl Expr for Set {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let object = self.object.evaluate(interpreter)?;

        match object {
            Value::LoxInstance(mut x) => {
                let value = self.value.evaluate(interpreter)?;
                x.set(self.name.clone(), value.clone());
                Ok(value)
            }
            _ => Err(LoxError::RuntimeError(RuntimeError {
                token: self.name.clone(),
                message: "Only instances have fields.".into(),
            })),
        }
    }

    fn resolve(&self, resolver: &mut Resolver) {
        self.value.resolve(resolver);
        self.object.resolve(resolver);
    }
}

impl fmt::Display for Set {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.object, self.value)
    }
}

#[derive(Clone)]
pub struct Super {
    pub keyword: Token,
    pub method: Token,
    pub id: usize,
}

impl Expr for Super {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        let distance = match interpreter.locals.get(&self.id) {
            Some(x) => x,
            None => todo!(),
        };

        let superclass = interpreter
            .environment
            .deref_mut()
            .get_at(*distance, "super")?;
        let superclass = match &superclass {
            Value::Callable(x) => match x {
                LoxCallable::LoxClass(y) => y,
                _ => panic!("Expected a Lox Class"),
            },
            _ => panic!("Expected a Lox Callable"),
        };

        let object = interpreter
            .environment
            .deref_mut()
            .get_at(distance - 1, "this")?;
        let object = match object {
            Value::LoxInstance(x) => x,
            _ => panic!("Expected an instance"),
        };

        let method = match superclass.find_method(self.method.lexeme.clone()) {
            Some(x) => x,
            None => {
                return Err(LoxError::RuntimeError(RuntimeError {
                    token: self.method.clone(),
                    message: format!("Undefined property '{}'.", self.method.lexeme),
                }))
            }
        };
        Ok(Value::Callable(LoxCallable::LoxFunction(std::rc::Rc::new(
            method.bind(object),
        ))))
    }

    fn resolve(&self, resolver: &mut Resolver) {
        match resolver.current_class {
            ClassType::NONE => {
                Lox::error_token(&self.keyword, "Can't use 'super' outside of a class.")
            }
            ClassType::CLASS => Lox::error_token(
                &self.keyword,
                "Can't use 'super' in a class with no superclass.",
            ),
            ClassType::SUBCLASS => resolver.resolve_local(self.id, self.keyword.clone()),
        }
    }
}

impl fmt::Display for Super {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "super")
    }
}

#[derive(Clone)]
pub struct SuperRef {
    pub superclass: std::rc::Rc<std::cell::RefCell<Super>>,
}

#[derive(Clone)]
pub struct This {
    pub keyword: Token,
    pub id: usize,
}

impl Expr for This {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<Value, LoxError> {
        interpreter.look_up_var(&self.keyword, &self.id)
    }

    fn resolve(&self, resolver: &mut Resolver) {
        if let ClassType::NONE = resolver.current_class {
            Lox::error_token(&self.keyword, "Can't use 'this' outside of a class.");
            return;
        }

        resolver.resolve_local(self.id, self.keyword.clone())
    }
}

impl fmt::Display for This {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "this")
    }
}

/*
#[cfg(test)]
  mod tests_expr {
  use std::cell::RefCell;
  use std::fmt;
  use std::rc::Rc;

  use crate::environment::Environment;
  use crate::expr::Expr;
  use crate::make_expr; // from mod utils
  use crate::runtime_error::RuntimeError;
  use crate::token::Token;
  use crate::token_type::TokenType;
  use crate::value::Value;


  #[test]
  fn test_make_expr() {
    make_expr!(Literal, value:u8);

    impl Expr for Literal {
      fn evaluate(&self, _environement: Rc<RefCell<Environment>>) -> Result<Value, LoxError> {
        Ok(Value::Nil)
      }
    }

    impl fmt::Display for Literal {
      fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.value)
      }
    }
    let literal: Literal = Literal{value: 8};
    assert_eq!(literal.to_string(), "8".to_string());

    make_expr!(Binary<T: Expr, U:Expr>, left: T, operator: Token, right: U);

    impl<T: Expr, U:Expr> Expr for Binary<T, U> {
      fn evaluate(&self, _environement: Rc<RefCell<Environment>>) -> Result<Value, LoxError> {
        Ok(Value::Nil)
      }
    }

    impl<T: Expr, U: Expr> fmt::Display for Binary<T, U> {
      fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", &self.operator.lexeme, &self.left, &self.right)
      }
    }

    let left: Literal = Literal{value: 3};
    let right: Literal = Literal{value: 5};
    let operator: Token = Token {token_type: TokenType::Plus, lexeme: "+".into(), line: 1};
    let binary: Binary<Literal, Literal> = Binary{left, operator, right};
    assert_eq!(binary.to_string(), "(+ 3 5)".to_string());
  }
}
*/
