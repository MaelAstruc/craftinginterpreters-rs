use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::environment::Environment;
use crate::interpreter::Interpreter;
use crate::make_expr;  // from mod utils
use crate::runtime_error::RuntimeError;
use crate::token::Token;
use crate::token_type::TokenType;
use crate::value::Value;

#[warn(dead_code)]
pub enum ExprEnum {
    Binary(Box<Binary>),
    Grouping(Box<Grouping>),
    Literal(Box<Literal>),
    Unary(Box<Unary>),
    Var(Box<Var>),
    Assign(Box<Assign>),
    Logic(Box<Logic>)
}

impl Expr for ExprEnum {
    fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        match self {
            ExprEnum::Binary(x) => x.evaluate(environment),
            ExprEnum::Grouping(x) => x.evaluate(environment),
            ExprEnum::Literal(x) => x.evaluate(environment),
            ExprEnum::Unary(x) => x.evaluate(environment),
            ExprEnum::Var(x) => x.evaluate(environment),
            ExprEnum::Assign(x) => x.evaluate(environment),
            ExprEnum::Logic(x) => x.evaluate(environment),
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
        }
    }
}

pub trait Expr: std::fmt::Display {
    fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError>;
}

impl Expr for Box<dyn Expr> {
    fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        (**self).evaluate(environment)
    }
}

make_expr!(Binary, left: ExprEnum, operator: Token, right: ExprEnum);

impl Expr for Binary {
    fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let left: Value = match self.left.evaluate(environment.clone()) {
            Ok(x) => x,
            Err(x) => return Err(x)
        };
        let right: Value  = match self.right.evaluate(environment) {
            Ok(x) => x,
            Err(x) => return Err(x)
        };
        let token: &Token = &self.operator;
        let token_type: &TokenType = &token.token_type;
        match token_type {
            TokenType::Minus => {
                match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x-y)),
                    (x, y) => Err(Interpreter::check_operands(token.clone(), "expected two numbers", x, y))
                }
            },
            TokenType::Plus => {
                match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x+y)),
                    (Value::String(x), Value::String(y)) => Ok(Value::String(x+&y)),
                    (x, y) => Err(Interpreter::check_operands(token.clone(), "expected two numbers or two strings", x, y))
                }
            },
            TokenType::Slash => {
                match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x/y)),
                    (x, y) => Err(Interpreter::check_operands(token.clone(), "expected two numbers", x, y))
                }
            },
            TokenType::Star => {
                match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x*y)),
                    (x, y) => Err(Interpreter::check_operands(token.clone(), "expected two numbers", x, y))
                }
            },
            TokenType::Greater => {
                match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x>y)),
                    (x, y) => Err(Interpreter::check_operands(token.clone(), "expected two numbers", x, y))
                }
            },
            TokenType::GreaterEqual => {
                match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x>=y)),
                    (x, y) => Err(Interpreter::check_operands(token.clone(), "expected two numbers", x, y))
                }
            },
            TokenType::Less => {
                match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x<y)),
                    (x, y) => Err(Interpreter::check_operands(token.clone(), "expected two numbers", x, y))
                }
            },
            TokenType::LessEqual => {
                match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x<=y)),
                    (x, y) => Err(Interpreter::check_operands(token.clone(), "expected two numbers", x, y))
                }
            },
            TokenType::EqualEqual => Ok(Value::Bool(Interpreter::check_equal(left, right))),
            TokenType::BangEqual => Ok(Value::Bool(!Interpreter::check_equal(left, right))),
            _ => panic!("Unexpected token: {}", token)
        }
    }
}

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", &self.operator.lexeme, &self.left, &self.right)
    }
}

make_expr!(Grouping, expression: ExprEnum);

impl Expr for Grouping {
    fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        self.expression.evaluate(environment)
    }
}

impl fmt::Display for Grouping {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(group {})", &self.expression)
    }
}

make_expr!(Literal, value: Value);

impl Expr for Literal {
    fn evaluate(&self, _environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        Ok(self.value.clone())
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.value)
    }
}

make_expr!(Unary, operator: Token, right: ExprEnum);

impl Expr for Unary {
    fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let right: Value  = match self.right.evaluate(environment) {
            Ok(x) => x,
            Err(x) => return Err(x)
        };
        let token = &self.operator;
        let token_type = &token.token_type;
        match token_type {
            TokenType::Minus => {
                match right {
                    Value::Number(x) => Ok(Value::Number(-x)),
                    x => Err(Interpreter::check_operand(self.operator.clone(), "expected a number", x))
                }
                },
            TokenType::Bang => Ok(Value::Bool(!Interpreter::check_bool(&right))),
            _ => panic!("Expected tokens: {}, found token ({})", "(-) or (!)", self.operator.clone())
        }
    }
}

impl fmt::Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {})", &self.operator.lexeme, &self.right)
    }
}

make_expr!(Var, name: Token);

impl Expr for Var {
    fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        environment.as_ref().borrow_mut().get(self.name.clone())
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({})", &self.name.lexeme)
    }
}

make_expr!(Assign, name: Token, value: ExprEnum);

impl Expr for Assign {
    fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        match self.value.evaluate(environment.clone()) {
            Ok(x) => environment.as_ref().borrow_mut().assign(self.name.clone(), x),
            Err(x) => Err(x)
        }
    }
}

impl fmt::Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", &self.name.lexeme, &self.value)
    }
}

make_expr!(Logic, left: ExprEnum, operator: Token, right: ExprEnum);

impl Expr for Logic {
    fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> { 
        match self.left.evaluate(environment.clone()) {
            Ok(left) => match self.operator.token_type {
                TokenType::Or if *Interpreter::check_bool(&left) => Ok(left),
                TokenType::And if !Interpreter::check_bool(&left) => Ok(left),
                _ => self.right.evaluate(environment)
            },
            Err(x) => Err(x)
        }
    }
}

impl fmt::Display for Logic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {}", &self.left, &self.operator, &self.right)
    }
}

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
      fn evaluate(&self, _environement: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
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
      fn evaluate(&self, _environement: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
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
