use std::fmt;

use crate::interpreter::Interpreter;
use crate::runtime_error::RuntimeError;
use crate::token::Token;
use crate::token_type::TokenType;
use crate::value::Value;

pub trait Expr: std::fmt::Display {
  fn evaluate(&self) -> Result<Value, RuntimeError>;
}

impl Expr for Box<dyn Expr> {
  fn evaluate(&self) -> Result<Value, RuntimeError> {
    (**self).evaluate()
  }
}

macro_rules! make_expr {
  (
    $name:ident
    $(<$($generics:tt: $trait:ident),*>)?, 
    $($element: ident: $ty: ty), *
  ) => {
    pub struct $name $(<$($generics:$trait),*>)? {
      $(pub $element: $ty), *
    }
  }
}


make_expr!(Binary<T: Expr, U:Expr>, left: T, operator: Token, right: U);

impl<T: Expr, U: Expr> Expr for Binary<T, U> {
    fn evaluate(&self) -> Result<Value, RuntimeError> {
        let left: Value = match self.left.evaluate() {
            Ok(x) => x,
            Err(x) => return Err(x)
        };
        let right: Value  = match self.right.evaluate() {
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

impl<T: Expr, U: Expr> fmt::Display for Binary<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", &self.operator.lexeme, &self.left, &self.right)
    }
}

make_expr!(Grouping<T: Expr>, expression: T);

impl<T: Expr> Expr for Grouping<T> {
    fn evaluate(&self) -> Result<Value, RuntimeError> {
        self.expression.evaluate()
    }
}

impl<T: Expr + fmt::Display> fmt::Display for Grouping<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(group {})", &self.expression)
    }
}

make_expr!(Literal, value: Value);

impl Expr for Literal {
    fn evaluate(&self) -> Result<Value, RuntimeError> {
        Ok(self.value.clone())
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.value)
    }
}

make_expr!(Unary<T: Expr>, operator: Token, right: T);

impl<T: Expr> Expr for Unary<T> {
    fn evaluate(&self) -> Result<Value, RuntimeError> {
        let right: Value  = match self.right.evaluate() {
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
            TokenType::Bang => Ok(Value::Bool(!Interpreter::check_bool(right))),
            _ => panic!("Expected tokens: {}, found token ({})", "(-) or (!)", self.operator.clone())
        }
    }
}

impl<T: Expr + fmt::Display> fmt::Display for Unary<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {})", &self.operator.lexeme, &self.right)
    }
}


#[cfg(test)]
mod tests_expr {
  use std::fmt;

  use crate::expr::Expr;
  use crate::runtime_error::RuntimeError;
  use crate::token::Token;
  use crate::token_type::TokenType;
  use crate::value::Value;


  #[test]
  fn test_make_expr() {
    make_expr!(Literal, value:u8);

    impl Expr for Literal {
      fn evaluate(&self) -> Result<Value, RuntimeError> {
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
      fn evaluate(&self) -> Result<Value, RuntimeError> {
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
