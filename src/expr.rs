use std::fmt::{Display, Formatter, Result};

use crate::token::Token;
use crate::token_type::TokenType;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  Bool(bool),
  F32(f32),
  String(String),
  None(Option<u8>)
}

impl std::fmt::Display for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match &self {
      Self::Bool(x) => write!(f, "{}", x),
      Self::F32(x) => write!(f, "{}", x),
      Self::String(x) => write!(f, "{}", x),
      Self::None(_) => write!(f, "{}", "None")
    }
  }
}

pub trait Expr: Display {
  fn evaluate(&self) -> Option<Value>;
}

impl Expr for Box<dyn Expr> {
  fn evaluate(&self) -> Option<Value> {
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

    impl $(<$($generics:$trait),*>)? $name $(<$($generics),*>)? {
      pub fn new($($element: $ty), *) -> $name $(<$($generics),*>)? {
        $name {
          $($element), *
        }
      }
    }
  }
}

make_expr!(Binary<T: Expr, U:Expr>, left: T, operator: Token, right: U);

impl<T: Expr, U: Expr> Expr for Binary<T, U> {
  fn evaluate(&self) -> Option<Value> {
    let left: Option<Value> = self.left.evaluate();
    let right: Option<Value> = self.right.evaluate();
    match self.operator.token_type {
      TokenType::Minus => {
        match (left, right) {
          (Some(Value::F32(x)), Some(Value::F32(y))) => Some(Value::F32(x-y)),
          (_, _) => None
        }
      },
      TokenType::Plus => {
        match (left, right) {
          (Some(Value::F32(x)), Some(Value::F32(y))) =>
            Some(Value::F32(x+y)),
          (Some(Value::String(x)), Some(Value::String(y))) =>
            Some(Value::String(x+&y)),
          (_, _) => None
        }
      },
      TokenType::Slash => {
        match (left, right) {
          (Some(Value::F32(x)), Some(Value::F32(y))) =>
            Some(Value::F32(x/y)),
          (_, _) => None
        }
      },
      TokenType::Star => {
        match (left, right) {
          (Some(Value::F32(x)), Some(Value::F32(y))) =>
            Some(Value::F32(x*y)),
          (_, _) => None
        }
      },
      TokenType::Greater => {
        match (left, right) {
          (Some(Value::F32(x)), Some(Value::F32(y))) =>
            Some(Value::Bool(x>y)),
          (_, _) => None
        }
      },
      TokenType::GreaterEqual => {
        match (left, right) {
          (Some(Value::F32(x)), Some(Value::F32(y))) =>
            Some(Value::Bool(x>=y)),
          (_, _) => None
        }
      },
      TokenType::Less => {
        match (left, right) {
          (Some(Value::F32(x)), Some(Value::F32(y))) =>
            Some(Value::Bool(x<y)),
          (_, _) => None
        }
      },
      TokenType::LessEqual => {
        match (left, right) {
          (Some(Value::F32(x)), Some(Value::F32(y))) =>
            Some(Value::Bool(x<=y)),
          (_, _) => None
        }
      },
      TokenType::EqualEqual => {
        Some(Value::Bool(check_equal(left, right)))
      },
      TokenType::BangEqual => {
        Some(Value::Bool(!check_equal(left, right)))
      },
      _ => None
    }
  }
}

impl<T: Expr, U: Expr> Display for Binary<T, U> {
  fn fmt(&self, f: &mut Formatter) -> Result {
    write!(f, "({} {} {})", &self.operator.lexeme, &self.left, &self.right)
  }
}

make_expr!(Grouping<T: Expr>, expression: T);

impl<T: Expr> Expr for Grouping<T> {
  fn evaluate(&self) -> Option<Value> {
    self.expression.evaluate()
  }
}

impl<T: Expr + Display> Display for Grouping<T> {
  fn fmt(&self, f: &mut Formatter) -> Result {
    write!(f, "(group {})", &self.expression)
  }
}

make_expr!(Literal, value: Option<Value>);

impl Expr for Literal {
  fn evaluate(&self) -> Option<Value> {
    self.value.clone()
  }
}

impl Display for Literal {
  fn fmt(&self, f: &mut Formatter) -> Result {
    match &self.value {
      Some(x) => write!(f, "{}", x),
      None => write!(f, "{}", "None")
    }
  }
}

make_expr!(Unary<T: Expr>, operator: Token, right: T);

impl<T: Expr> Expr for Unary<T> {
  fn evaluate(&self) -> Option<Value> {
    let value: Option<Value> = self.right.evaluate();
    match self.operator.token_type {
      TokenType::Minus => {
        match value {
          Some(Value::F32(x)) => Some(Value::F32(-x)),
          Some(_) => None,
          None => None
          }
        },
      TokenType::Bang => Some(Value::Bool(!check_bool(value))),
      _ => None
    }
  }
}

impl<T: Expr + Display> Display for Unary<T> {
  fn fmt(&self, f: &mut Formatter) -> Result {
    write!(f, "({} {})", &self.operator.lexeme, &self.right)
  }
}

fn check_bool(value: Option<Value>) -> bool {
  match value {
    Some(Value::Bool(x)) => x,
    Some(_) => true,
    None => true
  }
}

fn check_equal(left: Option<Value>, right: Option<Value>) -> bool {
  match (left, right) {
    (Some(Value::F32(x)), Some(Value::F32(y))) => x==y,
    (Some(Value::String(x)), Some(Value::String(y))) => x==y,
    (Some(Value::Bool(x)), Some(Value::Bool(y))) => x==y,
    (Some(Value::None(_)), Some(Value::None(_))) => true,
    (None, _) => panic!(),
    (_, None) => panic!(),
    (_, _) => false,
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_make_expr() {
    make_expr!(Literal, value:u8);

    impl Expr for Literal {
      fn evaluate(&self) -> Option<Value> { None }
    }
    
    impl Display for Literal {
      fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", &self.value)
      }
    }
    let literal: Literal = Literal::new(8);
    assert_eq!(literal.to_string(), "8".to_string());

    make_expr!(Binary<T: Expr, U:Expr>, left: T, operator: Token, right: U);

    impl<T: Expr, U:Expr> Expr for Binary<T, U> {
      fn evaluate(&self) -> Option<Value> { None }
    }
    
    impl<T: Expr, U: Expr> Display for Binary<T, U> {
      fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "({} {} {})", &self.operator.lexeme, &self.left, &self.right)
      }
    }
    
    let left: Literal = Literal::new(3);
    let right: Literal = Literal::new(5);
    let operator: Token = Token::new(TokenType::Plus, "+".into(), 1);
    let binary: Binary<Literal, Literal> = Binary::new(left, operator, right);
    assert_eq!(binary.to_string(), "(+ 3 5)".to_string());
  }

}
