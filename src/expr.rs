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

macro_rules! make_binary {
  ($name: ident, $left: ident, $operator: ident, $right: ident) => {
    pub struct $name<T: Expr, U: Expr> {
      pub $left: T,
      pub $operator: Token,
      pub $right: U
    }

    impl<T: Expr + Display, U: Expr + Display> Display for $name<T, U> {
      fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "({} {} {})", &self.$operator.lexeme, &self.$left, &self.$right)
      }
    }

    impl<T: Expr, U: Expr> $name<T, U> {
      pub fn new($left: T, $operator: Token, $right: U) -> $name<T,U> {
        $name {
          $left: $left,
          $operator: $operator,
          $right: $right,
        }
      }
    }
  }
}

macro_rules! make_grouping {
  ($name: ident, $expression: ident) => {
    pub struct $name<T: Expr> {
      pub $expression: T
    }

    impl<T: Expr + Display> Display for $name<T> {
      fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "(group {})", &self.$expression)
      }
    }

    impl<T: Expr> $name<T> {
      pub fn new($expression: T) -> $name<T> {
        $name {
          $expression: $expression,
        }
      }
    }
  }
}

macro_rules! make_literal {
  ($name: ident, $value: ident) => {
    pub struct $name {
      pub $value: Option<Value>
    }

    impl Display for Literal {
      fn fmt(&self, f: &mut Formatter) -> Result {
        match &self.$value {
          Some(x) => write!(f, "{}", x),
          None => write!(f, "{}", "Some")
        }
      }
    }

    impl $name {
      pub fn new($value: Option<Value>) -> $name {
        $name {
          $value: $value,
        }
      }
    }
  }
}

macro_rules! make_unary {
  ($name: ident, $operator: ident, $right: ident) => {
    pub struct $name<T: Expr> {
      pub $operator: Token,
      pub $right: T
    }

    impl<T: Expr + Display> Display for $name<T> {
      fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "({} {})", &self.$operator.lexeme, &self.$right)
      }
    }

    impl<T: Expr> $name<T> {
      pub fn new($operator: Token, $right: T) -> $name<T> {
        $name {
          $operator: $operator,
          $right: $right,
        }
      }
    }
  }
}

make_binary!(Binary, left, operator, right);

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

make_grouping!(Grouping, expression);

impl<T: Expr> Expr for Grouping<T> {
  fn evaluate(&self) -> Option<Value> {
    self.expression.evaluate()
  }
}

make_literal!(Literal, value);

impl Expr for Literal {
  fn evaluate(&self) -> Option<Value> {
    self.value.clone()
  }
}

make_unary!(Unary, operator, right);

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
