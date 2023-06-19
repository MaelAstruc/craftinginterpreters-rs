use std::fmt::{Display, Formatter, Result};

use crate::token::Token;

pub trait Expr: Display {
  fn box_clone(&self) -> Option<Box<dyn Expr>>;
}

impl Expr for Box<dyn Expr> {
  fn box_clone(&self) -> Option<Box<dyn Expr>> {
    None
  }
}

impl Clone for Box<dyn Expr> {
  fn clone(&self) -> Box<dyn Expr> {
    self.box_clone().unwrap()
  }
}

macro_rules! make_binary {
  ($name: ident, $left: ident, $operator: ident, $right: ident) => {
    #[derive(Clone)]
    pub struct $name<T: Expr, U: Expr> {
      pub $left: T,
      pub $operator: Token,
      pub $right: U
    }

    impl<T: Expr + Clone + 'static, U: Expr + Clone + 'static> Expr for $name<T, U> {
      fn box_clone(&self) -> Option<Box<dyn Expr>> {
        Some(Box::new((*self).clone()))
      }
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
    #[derive(Clone)]
    pub struct $name<T: Expr> {
      pub $expression: T
    }

    impl<T: Expr + Clone + 'static> Expr for $name<T> {
      fn box_clone(&self) -> Option<Box<dyn Expr>> {
        Some(Box::new((*self).clone()))
      }
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
    #[derive(Clone)]
    pub struct $name<T> {
      pub $value: Option<T>
    }

    impl<T: Clone + Display + 'static> Expr for $name<T> {
      fn box_clone(&self) -> Option<Box<dyn Expr>> {
        Some(Box::new((*self).clone()))
      }
    }

    impl<T: Display> Display for $name<T> {
      fn fmt(&self, f: &mut Formatter) -> Result {
        match &self.$value {
          Some(x) => write!(f, "{}", x),
          None => write!(f, "{}", "Some")
        }
      }
    }

    impl<T> $name<T> {
      pub fn new($value: Option<T>) -> $name<T> {
        $name {
          $value: $value,
        }
      }
    }
  }
}

macro_rules! make_unary {
  ($name: ident, $operator: ident, $right: ident) => {
    #[derive(Clone)]
    pub struct $name<T: Expr> {
      pub $operator: Token,
      pub $right: T
    }

    impl<T: Expr + Clone + 'static> Expr for $name<T> {
      fn box_clone(&self) -> Option<Box<dyn Expr>> {
        Some(Box::new((*self).clone()))
      }
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
make_grouping!(Grouping, expression);
make_literal!(Literal, value);
make_unary!(Unary, operator, right);
