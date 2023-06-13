use crate::token::Token;

pub struct Binary<T,U> {
  pub left: T,
  pub operator: Token,
  pub right: U,
}

impl <T,U> Binary<T,U> {
  pub fn new( left: T, operator: Token, right: U) -> Binary<T,U> {
    Binary {
      left: left,
      operator: operator,
      right: right,
    }
  }
}
pub struct Grouping<T> {
  pub expression: T,
}

impl <T> Grouping<T> {
  pub fn new( expression: T) -> Grouping<T> {
    Grouping {
      expression: expression,
    }
  }
}
pub struct Literal<T> {
  pub value: T,
}

impl <T> Literal<T> {
  pub fn new( value: T) -> Literal<T> {
    Literal {
      value: value,
    }
  }
}
pub struct Unary<T> {
  pub operator: Token,
  pub right: T,
}

impl <T> Unary<T> {
  pub fn new( operator: Token, right: T) -> Unary<T> {
    Unary {
      operator: operator,
      right: right,
    }
  }
}
