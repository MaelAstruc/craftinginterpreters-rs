use crate::token::Token;

macro_rules! make_binary {
  ($name: ident, $left: ident, $operator: ident, $right: ident) => {
    pub struct $name<T,U>  {
      pub $left: T,
      pub $operator: Token,
      pub $right: U
    }

    impl<T,U>  $name<T,U>  {
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
    pub struct $name<T>  {
      pub $expression: T
    }

    impl<T>  $name<T>  {
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
    pub struct $name<T>  {
      pub $value: T
    }

    impl<T>  $name<T>  {
      pub fn new($value: T) -> $name<T> {
        $name {
          $value: $value,
        }
      }
    }
  }
}

macro_rules! make_unary {
  ($name: ident, $operator: ident, $right: ident) => {
    pub struct $name<T>  {
      pub $operator: Token,
      pub $right: T
    }

    impl<T>  $name<T>  {
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
