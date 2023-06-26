#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  Bool(bool),
  Number(f32),
  String(String),
  Nil
}

impl std::fmt::Display for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match &self {
      Self::Bool(x) => write!(f, "{}", x),
      Self::Number(x) => write!(f, "{}", x),
      Self::String(x) => write!(f, "\"{}\"", x),
      Self::Nil => write!(f, "{}", "nil")
    }
  }
}
