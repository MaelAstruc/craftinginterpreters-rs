#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, SemiColon, Slash, Star,
  
    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
  
    // Literals.
    Identifier(String), String(String), Number(f32),
  
    // Keywords.
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
  
    Eof
  }

impl std::fmt::Display for TokenType {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match &self {
      Self::Identifier(x) => write!(f, "{}", x),
      Self::Number(x) => write!(f, "{}", x),
      Self::String(x) => write!(f, "{}", x),
      _ => write!(f, "{:?}", &self)
    }
  }
}  
  