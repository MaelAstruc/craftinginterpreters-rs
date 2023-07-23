use crate::token::Token;

pub struct RuntimeError {
    pub token: Token,
    pub message: String,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Run time error")
    }
}
