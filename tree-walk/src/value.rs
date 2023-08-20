use crate::callable::{InstanceRef, LoxCallable};

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    Number(f32),
    String(String),
    Nil,
    Callable(LoxCallable),
    LoxInstance(InstanceRef),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Self::Bool(x) => write!(f, "{x}"),
            Self::Number(x) => write!(f, "{x}"),
            Self::String(x) => write!(f, "{x}"),
            Self::Nil => write!(f, "nil"),
            Self::Callable(x) => match x {
                LoxCallable::LoxFunction(y) => write!(f, "{y}"),
                LoxCallable::LoxClass(y) => write!(f, "{y}"),
                LoxCallable::LoxClock(y) => write!(f, "{y}"),
            },
            Self::LoxInstance(y) => write!(f, "{}", y.deref_mut()),
        }
    }
}
