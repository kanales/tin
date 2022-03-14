use super::{Exp, TinError};
use std::{
    convert::{AsRef, From, TryFrom},
    fmt::Display,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(String);

impl Symbol {
    pub fn new<I: ToString>(x: I) -> Self {
        Symbol(x.to_string())
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl From<String> for Symbol {
    fn from(s: String) -> Self {
        Symbol(s)
    }
}
impl From<&str> for Symbol {
    fn from(s: &str) -> Self {
        Symbol(s.to_string())
    }
}

impl TryFrom<Exp> for Symbol {
    type Error = TinError;

    fn try_from(value: Exp) -> Result<Self, Self::Error> {
        match value {
            Exp::Ident(s) => Ok(s),
            _ => Err(TinError::TypeMismatch(
                vec!["symbol".into()],
                value.to_string(),
            )),
        }
    }
}
