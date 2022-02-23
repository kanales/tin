use crate::lib::types::{Number, Symbol};
#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Symbol(Symbol),
    Number(Number),
    Bool(bool),
    Char(char),
}
impl From<String> for Atom {
    fn from(x: String) -> Self {
        Atom::Symbol(x.into())
    }
}

impl<T: Into<Number>> From<T> for Atom {
    fn from(x: T) -> Self {
        Atom::Number(x.into())
    }
}

impl From<bool> for Atom {
    fn from(x: bool) -> Self {
        Atom::Bool(x)
    }
}
