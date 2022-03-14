use crate::lib::eval::eval;
use crate::lib::parser::parse;
use crate::lib::types::{Atom, Exp, Number, TinError, TinResult};
use std::collections::hash_map::HashMap;
use std::convert::{From, TryFrom, TryInto};
use std::fmt::Display;

use super::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Key {
    Int(i64),
    Bool(bool),
    Keyword(Symbol),
    Char(char),
    String(String),
}
impl From<Key> for Exp {
    fn from(k: Key) -> Self {
        match k {
            Key::Int(i) => Exp::Number(Number::Int(i)),
            Key::String(s) => Exp::String(s),
            Key::Bool(b) => Exp::Bool(b),
            Key::Keyword(s) => Exp::Keyword(s),
            Key::Char(s) => Exp::Char(s),
        }
    }
}

impl Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Key::Int(i) => write!(f, "{}", i),
            Key::Bool(i) => write!(f, "{}", i),
            Key::Keyword(i) => write!(f, ":{}", i),
            Key::Char(i) => write!(f, "{}", i),
            Key::String(i) => write!(f, "{}", i),
        }
    }
}

impl TryFrom<Exp> for Key {
    type Error = TinError;

    fn try_from(value: Exp) -> Result<Self, Self::Error> {
        match value {
            Exp::Number(Number::Int(n)) => Ok(Key::Int(n)),
            Exp::String(s) => Ok(Key::String(s)),
            Exp::Bool(b) => Ok(Key::Bool(b)),
            Exp::Keyword(s) => Ok(Key::Keyword(s.into())),
            Exp::Char(s) => Ok(Key::Char(s)),
            _ => Err(TinError::TypeMismatch(
                vec![
                    "number".to_string(),
                    "string".to_string(),
                    "bool".to_string(),
                    "symbol".to_string(),
                    "char".to_string(),
                ],
                value.to_string(),
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Map(HashMap<Key, Exp>);

impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::addr_of!(self) == std::ptr::addr_of!(other)
    }
}

type Pairs<'a> = std::iter::Map<
    std::collections::hash_map::Iter<'a, Key, Exp>,
    fn((&'a Key, &'a Exp)) -> (Exp, Exp),
>;
impl Map {
    pub fn new() -> Self {
        Map(HashMap::new())
    }

    pub fn len(&self) -> Number {
        Number::Int(self.0.len() as i64)
    }

    pub fn includes(&self, k: &Key) -> bool {
        self.0.contains_key(k)
    }

    pub fn try_insert(&mut self, k: Exp, v: Exp) -> TinResult<()> {
        self.0.insert(k.try_into()?, v);
        Ok(())
    }

    pub fn iter(&self) -> Pairs {
        self.0.iter().map(|(k, v)| (k.clone().into(), v.clone()))
    }
}

impl std::ops::Index<&Key> for Map {
    type Output = Exp;
    fn index(&self, index: &Key) -> &Self::Output {
        &self.0[index]
    }
}
mod test {
    use crate::lib::{
        eval::eval,
        parser::parse,
        types::{Environment, EnvironmentRef},
    };
}
