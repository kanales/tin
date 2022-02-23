use crate::lib::types::{Atom, Exp, Number, TinError, TinResult};
use std::collections::hash_map::HashMap;
use std::convert::{From, TryFrom, TryInto};

use super::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Key {
    Int(i64),
    Bool(bool),
    Symbol(Symbol),
    Char(char),
    String(String),
}
impl From<Key> for Exp {
    fn from(k: Key) -> Self {
        match k {
            Key::Int(i) => Exp::Atom(Atom::Number(Number::Int(i))),
            Key::String(s) => Exp::String(s),
            Key::Bool(b) => Exp::Atom(Atom::Bool(b)),
            Key::Symbol(s) => Exp::Atom(Atom::Symbol(s)),
            Key::Char(s) => Exp::Atom(Atom::Char(s)),
        }
    }
}

impl TryFrom<Exp> for Key {
    type Error = TinError;

    fn try_from(value: Exp) -> Result<Self, Self::Error> {
        match value {
            Exp::Atom(Atom::Number(Number::Int(n))) => Ok(Key::Int(n)),
            Exp::String(s) => Ok(Key::String(s)),
            Exp::Atom(Atom::Bool(b)) => Ok(Key::Bool(b)),
            Exp::Atom(Atom::Symbol(s)) => Ok(Key::Symbol(s.into())),
            Exp::Atom(Atom::Char(s)) => Ok(Key::Char(s)),
            _ => Err(TinError::TypeMismatch(
                "Hashable".to_string(),
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
