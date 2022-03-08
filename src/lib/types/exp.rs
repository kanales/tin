use crate::lib::types::{Atom, Closure, List, Macro, Map, Number, Proc, Symbol};

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Symbol(Symbol),
    Number(Number),
    Bool(bool),
    Char(char),

    List(List),
    Vector(Vec<Exp>),
    String(String),
    Map(Map),
    Macro(Macro),
    Proc(Proc),
    Closure(Closure),
}

impl From<Symbol> for Exp {
    fn from(x: Symbol) -> Self {
        Exp::Symbol(x)
    }
}

impl From<List> for Exp {
    fn from(x: List) -> Self {
        Exp::List(x)
    }
}
impl TryFrom<Exp> for Vec<Exp> {
    type Error = TinError;

    fn try_from(value: Exp) -> Result<Self, Self::Error> {
        if let Exp::Vector(v) = value {
            return Ok(v);
        }

        return Err(TinError::TypeMismatch(
            "vector".to_string(),
            value.to_string(),
        ));
    }
}

impl<T> TryFrom<Exp> for Vec<T>
where
    T: TryFrom<Exp, Error = TinError>,
{
    type Error = TinError;

    fn try_from(value: Exp) -> Result<Self, Self::Error> {
        if let Exp::Vector(v) = value {
            return v
                .into_iter()
                .map(|x| x.try_into())
                .collect::<TinResult<_>>();
        }

        return Err(TinError::TypeMismatch(
            "vector".to_string(),
            value.to_string(),
        ));
    }
}

impl From<Proc> for Exp {
    fn from(x: Proc) -> Self {
        Exp::Proc(x)
    }
}

impl From<Closure> for Exp {
    fn from(x: Closure) -> Self {
        Exp::Closure(x)
    }
}

impl Exp {
    pub fn is_null(&self) -> bool {
        if let Exp::List(l) = self {
            return l.is_empty();
        }
        false
    }
    pub fn truthy(&self) -> bool {
        if let Exp::Bool(false) = self {
            false
        } else {
            true
        }
    }
}

use std::{
    convert::{TryFrom, TryInto},
    fmt::{self, Debug},
};

use super::{TinError, TinResult};
impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Exp::Symbol(s) => write!(f, "{}", s),
            Exp::Number(Number::Int(n)) => write!(f, "{}", n),
            Exp::Number(Number::Float(n)) => write!(f, "{}", n),
            Exp::Bool(b) => write!(f, "{}", b),
            Exp::Char(c) => write!(f, "\\{}", c),
            Exp::String(s) => write!(f, "\"{}\"", s),
            Exp::Macro(_) => write!(f, "#macro"),
            Exp::Proc(_) => write!(f, "#proc"),
            Exp::Closure(_) => write!(f, "#proc"),
            Exp::Map(m) => {
                write!(f, "{{ ")?;
                for (k, v) in m.iter() {
                    write!(f, "{} {} ", k, v)?;
                }
                write!(f, "}}")
            }
            Exp::Vector(v) => {
                write!(f, "[ ")?;
                for exp in v {
                    write!(f, "{} ", exp)?;
                }
                write!(f, "]")
            }
            Exp::List(l) => {
                write!(f, "( ")?;
                for exp in l.iter() {
                    write!(f, "{} ", exp)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl TryFrom<Exp> for List {
    type Error = TinError;
    fn try_from(e: Exp) -> Result<Self, Self::Error> {
        match e {
            Exp::List(lst) => Ok(lst),
            x => Err(TinError::TypeMismatch("list".into(), x.to_string())),
        }
    }
}

impl<T: Into<Number>> From<T> for Exp {
    fn from(n: T) -> Self {
        Exp::Number(n.into())
    }
}

impl From<bool> for Exp {
    fn from(n: bool) -> Self {
        Exp::Bool(n)
    }
}
