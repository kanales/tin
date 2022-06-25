use std::convert::TryFrom;
use std::{cell::Cell, rc::Rc};

use crate::error::TinError;

use super::{App, Closure, Def, Ident, If, Lambda, List, Pair, TinCell, TinValue};

macro_rules! packs {
    ($t:ty => $e:path) => {
        impl From<$t> for TinValue {
            fn from(x: $t) -> Self {
                $e(Rc::new(x))
            }
        }
    };
}

packs!(If => TinValue::If);
packs!(TinCell => TinValue::Cell);
packs!(Lambda => TinValue::Lambda);
packs!(Vec<TinCell> => TinValue::Vector);
packs!(Def => TinValue::Def);
packs!(Closure => TinValue::Closure);
packs!(App => TinValue::App);
packs!(List => TinValue::List);
packs!(Pair => TinValue::Pair);

impl From<Ident> for TinValue {
    fn from(s: Ident) -> Self {
        TinValue::Symbol(s)
    }
}

macro_rules! simple {
    ($t:ty => $e:path) => {
        impl From<$t> for TinValue {
            fn from(x: $t) -> Self {
                $e(x)
            }
        }
    };
}

simple!(f64 => TinValue::Number);

impl TryFrom<TinValue> for f64 {
    type Error = TinError;

    fn try_from(value: TinValue) -> Result<Self, Self::Error> {
        if let TinValue::Number(n) = value {
            Ok(n)
        } else {
            Err(TinError::NotANumber)
        }
    }
}

impl TryFrom<&TinValue> for f64 {
    type Error = TinError;

    fn try_from(value: &TinValue) -> Result<Self, Self::Error> {
        if let TinValue::Number(n) = value {
            Ok(*n)
        } else {
            Err(TinError::NotANumber)
        }
    }
}

impl TryFrom<TinValue> for Ident {
    type Error = TinError;
    fn try_from(value: TinValue) -> Result<Self, Self::Error> {
        if let TinValue::Symbol(id) = value {
            Ok(id)
        } else {
            Err(TinError::NotAnIdentifier(Box::new(value)))
        }
    }
}

impl From<Rc<TinValue>> for TinValue {
    fn from(val: Rc<TinValue>) -> Self {
        match val.as_ref() {
            TinValue::Symbol(x) => TinValue::Symbol(*x),
            TinValue::Number(x) => TinValue::Number(*x),
            TinValue::Bool(x) => TinValue::Bool(*x),
            TinValue::Char(x) => TinValue::Char(*x),
            TinValue::Vector(x) => TinValue::Vector(Rc::clone(x)),
            TinValue::String(x) => TinValue::String(Rc::clone(x)),
            TinValue::Bytes(x) => TinValue::Bytes(Rc::clone(x)),
            TinValue::Quote(x) => TinValue::Quote(Rc::clone(x)),
            TinValue::Cell(x) => TinValue::Cell(Rc::clone(x)),
            TinValue::Pair(x) => TinValue::Pair(Rc::clone(x)),
            TinValue::List(x) => TinValue::List(Rc::clone(x)),
            TinValue::App(x) => TinValue::App(Rc::clone(x)),
            TinValue::Lambda(x) => TinValue::Lambda(Rc::clone(x)),
            TinValue::If(x) => TinValue::If(Rc::clone(x)),
            TinValue::Def(x) => TinValue::Def(Rc::clone(x)),
            TinValue::Nil => TinValue::Nil,
            TinValue::Closure(x) => TinValue::Closure(Rc::clone(x)),
            TinValue::Exception(x) => TinValue::Exception(Rc::clone(x)),
            TinValue::Native(x) => TinValue::Native(Rc::clone(x)),
            TinValue::Port(x) => TinValue::Port(Rc::clone(x)),
            TinValue::Environment(x) => TinValue::Environment(Rc::clone(x)),
        }
    }
}

impl TryFrom<TinValue> for String {
    type Error = TinError;

    fn try_from(value: TinValue) -> Result<Self, Self::Error> {
        if let TinValue::String(s) = value {
            Ok(s.to_string())
        } else {
            Err(TinError::NotAString(Box::new(value)))
        }
    }
}
