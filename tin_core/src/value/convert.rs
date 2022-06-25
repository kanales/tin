use std::convert::TryFrom;
use std::{cell::Cell, rc::Rc};

use crate::error::TinError;

use super::{App, Closure, Def, Ident, If, Lambda, Pair, TinCell, TinValue};

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
