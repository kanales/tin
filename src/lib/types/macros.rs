use super::{Atom, EnvironmentRef, Exp, List, Symbol, TinError, TinResult};
use std::{
    convert::{TryFrom, TryInto},
    env::args,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Symbol(Symbol),
    List(List),
}

impl TryFrom<List> for Pattern {
    type Error = TinError;

    fn try_from(value: List) -> Result<Self, Self::Error> {
        Ok(Pattern::List(value))
    }
}

impl TryFrom<Exp> for Pattern {
    type Error = TinError;

    fn try_from(value: Exp) -> Result<Self, Self::Error> {
        match value {
            Exp::Symbol(sym) => Ok(Pattern::Symbol(sym)),
            Exp::List(lst) => lst.try_into(),
            _ => Err(TinError::TypeMismatch(
                "Symbol | List".into(),
                value.to_string(),
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Macro {
    params: List,
    arity: usize,
    rule: Box<Exp>,
}

impl Macro {
    pub fn new(params: List, rule: Exp) -> TinResult<Self> {
        let arity = params.len();
        let params: List = params
            .iter()
            .map(|p| Ok(Symbol::try_from(p.clone())?.into()))
            .collect::<TinResult<_>>()?;
        Ok(Macro {
            params,
            arity,
            rule: Box::new(rule),
        })
    }
    pub fn expand(&self, args: &[Exp]) -> TinResult<Exp> {
        if args.len() != self.arity {
            return Err(TinError::ArityMismatch(self.arity, args.len()));
        }
        let params = self.params.clone();

        let mut res: Exp = *self.rule.clone();
        Ok(res)
    }
}

fn expand_rule(rule: Exp, params: List, args: &[Exp]) -> TinResult<Exp> {
    // match rule {
    //     Exp::List(mut lst) => {
    //         lst = lst
    //             .iter()
    //             .map(|el| expand_rule(el.clone(), params, args))
    //             .collect::<TinResult<_>>()?;
    //         Ok(lst.into())
    //     }
    //     Exp::Quasi() => {}
    // }
    //
    unimplemented!()
}

impl From<Macro> for Exp {
    fn from(m: Macro) -> Self {
        Exp::Macro(m)
    }
}

impl TryFrom<Exp> for Macro {
    type Error = TinError;

    fn try_from(value: Exp) -> Result<Self, Self::Error> {
        if let Exp::Macro(m) = value {
            return Ok(m);
        }
        Err(TinError::TypeMismatch(
            "macro".to_string(),
            value.to_string(),
        ))
    }
}
