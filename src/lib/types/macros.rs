use persistent::list;

use super::{Environment, EnvironmentRef, Evaluable, Exp, List, Symbol, TinError, TinResult};
use crate::lib::eval;
use std::convert::{TryFrom, TryInto};

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(Symbol),
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
            Exp::Ident(sym) => Ok(Pattern::Ident(sym)),
            Exp::List(lst) => lst.try_into(),
            _ => Err(TinError::TypeMismatch(
                vec!["symbol".into(), "list".into()],
                value.to_string(),
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Macro {
    params: list::List<Symbol>,
    body: Box<Exp>,
    va: Option<Symbol>,
    env: EnvironmentRef,
}

impl Macro {
    pub fn new(env: EnvironmentRef, params: list::List<Symbol>, rule: Exp) -> Self {
        Macro {
            params,
            va: None,
            body: Box::new(rule),
            env,
        }
    }
}

impl Evaluable for Macro {
    fn eval(&self, args: List) -> TinResult<Exp> {
        let argc = args.len();
        let plen = self.params.len();
        if argc < plen {
            return Err(TinError::ArityMismatch(plen, argc));
        }
        let mut env = Environment::from(self.env.clone());
        let mut it = args;
        for p in self.params.iter() {
            env.insert(p.clone(), it.head().unwrap().clone());
            it = it.tail();
        }

        match (self.va.as_ref(), it.len()) {
            (None, 0) => {}
            (None, _) => return Err(TinError::ArityMismatch(plen, argc)),
            (Some(x), _) => env.insert(x.clone(), it.into()),
        }

        eval(env.as_ref(), *self.body.clone())
    }
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
            vec!["macro".to_string()],
            value.to_string(),
        ))
    }
}
