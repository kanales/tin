use super::{EnvironmentRef, Evaluable, Exp, List, Symbol, TinError, TinResult};
use std::convert::{TryFrom, TryInto};

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
    params: Vec<Symbol>,
    body: Box<Exp>,
    env: EnvironmentRef,
}

impl Macro {
    pub fn new(env: EnvironmentRef, params: Vec<Symbol>, rule: Exp) -> Self {
        Macro {
            params,
            body: Box::new(rule),
            env,
        }
    }
}

impl Evaluable for Macro {
    fn eval(&self, args: &[Exp]) -> TinResult<Exp> {
        let env = EnvironmentRef::from(&self.params, args, self.env.clone());
        crate::lib::eval(env, *self.body.clone())
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
            "macro".to_string(),
            value.to_string(),
        ))
    }
}
