use super::{Atom, Exp, List, Symbol, TinError, TinResult};
use std::convert::{TryFrom, TryInto};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pattern {
    Symbol(Symbol),
    List(Vec<Pattern>),
}

impl TryFrom<List> for Pattern {
    type Error = TinError;

    fn try_from(value: List) -> Result<Self, Self::Error> {
        let vals: Vec<_> = value
            .iter()
            .map(|x| x.clone().try_into())
            .collect::<TinResult<_>>()?;
        Ok(Pattern::List(vals))
    }
}

impl TryFrom<Exp> for Pattern {
    type Error = TinError;

    fn try_from(value: Exp) -> Result<Self, Self::Error> {
        match value {
            Exp::Atom(Atom::Symbol(sym)) => Ok(Pattern::Symbol(sym)),
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
    params: Vec<Pattern>,
    arity: usize,
    rule: Box<Exp>,
}

impl Macro {
    pub fn new(params: Vec<Pattern>, rule: Exp) -> Self {
        let arity = params.len();
        Macro {
            params,
            arity,
            rule: Box::new(rule),
        }
    }
    pub fn expand(&self, args: &[Exp]) -> TinResult<Exp> {
        if args.len() != self.arity {
            return Err(TinError::ArityMismatch(self.arity, args.len()));
        }
        unimplemented!();

        //         let params = self.params.clone();
        //         let mut res: Exp = *self.rule.clone();
        //         for (i, param) in params.enumerate() {
        //             res = res.replace(&param, args[i].clone());
        //         }
        // Ok(res)
    }
}

impl From<Macro> for Exp {
    fn from(m: Macro) -> Self {
        Exp::Macro(m)
    }
}
