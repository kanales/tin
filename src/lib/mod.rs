pub mod eval;

#[macro_use]
pub mod macros;

pub mod parser;
pub mod procs;
pub mod types;

use crate::lib::eval::eval;
use crate::lib::parser::parse;
use crate::lib::types::{EnvironmentRef, Exp, TinResult};

pub struct Tin {
    base: EnvironmentRef,
}

impl Tin {
    pub fn new() -> Self {
        Tin {
            base: EnvironmentRef::default(),
        }
    }
    pub fn eval_str(&mut self, s: &str) -> TinResult<Exp> {
        let exp = parse(s)?;
        eval(self.base.clone(), exp)
    }

    pub fn eval(&mut self, e: Exp) -> TinResult<Exp> {
        eval(self.base.clone(), e)
    }
}
