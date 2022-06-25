use std::fs::File;

use crate::builtin;
use crate::datum::{Datum, Symbol};
use crate::error::{TinError, TinResult};
use crate::eval::{Evaluable, Interner};
use crate::parser::{parse_form, parse_str};
use crate::scanner::tokenize_file;
use crate::value::{Closure, Def, Environment, Function, Ident, TinValue};

struct SymbolDict {
    symbols: Vec<Symbol>,
    counter: usize,
}

impl SymbolDict {
    pub fn new() -> Self {
        SymbolDict {
            symbols: Vec::new(),
            counter: 0,
        }
    }
}

impl Interner for SymbolDict {
    fn find(&self, sym: &Symbol) -> Option<Ident> {
        for (i, e) in self.symbols.iter().enumerate() {
            if e == sym {
                return Some(i);
            }
        }
        None
    }

    fn intern(&mut self, sym: &Symbol) -> Ident {
        if let Some(s) = self.find(sym) {
            return s;
        }
        self.symbols.push(sym.clone());
        self.counter += 1;
        self.counter - 1
    }

    fn unintern(&self, sym: Ident) -> &Symbol {
        &self.symbols[sym]
    }
}

pub struct TinState {
    interner: SymbolDict,
    environ: Environment,
}

impl TinState {
    pub fn new() -> Self {
        let mut s = TinState {
            interner: SymbolDict::new(),
            environ: Environment::new(),
        };
        builtin::builtins(&mut s);
        s
    }
    pub fn define_closure(&mut self, s: Symbol, f: Function) {
        let kw = self.intern(&s);
        let cl = Closure::new(f);
        Def::new(kw, cl.into())
            .eval_using(&mut self.interner, &mut self.environ)
            .unwrap();
    }

    pub fn eval_str(&mut self, s: &str) -> TinResult<TinValue> {
        let d = parse_str(s)?;
        self.eval_datum(d)
    }

    pub fn eval_datum(&mut self, d: Datum) -> TinResult<TinValue> {
        let expr = d.eval_using(&mut self.interner, &mut self.environ)?;
        expr.eval_using(&mut self.interner, &mut self.environ)
    }

    pub fn do_file(&mut self, f: File) -> TinResult<TinValue> {
        let mut it = tokenize_file(f);
        let mut last = TinValue::default();
        loop {
            let form = parse_form(it.by_ref());
            match form {
                Ok(x) => {
                    last = self.eval_datum(x)?;
                }
                Err(TinError::EOF) => return Ok(last),
                Err(e) => return Err(e),
            }
        }
    }
}

impl Interner for TinState {
    fn find(&self, sym: &Symbol) -> Option<Ident> {
        self.interner.find(sym)
    }

    fn intern(&mut self, sym: &Symbol) -> Ident {
        self.interner.intern(sym)
    }

    fn unintern(&self, sym: Ident) -> &Symbol {
        self.interner.unintern(sym)
    }
}
