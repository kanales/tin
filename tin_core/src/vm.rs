use std::cell::RefCell;
use std::rc::Rc;

use crate::datum::{Datum, Symbol};
use crate::error::TinResult;
use crate::eval::{Environ, Evaluable, Interner};
use crate::parser::parse_str;
use crate::value::{Closure, Def, Ident, Mapping, TinValue};

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
    environ: Rc<RefCell<Mapping>>,
}

#[macro_export]
macro_rules! def_closure {
    ($vm:ident, $l:literal, $e:expr) => {
        $vm.define_closure(
            $crate::datum::Symbol::try_new($l).expect("expected indentifier"),
            $e,
        )
    };
}

impl TinState {
    pub fn new() -> Self {
        TinState {
            interner: SymbolDict::new(),
            environ: Rc::new(RefCell::new(Mapping::new())),
        }
    }
    pub fn define_closure<F>(&mut self, s: Symbol, f: F)
    where
        F: Fn(Vec<TinValue>) -> TinResult<TinValue> + 'static,
    {
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
