use std::{cell::Cell, collections::HashMap, rc::Rc};

use crate::{
    datum::{Datum, Symbol},
    error::{TinError, TinResult},
    eval::{Environ, Evaluable, Interner},
};
mod convert;
// use convert::*;

#[derive(Debug)]
pub struct Pair {
    car: TinValue,
    cdr: TinValue,
}

impl Pair {
    pub fn new(car: TinValue, cdr: TinValue) -> Self {
        Pair { car, cdr }
    }
}

pub type Ident = usize; // Interned symbol

pub type Mapping = HashMap<Ident, TinValue>;
impl Environ for Mapping {
    fn fetch(&self, sym: &Ident) -> Option<TinValue> {
        self.get(sym).map(TinValue::clone)
    }
    fn put(&mut self, sym: Ident, val: TinValue) {
        self.insert(sym, val);
    }
}

#[derive(Debug)]
pub struct Lambda {
    formals: Vec<Ident>,
    body: Vec<TinValue>,
}

#[derive(Debug)]
pub struct If {
    test: TinValue,
    conseq: TinValue,
    altern: Option<TinValue>,
}

impl If {
    pub fn new(test: TinValue, conseq: TinValue) -> Self {
        If {
            test,
            conseq,
            altern: None,
        }
    }

    pub fn new_with_alternative(test: TinValue, conseq: TinValue, altern: TinValue) -> Self {
        If {
            test,
            conseq,
            altern: Some(altern),
        }
    }
}

pub struct TinCell {
    inner: Cell<TinValue>,
}

impl TinCell {
    pub fn new(v: TinValue) -> Self {
        TinCell {
            inner: Cell::new(v),
        }
    }
}

impl std::fmt::Debug for TinCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TinCell {{ ... }}")
    }
}

pub struct Closure(Box<dyn Fn(Vec<TinValue>) -> TinResult<TinValue>>);
impl Closure {
    pub fn new<F>(f: F) -> Self
    where
        F: Fn(Vec<TinValue>) -> TinResult<TinValue> + 'static,
    {
        Closure(Box::new(f))
    }
}

impl std::fmt::Debug for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Closure(...)")
    }
}

#[derive(Debug, Clone)]
pub struct Def(Ident, TinValue);

impl Evaluable for Def {
    fn eval_using<VM>(&self, vm: &mut VM) -> TinResult<TinValue>
    where
        VM: Interner + Environ,
    {
        vm.put(self.0, self.1.clone());
        Ok(TinValue::Nil)
    }
}

impl Def {
    pub fn new(id: Ident, v: TinValue) -> Self {
        Def(id, v)
    }
}

#[derive(Debug)]
pub struct App {
    cmd: TinValue,
    body: Vec<TinValue>,
}

impl App {
    pub fn new(cmd: TinValue, body: Vec<TinValue>) -> Self {
        App { cmd, body }
    }
}

impl Evaluable for App {
    fn eval_using<VM>(&self, vm: &mut VM) -> TinResult<TinValue>
    where
        VM: Interner + Environ,
    {
        match &self.cmd {
            TinValue::Closure(c) => c.0(self.body.clone()),
            TinValue::Lambda(l) => todo!(),
            _ => Err(TinError::NotAProc(Box::new(Datum::Nil))),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TinValue {
    // <identifier>
    Symbol(Ident),

    // literal
    Number(f64),              // self eval
    Bool(bool),               // self eval
    Char(char),               // self eval
    Vector(Rc<Vec<TinCell>>), // self eval + mutable
    String(Rc<String>),       // self eval
    Bytes(Rc<Vec<u8>>),       // self eval
    Quote(Rc<Datum>),
    Cell(Rc<TinCell>), // mutable
    Pair(Rc<Pair>),

    // <procedure call>
    App(Rc<App>),

    // <lambda expression>
    Lambda(Rc<Lambda>),

    // <conditional>
    If(Rc<If>),

    Def(Rc<Def>), // TODO define functions too
    Nil,
    Closure(Rc<Closure>),
    Exception(Rc<String>),
    Environ(Rc<Mapping>),
}

impl TinValue {
    fn truthy(&self) -> bool {
        if let TinValue::Bool(b) = self {
            *b
        } else {
            true
        }
    }
}

macro_rules! self_evaluating {
    ($t:ty => $var:ident) => {
        impl Evaluable for $t {
            fn eval_using<VM>(&self, _: &mut VM) -> TinResult<TinValue>
            where
                VM: Interner + Environ,
            {
                Ok(TinValue::$var(*self))
            }
        }
    };
}

self_evaluating!(bool => Bool);
self_evaluating!(f64 => Number);
self_evaluating!(char => Char);

impl Evaluable for Rc<String> {
    fn eval_using<VM>(&self, vm: &mut VM) -> TinResult<TinValue>
    where
        VM: Interner + Environ,
    {
        Ok(TinValue::String(Rc::clone(self)))
    }
}

impl Evaluable for If {
    fn eval_using<VM>(&self, vm: &mut VM) -> TinResult<TinValue>
    where
        VM: Interner + Environ,
    {
        let t = self.test.eval_using(vm)?;
        if t.truthy() {
            return self.conseq.eval_using(vm);
        }
        if let Some(altern) = &self.altern {
            return altern.eval_using(vm);
        }
        Ok(TinValue::Nil)
    }
}

impl Evaluable for TinValue {
    fn eval_using<VM>(&self, vm: &mut VM) -> TinResult<TinValue>
    where
        VM: Interner + Environ,
    {
        match self {
            // self eval
            TinValue::Quote(q) => quote(Rc::clone(q), vm),

            TinValue::Symbol(s) => match vm.fetch(&s) {
                Some(v) => Ok(v),
                None => Err(TinError::UndefinedSymbol(vm.unintern(*s).to_string())),
            },

            TinValue::App(a) => Rc::clone(a).eval_using(vm),
            TinValue::If(cond) => Rc::clone(cond).eval_using(vm),
            TinValue::Def(d) => d.clone().eval_using(vm),
            TinValue::Exception(_) => todo!(),
            _ => Ok(self.clone()),
        }
    }
}

fn quote<VM>(d: Rc<Datum>, vm: &mut VM) -> TinResult<TinValue>
where
    VM: Interner + Environ,
{
    match d.as_ref() {
        Datum::Symbol(s) => Ok(vm.intern(s).into()),
        //
        Datum::List(lst) => {
            let it = lst.into_iter();
            let mut k = TinValue::Nil;
            for el in it.rev() {
                let left = el.clone().eval_using(vm)?;
                k = Pair::new(left, k).into();
            }
            Ok(k)
        }
        Datum::DotList(lst, end) => {
            let it = lst.into_iter();
            let mut k = end.clone().eval_using(vm)?;
            for el in it.rev() {
                let left = el.clone().eval_using(vm)?;
                k = Pair::new(left, k).into();
            }
            Ok(k)
        } //
        x => x.clone().eval_using(vm),
    }
}
impl Evaluable for Symbol {
    fn eval_using<VM>(&self, vm: &mut VM) -> TinResult<TinValue>
    where
        VM: Interner + Environ,
    {
        Ok(TinValue::Symbol(vm.intern(&self)))
    }
}
