use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::Rc,
};

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

    pub fn from_iter<I>(items: I) -> TinValue
    where
        I: Iterator<Item = TinValue> + DoubleEndedIterator,
    {
        let mut end = TinValue::Nil;
        for el in items.rev() {
            end = Pair::new(el, end).into();
        }
        end
    }
}

pub type Ident = usize; // Interned symbol

#[derive(Debug)]
pub struct Mapping {
    env: HashMap<Ident, TinValue>,
    parent: Option<Rc<RefCell<Mapping>>>,
}

impl Mapping {
    pub fn new() -> Self {
        Mapping {
            env: HashMap::new(),
            parent: None,
        }
    }
}

impl Environ for Rc<RefCell<Mapping>> {
    fn fetch(&self, sym: &Ident) -> Option<TinValue> {
        self.as_ref()
            .borrow()
            .env
            .get(sym)
            .map(TinValue::clone)
            .or_else(|| self.borrow().parent.as_ref().and_then(|p| p.fetch(sym)))
    }
    fn put(&mut self, sym: Ident, val: TinValue) {
        self.borrow_mut().env.insert(sym, val);
    }

    fn make_child(s: &Self) -> Self {
        Rc::new(RefCell::new(Mapping {
            env: HashMap::new(),
            parent: Some(Rc::clone(s)),
        }))
    }
}

#[derive(Debug)]
pub struct Lambda {
    formals: Vec<Ident>,
    va_sink: Option<Ident>,
    body: Vec<TinValue>,
}

impl Lambda {
    pub fn new_no_params(body: Vec<TinValue>) -> Self {
        Lambda {
            formals: Vec::new(),
            va_sink: None,
            body,
        }
    }

    pub fn new_symbol(va: Ident, body: Vec<TinValue>) -> Self {
        Lambda {
            formals: Vec::new(),
            va_sink: Some(va),
            body,
        }
    }

    pub fn new_dotted(formals: Vec<Ident>, va: Ident, body: Vec<TinValue>) -> Self {
        Lambda {
            formals,
            va_sink: Some(va),
            body,
        }
    }

    pub fn new_list(formals: Vec<Ident>, body: Vec<TinValue>) -> Self {
        Lambda {
            formals,
            va_sink: None,
            body,
        }
    }

    /// returns None if accepting varargs
    pub fn arity(&self) -> Option<usize> {
        if let None = self.va_sink {
            return Some(self.formals.len());
        }
        None
    }

    /// true if accepts `n` or more args; fails if accepts exactly `n`
    pub fn is_arity_at_least(&self, n: usize) -> bool {
        match self.va_sink {
            Some(_) => self.formals.len() < n,
            None => false,
        }
    }
}

impl Evaluable for Lambda {
    fn eval_using<I, E>(&self, i: &mut I, e: &mut E) -> TinResult<TinValue>
    where
        I: Interner,
        E: Environ,
    {
        if self.formals.len() == 0 {
            let mut local = E::make_child(e);
            self.body
                .iter()
                .fold(Ok(TinValue::Nil), |_a, exp| exp.eval_using(i, &mut local))
        } else {
            Err(TinError::ArityMismatch(0, self.formals.len()))
        }
    }
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
    fn eval_using<I, E>(&self, _i: &mut I, e: &mut E) -> TinResult<TinValue>
    where
        I: Interner,
        E: Environ,
    {
        e.put(self.0, self.1.clone());
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
    fn eval_using<I, E>(&self, i: &mut I, e: &mut E) -> TinResult<TinValue>
    where
        I: Interner,
        E: Environ,
    {
        let body = self
            .body
            .iter()
            .map(|x| x.eval_using(i, e))
            .collect::<TinResult<_>>()?;
        match &self.cmd {
            TinValue::Closure(c) => c.0(body),
            TinValue::Lambda(l) => {
                let bl = body.len();
                let fl = l.formals.len();
                if bl < fl {
                    return Err(TinError::ArityMismatch(fl, bl));
                }
                let mut env = E::make_child(e);
                if let Some(s) = l.va_sink {
                    let mut it = body.iter().map(TinValue::clone);
                    for (el, sym) in it.by_ref().zip(l.formals.iter()) {
                        env.put(*sym, el)
                    }
                    let rest = Pair::from_iter(it);
                    env.put(s, rest);

                    l.body
                        .iter()
                        .fold(Ok(TinValue::Nil), |_, exp| exp.eval_using(i, &mut env))
                } else {
                    if bl > fl {
                        return Err(TinError::ArityMismatch(fl, bl));
                    }
                    let it = body.iter().map(TinValue::clone);
                    for (el, sym) in it.zip(l.formals.iter()) {
                        env.put(*sym, el)
                    }
                    l.body
                        .iter()
                        .fold(Ok(TinValue::Nil), |_, exp| exp.eval_using(i, &mut env))
                }
            }
            _ => todo!(),
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
            fn eval_using<I, E>(&self, i: &mut I, e: &mut E) -> TinResult<TinValue>
            where
                I: Interner,
                E: Environ,
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
    fn eval_using<I, E>(&self, i: &mut I, e: &mut E) -> TinResult<TinValue>
    where
        I: Interner,
        E: Environ,
    {
        Ok(TinValue::String(Rc::clone(self)))
    }
}

impl Evaluable for If {
    fn eval_using<I, E>(&self, i: &mut I, e: &mut E) -> TinResult<TinValue>
    where
        I: Interner,
        E: Environ,
    {
        let t = self.test.eval_using(i, e)?;
        if t.truthy() {
            return self.conseq.eval_using(i, e);
        }
        if let Some(altern) = &self.altern {
            return altern.eval_using(i, e);
        }
        Ok(TinValue::Nil)
    }
}

impl Evaluable for TinValue {
    fn eval_using<I, E>(&self, i: &mut I, e: &mut E) -> TinResult<TinValue>
    where
        I: Interner,
        E: Environ,
    {
        match self {
            // self eval
            TinValue::Quote(q) => quote(Rc::clone(q), i, e),

            TinValue::Symbol(s) => match e.fetch(&s) {
                Some(v) => Ok(v),
                None => Err(TinError::UndefinedSymbol(i.unintern(*s).to_string())),
            },

            TinValue::App(a) => Rc::clone(a).eval_using(i, e),
            TinValue::If(cond) => Rc::clone(cond).eval_using(i, e),
            TinValue::Def(d) => d.clone().eval_using(i, e),
            TinValue::Exception(_) => todo!(),
            _ => Ok(self.clone()),
        }
    }
}

fn quote<I, E>(d: Rc<Datum>, i: &mut I, e: &mut E) -> TinResult<TinValue>
where
    I: Interner,
    E: Environ,
{
    match d.as_ref() {
        Datum::Symbol(s) => Ok(i.intern(s).into()),
        //
        Datum::List(lst) => {
            let it = lst.into_iter();
            let mut k = TinValue::Nil;
            for el in it.rev() {
                let left = el.clone().eval_using(i, e)?;
                k = Pair::new(left, k).into();
            }
            Ok(k)
        }
        Datum::DotList(lst, end) => {
            let it = lst.into_iter();
            let mut k = end.clone().eval_using(i, e)?;
            for el in it.rev() {
                let left = el.clone().eval_using(i, e)?;
                k = Pair::new(left, k).into();
            }
            Ok(k)
        } //
        x => x.clone().eval_using(i, e),
    }
}
impl Evaluable for Symbol {
    fn eval_using<I, E>(&self, i: &mut I, e: &mut E) -> TinResult<TinValue>
    where
        I: Interner,
        E: Environ,
    {
        Ok(TinValue::Symbol(i.intern(&self)))
    }
}
