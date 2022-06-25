use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::convert::TryInto;
use std::fs::File;
use std::iter::FromIterator;
use std::rc::Rc;

use crate::parser::parse_form;
use crate::scanner::{tokenize, tokenize_file};
use crate::{
    datum::{Datum, Symbol},
    error::{TinError, TinResult},
    eval::{Evaluable, Interner},
};
mod convert;
// use convert::*;

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum List {
    Pair(Rc<TinValue>, Rc<List>),
    Nil,
}

impl List {
    pub fn new() -> Self {
        Self::Nil
    }

    pub fn is_empty(&self) -> bool {
        if let List::Pair(_, _) = self {
            false
        } else {
            true
        }
    }

    pub fn one(self) -> TinResult<TinValue> {
        if let List::Pair(car, cdr) = self {
            if cdr.is_empty() {
                return Ok(car.into());
            }
        }

        Err(TinError::GenericError(
            "expected exactly one arg".to_string(),
        ))
    }

    pub fn iter(&self) -> ListIterator {
        return Rc::new(self.clone()).into();
    }
}

impl IntoIterator for List {
    type Item = Rc<TinValue>;
    type IntoIter = ListIterator;
    fn into_iter(self) -> Self::IntoIter {
        ListIterator(Rc::new(self))
    }
}

impl From<Rc<List>> for ListIterator {
    fn from(v: Rc<List>) -> Self {
        ListIterator(v)
    }
}

#[test]
fn test_from_iterator() {
    let src: Vec<TinValue> = vec![1.0.into(), 2.0.into(), 3.0.into()];
    let lst: List = src.clone().into_iter().collect();
    let mut it = lst.into_iter();
    for el in src {
        let el2 = it.next().unwrap();
        assert_eq!(&el, el2.as_ref());
    }
}

impl FromIterator<TinValue> for List {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = TinValue>,
    {
        let sink: Vec<_> = iter.into_iter().collect();
        let mut s = List::Nil;
        for el in sink.into_iter().rev() {
            s = List::Pair(Rc::new(el), Rc::new(s))
        }
        s
    }
}

pub struct ListIterator(Rc<List>);

impl Iterator for ListIterator {
    type Item = Rc<TinValue>;

    fn next(&mut self) -> Option<Self::Item> {
        let s = Rc::clone(&self.0);
        match s.as_ref() {
            List::Nil => None,
            List::Pair(car, cdr) => {
                self.0 = Rc::clone(cdr);
                Some(Rc::clone(car))
            }
        }
    }
}

pub type Ident = usize; // Interned symbol

#[derive(Debug)]
pub struct Mapping {
    env: HashMap<Ident, TinValue>,
    parent: Option<Environment>,
}

impl Mapping {
    pub fn new() -> Self {
        Mapping {
            env: HashMap::new(),
            parent: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment(Rc<RefCell<Mapping>>);

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

// Rc<RefCell<Mapping>>
impl Environment {
    pub fn new() -> Self {
        Environment(Rc::new(RefCell::new(Mapping::new())))
    }

    pub fn fetch(&self, sym: &Ident) -> Option<TinValue> {
        self.0
            .as_ref()
            .borrow()
            .env
            .get(sym)
            .map(TinValue::clone)
            .or_else(|| {
                let parent = &self.0.as_ref().borrow().parent;
                parent.as_ref().and_then(|p| p.fetch(sym))
            })
    }
    pub fn put(&mut self, sym: Ident, val: TinValue) {
        self.0.borrow_mut().env.insert(sym, val);
    }

    pub fn make_child(&self) -> Self {
        let mut child = Mapping::new();
        child.parent = Some(Environment(Rc::clone(&self.0)));

        Environment(Rc::new(RefCell::new(child)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    formals: Vec<Ident>,
    va_sink: Option<Ident>,
    body: Vec<TinValue>,
    upvalues: Option<Environment>,
}

pub struct Set<T>(T);
pub struct Unset;

impl<T> Set<T> {
    fn new(t: T) -> Self {
        Set(t)
    }
    fn into_inner(self) -> T {
        self.0
    }
}

#[derive(Debug)]
pub struct LambdaBuilder<T1, T2> {
    formals: T1,
    body: T2,
    sink: Option<Ident>,
    env: Option<Environment>,
}

impl LambdaBuilder<Set<Vec<Ident>>, Set<Vec<TinValue>>> {
    pub fn build(self) -> Lambda {
        Lambda {
            formals: self.formals.into_inner(),
            va_sink: self.sink,
            body: self.body.into_inner(),
            upvalues: self.env,
        }
    }
}

impl LambdaBuilder<Unset, Set<Vec<TinValue>>> {
    pub fn build(self) -> Lambda {
        Lambda {
            formals: Vec::new(),
            va_sink: self.sink,
            body: self.body.into_inner(),
            upvalues: self.env,
        }
    }
}

impl<T1, T2> LambdaBuilder<T1, T2> {
    pub fn upvalues(mut self, env: Environment) -> LambdaBuilder<T1, T2> {
        self.env = Some(env);
        self
    }

    pub fn va_sink(mut self, id: Ident) -> LambdaBuilder<T1, T2> {
        self.sink = Some(id);
        self
    }
    pub fn formals(self, f: Vec<Ident>) -> LambdaBuilder<Set<Vec<Ident>>, T2> {
        LambdaBuilder {
            formals: Set::new(f),
            body: self.body,
            sink: self.sink,
            env: self.env,
        }
    }
    pub fn body(self, b: Vec<TinValue>) -> LambdaBuilder<T1, Set<Vec<TinValue>>> {
        LambdaBuilder {
            formals: self.formals,
            body: Set::new(b),
            sink: self.sink,
            env: self.env,
        }
    }
}

impl Lambda {
    pub fn builder() -> LambdaBuilder<Unset, Unset> {
        LambdaBuilder {
            formals: Unset,
            env: None,
            sink: None,
            body: Unset,
        }
    }
}

impl Lambda {
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

    pub fn call<I>(
        &self,
        args: Vec<TinValue>,
        i: &mut I,
        e: &mut Environment,
    ) -> TinResult<TinValue>
    where
        I: Interner,
    {
        let bl = args.len();
        let fl = self.formals.len();
        if bl < fl {
            return Err(TinError::ArityMismatch(fl, bl));
        }
        let mut env = self
            .upvalues
            .as_ref()
            .map(|e| e.clone())
            .unwrap_or_else(|| e.make_child());

        if let Some(s) = self.va_sink {
            let mut it = args.iter().map(TinValue::clone);
            for (el, sym) in it.by_ref().zip(self.formals.iter()) {
                env.put(*sym, el)
            }
            let rest = Pair::from_iter(it);
            env.put(s, rest);

            let mut last = TinValue::Nil;

            for el in self.body.iter() {
                last = el.eval_using(i, &mut env)?;
            }
            return Ok(last);
        } else {
            if bl > fl {
                return Err(TinError::ArityMismatch(fl, bl));
            }
            let it = args.iter().map(TinValue::clone);
            for (el, sym) in it.zip(self.formals.iter()) {
                env.put(*sym, el)
            }
            self.body
                .iter()
                .fold(Ok(TinValue::Nil), |_, exp| exp.eval_using(i, &mut env))
        }
    }
}

impl Evaluable for Lambda {
    fn eval_using<I>(&self, _i: &mut I, e: &mut Environment) -> TinResult<TinValue>
    where
        I: Interner,
    {
        let new = Lambda {
            formals: self.formals.clone(),
            body: self.body.clone(),
            va_sink: self.va_sink,
            upvalues: Some(e.clone()),
        };
        Ok(new.into())
    }
}

#[derive(Debug, PartialEq)]
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

impl PartialEq for TinCell {
    fn eq(&self, other: &Self) -> bool {
        let left = self.inner.take();
        let right = other.inner.take();
        let test = left == right;
        self.inner.set(left);
        other.inner.set(right);
        test
    }
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
        write!(f, "TinCell {{ inner: ... }}")
    }
}

pub struct Closure {
    f: Box<dyn Fn(List) -> TinResult<TinValue>>,
    name: Option<String>,
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Closure {
    pub fn new_with_id<F>(id: String, f: F) -> Self
    where
        F: Fn(List) -> TinResult<TinValue> + 'static,
    {
        Closure {
            f: Box::new(f),
            name: Some(id),
        }
    }
    pub fn new<F>(f: F) -> Self
    where
        F: Fn(List) -> TinResult<TinValue> + 'static,
    {
        Closure {
            f: Box::new(f),
            name: None,
        }
    }

    pub fn call(&self, args: List) -> Result<TinValue, TinError> {
        (self.f)(args)
    }
}

impl std::fmt::Debug for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(x) = &self.name {
            write!(f, "#closure({})", x)
        } else {
            write!(f, "#closure")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def(Ident, TinValue);

impl Evaluable for Def {
    fn eval_using<I>(&self, i: &mut I, e: &mut Environment) -> TinResult<TinValue>
    where
        I: Interner,
    {
        let v = self.1.eval_using(i, e)?;
        e.put(self.0, v);
        Ok(TinValue::Nil)
    }
}

impl Def {
    pub fn new(id: Ident, v: TinValue) -> Self {
        Def(id, v)
    }
}

#[derive(Debug, PartialEq)]
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
    fn eval_using<I>(&self, i: &mut I, e: &mut Environment) -> TinResult<TinValue>
    where
        I: Interner,
    {
        let cmd = self.cmd.eval_using(i, e)?;
        match cmd {
            TinValue::Closure(c) => {
                let args = self
                    .body
                    .iter()
                    .map(|x| x.eval_using(i, e))
                    .collect::<TinResult<_>>()?;
                c.call(args)
            }

            TinValue::Lambda(l) => l.call(self.body.clone(), i, e),
            TinValue::Native(f) => f.call(self.body.clone(), i, e),
            _ => Err(TinError::NotAProc(Box::new(Datum::Nil))),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Native {
    DoFile,
    OpenFile,
}

impl Native {
    pub fn call<I: Interner>(
        &self,
        args: Vec<TinValue>,
        i: &mut I,
        e: &mut Environment,
    ) -> TinResult<TinValue> {
        match self {
            Native::DoFile => {
                if args.len() != 1 {
                    return Err(TinError::ArityMismatch(1, args.len()));
                }
                let filename: String = args[0].clone().try_into()?;
                let f = File::open(filename).map_err(|_| TinError::IOError)?;
                let mut it = tokenize_file(f);
                loop {
                    let form = parse_form(it.by_ref());
                    match form {
                        Ok(x) => x.eval_using(i, e)?.eval_using(i, e)?,
                        Err(TinError::EOF) => return Ok(TinValue::default()),
                        Err(x) => return Err(x),
                    };
                }
            }
            Native::OpenFile => {
                if args.len() != 1 {
                    return Err(TinError::ArityMismatch(1, args.len()));
                }
                let filename: String = args[0].clone().try_into()?;
                let f = File::open(filename).map_err(|_| TinError::IOError)?;
                Ok(TinValue::Port(Rc::new(Port::new(f))))
            }
        }
    }
}

// use crate::eval::Evaluable;

// pub fn eval_datum<I: Interner>(
//     interner: &mut I,
//     env: &mut Environment,
//     d: Datum,
// ) -> TinResult<TinValue> {
//     let expr = d.eval_using(interner, env)?;
//     expr.eval_using(interner, env)
// }

// pub fn do_file<I: Interner>(
//     interner: &mut I,
//     env: &mut Environment,
//     f: String,
// ) -> TinResult<TinValue> {
//     let f = File::open(f).map_err(|_| TinError::IOError)?;
//     let mut it = tokenize_file(f);
//     let mut last = TinValue::default();
//     loop {
//         let form = parse_form(it.by_ref());
//         match form {
//             Ok(x) => {
//                 last = eval_datum(interner, env, x)?;
//             }
//             Err(TinError::EOF) => return Ok(last),
//             Err(e) => return Err(e),
//         }
//     }
// }
#[derive(Debug)]
pub struct Port(Box<File>);

impl Port {
    fn new(f: File) -> Self {
        Port(Box::new(f))
    }
}

impl PartialEq for Port {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    List(Rc<List>),

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

    Native(Rc<Native>),

    Port(Rc<Port>),

    Environment(Rc<Environment>),
}

impl Default for TinValue {
    fn default() -> Self {
        Self::Nil
    }
}

impl TinValue {
    pub fn truthy(&self) -> bool {
        if let TinValue::Bool(b) = self {
            *b
        } else {
            true
        }
    }

    pub fn number<N: Into<f64>>(x: N) -> Self {
        Self::Number(x.into())
    }

    pub fn to_string<I: Interner>(&self, i: &I) -> String {
        match self {
            TinValue::Symbol(s) => i.unintern(*s).to_string(),
            TinValue::Number(n) => n.to_string(),
            TinValue::Bool(n) => n.to_string(),
            TinValue::Char(n) => n.to_string(),
            TinValue::Vector(_) => todo!(),
            TinValue::String(s) => format!("\"{}\"", s),
            TinValue::Bytes(_) => todo!(),
            TinValue::Quote(q) => format!("'{}", q),
            TinValue::Cell(_) => todo!(),
            TinValue::Pair(p) => format!("({} . {})", p.car.to_string(i), p.cdr.to_string(i)),
            TinValue::List(s) => format!(
                "({})",
                s.iter()
                    .map(|x| x.to_string(i))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            TinValue::App(a) => format!(
                "({} {})",
                a.cmd.to_string(i),
                a.body
                    .clone()
                    .into_iter()
                    .map(|x| x.to_string(i))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            TinValue::Lambda(_) => format!("#lambda"),
            TinValue::If(x) => {
                if let Some(v) = &x.altern {
                    format!(
                        "(if {} {} {})",
                        x.test.to_string(i),
                        x.conseq.to_string(i),
                        v.to_string(i)
                    )
                } else {
                    format!("(if {} {})", x.test.to_string(i), x.conseq.to_string(i))
                }
            }
            TinValue::Def(d) => format!("(define {} {})", i.unintern(d.0), d.1.to_string(i)),
            TinValue::Nil => format!("()"),
            TinValue::Closure(c) => format!("#closure({:?})", c.name),
            TinValue::Exception(_) => todo!(),
            TinValue::Native(_) => todo!("#native"),
            TinValue::Port(_) => todo!("#port"),
            TinValue::Environment(_) => format!("#environment"),
        }
    }
}

macro_rules! self_evaluating {
    ($t:ty => $var:ident) => {
        impl Evaluable for $t {
            fn eval_using<I>(&self, _i: &mut I, _e: &mut Environment) -> TinResult<TinValue>
            where
                I: Interner,
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
    fn eval_using<I>(&self, _i: &mut I, _e: &mut Environment) -> TinResult<TinValue>
    where
        I: Interner,
    {
        Ok(TinValue::String(Rc::clone(self)))
    }
}

impl Evaluable for If {
    fn eval_using<I>(&self, i: &mut I, e: &mut Environment) -> TinResult<TinValue>
    where
        I: Interner,
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
    fn eval_using<I>(&self, i: &mut I, e: &mut Environment) -> TinResult<TinValue>
    where
        I: Interner,
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
            TinValue::Lambda(x) => x.eval_using(i, e),
            TinValue::Number(n) => n.eval_using(i, e),
            TinValue::Bool(x) => x.eval_using(i, e),
            TinValue::Char(x) => x.eval_using(i, e),
            TinValue::Vector(x) => Ok(TinValue::Vector(Rc::clone(x))),
            TinValue::String(x) => x.eval_using(i, e),
            TinValue::Bytes(x) => Ok(TinValue::Bytes(Rc::clone(x))),
            TinValue::Cell(x) => Ok(TinValue::Cell(Rc::clone(x))),
            TinValue::Pair(x) => Ok(TinValue::Pair(Rc::clone(x))),
            TinValue::List(x) => Ok(TinValue::List(Rc::clone(x))),
            TinValue::Nil => Ok(TinValue::Nil),
            TinValue::Closure(x) => Ok(TinValue::Closure(Rc::clone(x))),
            TinValue::Native(x) => Ok(TinValue::Native(Rc::clone(x))),
            TinValue::Port(x) => Ok(TinValue::Port(Rc::clone(x))),
            TinValue::Environment(x) => Ok(TinValue::Environment(Rc::clone(x))),
        }
    }
}

fn quote<I>(d: Rc<Datum>, i: &mut I, e: &mut Environment) -> TinResult<TinValue>
where
    I: Interner,
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
    fn eval_using<I>(&self, i: &mut I, _e: &mut Environment) -> TinResult<TinValue>
    where
        I: Interner,
    {
        Ok(TinValue::Symbol(i.intern(&self)))
    }
}
