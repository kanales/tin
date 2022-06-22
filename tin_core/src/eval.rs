use std::convert::TryInto;
use std::fmt::Debug;
use std::rc::Rc;

use crate::datum::{Datum, Symbol};
use crate::error::{TinError, TinResult};
use crate::value::{App, Def, Ident, If, Lambda, Pair, TinCell, TinValue};
use crate::{ensure_arity, ensure_symbol};

pub trait Environ: Debug {
    fn fetch(&self, sym: &Ident) -> Option<TinValue>;
    fn put(&mut self, sym: Ident, val: TinValue);
    fn make_child(s: &Self) -> Self;
}
pub trait Evaluable {
    fn eval_using<I, E>(&self, a: &mut I, b: &mut E) -> TinResult<TinValue>
    where
        I: Interner,
        E: Environ;
}

pub trait Interner {
    fn find(&self, sym: &Symbol) -> Option<Ident>;
    fn intern(&mut self, sym: &Symbol) -> Ident;
    fn unintern(&self, sym: Ident) -> &Symbol;
}

type NonEmpty<T> = Vec<T>;
fn eval_list<I, E>(list: NonEmpty<Datum>, i: &mut I, e: &mut E) -> TinResult<TinValue>
where
    I: Interner,
    E: Environ,
{
    #[allow(unused_mut)]
    match &list[0] {
        Datum::Symbol(s) => match s.as_str() {
            "define" => {
                let mut forms = list.into_iter().skip(1);
                let a = forms.next().ok_or(TinError::ArityMismatch(2, 0))?;
                match a {
                    Datum::Symbol(sym) => {
                        let rest = forms.collect::<Vec<_>>();
                        if rest.len() != 1 {
                            return Err(TinError::ArityMismatch(2, rest.len()));
                        }

                        let val = rest[0].eval_using(i, e)?;
                        Ok(Def::new(i.intern(&sym), val).into())
                    }
                    Datum::List(list) => {
                        let mut it = list.into_iter();
                        let x = ensure_symbol!(it.next().unwrap());
                        let sym = i.intern(&x);
                        let formals: Vec<Ident> = it
                            .map(|x| x.eval_using(i, e).and_then(|x| x.try_into()))
                            .collect::<TinResult<_>>()?;
                        let body = forms
                            .map(|x| x.eval_using(i, e))
                            .collect::<TinResult<_>>()?;

                        let f = Lambda::new_list(formals, body);
                        Ok(Def::new(sym, f.into()).into())
                    }
                    Datum::DotList(list, tail) => {
                        let mut it = list.into_iter();
                        let x = ensure_symbol!(it.next().unwrap());
                        let sym = i.intern(&x);
                        let t: Ident = tail.eval_using(i, e)?.try_into()?;
                        let formals: Vec<Ident> = it
                            .map(|x| x.eval_using(i, e).and_then(|x| x.try_into()))
                            .collect::<TinResult<_>>()?;
                        let body = forms
                            .map(|x| x.eval_using(i, e))
                            .collect::<TinResult<_>>()?;

                        let f = Lambda::new_dotted(formals, t, body);
                        Ok(Def::new(sym, f.into()).into())
                    }
                    x => Err(TinError::NotFormals(Box::new(x))),
                }
            }
            "begin" => {
                let mut last = TinValue::Nil;
                for el in list.into_iter().skip(1) {
                    last = el.eval_using(i, e)?;
                }
                Ok(last)
            }
            "quote" => {
                let a: Datum = ensure_arity!(1, list.into_iter().skip(1));
                Ok(TinValue::Quote(Rc::new(a)).into())
            }
            "if" => {
                let mut it = list.into_iter().skip(1);
                let (test, conseq) = ensure_arity!(2, it.by_ref());
                if let Some(v) = it.next() {
                    Ok(If::new_with_alternative(
                        test.eval_using(i, e)?,
                        conseq.eval_using(i, e)?,
                        v.eval_using(i, e)?,
                    )
                    .into())
                } else {
                    Ok(If::new(test.eval_using(i, e)?, conseq.eval_using(i, e)?).into())
                }
            }
            "set!" => unimplemented!(),
            "lambda" => {
                let mut it = list.into_iter().skip(1);
                match it.next() {
                    None => Err(TinError::ArityMismatch(2, 0)),
                    Some(Datum::Symbol(s)) => {
                        let sym = i.intern(&s);
                        let body: Vec<TinValue> =
                            it.map(|el| el.eval_using(i, e)).collect::<TinResult<_>>()?;
                        Ok(Lambda::new_symbol(sym, body).into())
                    }

                    Some(Datum::List(s)) => {
                        let formals: Vec<Ident> = s
                            .into_iter()
                            .map(|el| el.eval_using(i, e).and_then(|e| e.try_into()))
                            .collect::<TinResult<_>>()?;
                        let body: Vec<TinValue> =
                            it.map(|el| el.eval_using(i, e)).collect::<TinResult<_>>()?;
                        Ok(Lambda::new_list(formals, body).into())
                    }
                    Some(Datum::DotList(s, t)) => {
                        if let Datum::Symbol(sym) = t.as_ref() {
                            let id = i.intern(sym);
                            let formals: Vec<Ident> = s
                                .into_iter()
                                .map(|el| el.eval_using(i, e).and_then(|e| e.try_into()))
                                .collect::<TinResult<_>>()?;
                            let body: Vec<TinValue> =
                                it.map(|el| el.eval_using(i, e)).collect::<TinResult<_>>()?;
                            Ok(Lambda::new_dotted(formals, id, body).into())
                        } else {
                            Err(TinError::NotASymbol(t))
                        }
                    }
                    Some(x) => Err(TinError::NotFormals(Box::new(x))),
                }
            }
            _ => {
                let sym = i.intern(&s);
                match e.fetch(&sym) {
                    Some(v) => {
                        let exps: Vec<_> = list
                            .into_iter()
                            .skip(1)
                            .map(|d| d.eval_using(i, e))
                            .collect::<TinResult<_>>()?;
                        Ok(App::new(v.into(), exps).into())
                    }
                    None => Err(TinError::UndefinedSymbol(sym.to_string())),
                }
            }
        },
        Datum::List(lst) => {
            let cmd = eval_list(lst.clone(), i, e)?;
            let exps: Vec<_> = list
                .into_iter()
                .skip(1)
                .map(|d| d.eval_using(i, e))
                .collect::<TinResult<_>>()?;
            Ok(App::new(cmd.into(), exps).into())
        }
        x @ (Datum::Bool(_)
        | Datum::Number(_)
        | Datum::Char(_)
        | Datum::String(_)
        | Datum::Bytes(_)
        | Datum::Nil
        | Datum::Vector(_)
        | Datum::DotList(_, _)) => Err(TinError::NotAProc(Box::new(x.clone()))),
    }
}

impl Evaluable for Datum {
    fn eval_using<I, E>(&self, i: &mut I, e: &mut E) -> TinResult<TinValue>
    where
        I: Interner,
        E: Environ,
    {
        match self {
            Datum::Bool(b) => b.eval_using(i, e),
            Datum::Number(n) => n.eval_using(i, e),
            Datum::Char(c) => c.eval_using(i, e),
            Datum::String(s) => Ok(TinValue::String(Rc::new(s.clone()))),
            Datum::Symbol(s) => s.eval_using(i, e),
            Datum::Bytes(bs) => Ok(TinValue::Bytes(Rc::new(bs.clone()))),
            Datum::Nil => Ok(TinValue::Nil.into()),
            Datum::List(lst) => eval_list(lst.clone(), i, e),
            Datum::DotList(lefts, right) => {
                if lefts.is_empty() {
                    panic!("ill formed dotted list.")
                }
                let right = right.eval_using(i, e);
                lefts.into_iter().rev().fold(right, |a, el| {
                    Ok(Pair::new(el.eval_using(i, e)?, a?).into())
                })
            }
            Datum::Vector(v) => {
                let vals: Vec<TinCell> = v
                    .into_iter()
                    .map(|el| el.eval_using(i, e).map(TinCell::new))
                    .collect::<TinResult<_>>()?;
                Ok(vals.into())
            }
        }
    }
}
