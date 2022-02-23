use std::convert::{TryFrom, TryInto};

use crate::lib::types::{Atom, Exp, Number, TinError, TinResult};

fn ensure_arity<T>(want: usize, args: &[T]) -> TinResult<()> {
    if args.len() != want {
        Err(TinError::ArityMismatch(want, args.len()))
    } else {
        Ok(())
    }
}
pub fn add(args: &[Exp]) -> TinResult<Exp> {
    let mut acc = Number::Int(0);
    for el in args {
        acc = acc + Number::try_from(el)?;
    }
    Ok(Exp::Atom(Atom::Number(acc)))
}

pub fn sub(args: &[Exp]) -> TinResult<Exp> {
    let mut acc = if args.len() < 1 {
        return Err(TinError::ArityMismatch(1, 0));
    } else {
        args[0].clone().try_into()?
    };
    for el in &args[1..] {
        acc = acc - el.try_into()?;
    }
    Ok(Exp::Atom(Atom::Number(acc)))
}
pub fn mul(args: &[Exp]) -> TinResult<Exp> {
    let mut acc = Number::Int(1);
    for el in args {
        acc = acc * el.try_into()?;
    }
    Ok(Exp::Atom(Atom::Number(acc)))
}

pub fn div(args: &[Exp]) -> TinResult<Exp> {
    let mut acc = if args.len() < 1 {
        return Err(TinError::ArityMismatch(1, 0));
    } else {
        args[0].clone().try_into()?
    };
    for el in &args[1..] {
        acc = acc / el.try_into()?;
    }
    Ok(Exp::Atom(Atom::Number(acc)))
}

pub fn abs(args: &[Exp]) -> TinResult<Exp> {
    ensure_arity(1, args)?;
    let n: Number = args[0].clone().try_into()?;
    Ok(Exp::Atom(Atom::Number(n.abs())))
}

pub fn eq(args: &[Exp]) -> TinResult<Exp> {
    ensure_arity(2, args)?;
    let a = args[0].clone().try_into()?;
    let b = args[1].clone().try_into()?;
    Ok(Exp::Atom(Atom::Bool(match (a, b) {
        (Number::Int(a), Number::Int(b)) => a == b,
        (Number::Float(a), Number::Int(b)) => {
            if a.is_finite() {
                a as i64 == b
            } else {
                false
            }
        }
        (Number::Int(b), Number::Float(a)) => {
            if a.is_finite() {
                a as i64 == b
            } else {
                false
            }
        }
        (Number::Float(a), Number::Float(b)) => a == b,
    })))
}

macro_rules! ord_op {
    ($id:ident) => {
        pub fn $id(args: &[Exp]) -> TinResult<Exp> {
            ensure_arity(2, args)?;
            let a = Number::try_from(args[0].clone())?;
            let b = Number::try_from(args[1].clone())?;
            let res = a.$id(&b);

            Ok(Exp::Atom(Atom::Bool(res)))
        }
    };
}

ord_op!(le);
ord_op!(ge);
ord_op!(lt);
ord_op!(gt);
