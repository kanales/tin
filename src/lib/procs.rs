use std::convert::{TryFrom, TryInto};

use crate::lib::types::{Exp, Number, TinError, TinResult};

use super::{types::List, utils};

fn ensure_arity<T>(want: usize, args: &[T]) -> TinResult<()> {
    if args.len() != want {
        TinError::ArityMismatch(want, args.len()).into()
    } else {
        Ok(())
    }
}
pub fn add(args: List) -> TinResult<Exp> {
    let mut acc = Number::Int(0);
    for el in args.iter() {
        acc = acc + Number::try_from(el)?;
    }
    Ok(Exp::Number(acc))
}

pub fn sub(args: List) -> TinResult<Exp> {
    match args.snoc() {
        Some((h, tail)) => {
            let mut acc = h.clone().try_into()?;
            for el in tail.iter() {
                acc = acc - el.try_into()?;
            }

            Ok(Exp::Number(acc))
        }
        _ => TinError::ArityMismatch(1, 0).into(),
    }
}
pub fn mul(args: List) -> TinResult<Exp> {
    let mut acc = Number::Int(1);
    for el in args.iter() {
        acc = acc * el.try_into()?;
    }
    Ok(Exp::Number(acc))
}

pub fn min(args: List) -> TinResult<Exp> {
    let mut acc = Number::Float(f64::MAX);
    for el in args.iter() {
        let el = el.try_into()?;
        acc = if el < acc { el } else { acc };
    }
    Ok(Exp::Number(acc))
}
pub fn max(args: List) -> TinResult<Exp> {
    let mut acc = Number::Float(f64::MIN);
    for el in args.iter() {
        let el = el.try_into()?;
        acc = if el > acc { el } else { acc };
    }
    Ok(Exp::Number(acc))
}

pub fn div(args: List) -> TinResult<Exp> {
    match args.snoc() {
        Some((h, tail)) => {
            let mut acc = h.clone().try_into()?;
            for el in tail.iter() {
                acc = acc / el.try_into()?;
            }

            Ok(Exp::Number(acc))
        }
        _ => TinError::ArityMismatch(1, 0).into(),
    }
}

pub fn abs(args: List) -> TinResult<Exp> {
    let n: Number = utils::list1(args)?.try_into()?;
    Ok(Exp::Number(n.abs()))
}

pub fn eq(args: List) -> TinResult<Exp> {
    let (a, b) = utils::list2(args)?;
    Ok(Exp::Bool(match (a.try_into()?, b.try_into()?) {
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
    }))
}

macro_rules! ord_op {
    ($id:ident) => {
        pub fn $id(args: List) -> TinResult<Exp> {
            let (a, b) = utils::list2(args)?;
            let a: Number = a.try_into()?;
            let b: Number = b.try_into()?;
            let res = a.$id(&b);

            Ok((Exp::Bool(res)))
        }
    };
}

ord_op!(le);
ord_op!(ge);
ord_op!(lt);
ord_op!(gt);
