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
        acc = acc + to_number!(*el);
    }
    Ok(Exp::Atom(Atom::Number(acc)))
}

pub fn sub(args: &[Exp]) -> TinResult<Exp> {
    let mut acc = if args.len() < 1 {
        return Err(TinError::ArityMismatch(1, 0));
    } else {
        to_number!(args[0])
    };
    for el in &args[1..] {
        acc = acc - to_number!(*el);
    }
    Ok(Exp::Atom(Atom::Number(acc)))
}
pub fn mul(args: &[Exp]) -> TinResult<Exp> {
    let mut acc = Number::Int(1);
    for el in args {
        acc = acc * to_number!(*el);
    }
    Ok(Exp::Atom(Atom::Number(acc)))
}

pub fn div(args: &[Exp]) -> TinResult<Exp> {
    let mut acc = if args.len() < 1 {
        return Err(TinError::ArityMismatch(1, 0));
    } else {
        to_number!(args[0])
    };
    for el in &args[1..] {
        acc = acc / to_number!(*el);
    }
    Ok(Exp::Atom(Atom::Number(acc)))
}

pub fn abs(args: &[Exp]) -> TinResult<Exp> {
    ensure_arity(1, args)?;
    let n = to_number!(args[0]);
    Ok(Exp::Atom(Atom::Number(n.abs())))
}

pub fn eq(args: &[Exp]) -> TinResult<Exp> {
    ensure_arity(2, args)?;
    let a = to_number!(args[0]);
    let b = to_number!(args[1]);
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
            let a = to_number!(args[0]);
            let b = to_number!(args[1]);
            let res = a.$id(&b);

            Ok(Exp::Atom(Atom::Bool(res)))
        }
    };
}

ord_op!(le);
ord_op!(ge);
ord_op!(lt);
ord_op!(gt);
