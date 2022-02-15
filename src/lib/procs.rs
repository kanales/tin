use crate::lib::types::{Atom, Exp, Number, SchemeError, SchemeResult};

fn ensure_arity(want: usize, args: &[Exp]) -> SchemeResult<()> {
    if args.len() != want {
        Err(SchemeError::ArityMismatch(want, args.len()))
    } else {
        Ok(())
    }
}

macro_rules! to_number {
    ($e:expr) => {
        match $e {
            Exp::Atom(Atom::Number(n)) => n,
            _ => {
                return Err(SchemeError::TypeMismatch(
                    "Number".to_string(),
                    format!("{:?}", $e),
                ))
            }
        }
    };
}

pub fn add(args: &[Exp]) -> SchemeResult<Exp> {
    let mut acc = Number::Int(0);
    for el in args {
        acc = acc + to_number!(*el);
    }
    Ok(Exp::Atom(Atom::Number(acc)))
}

pub fn sub(args: &[Exp]) -> SchemeResult<Exp> {
    let mut acc = if args.len() < 1 {
        return Err(SchemeError::ArityMismatch(1, 0));
    } else {
        to_number!(args[0])
    };
    for el in &args[1..] {
        acc = acc - to_number!(*el);
    }
    Ok(Exp::Atom(Atom::Number(acc)))
}
pub fn mul(args: &[Exp]) -> SchemeResult<Exp> {
    let mut acc = Number::Int(1);
    for el in args {
        acc = acc * to_number!(*el);
    }
    Ok(Exp::Atom(Atom::Number(acc)))
}

pub fn div(args: &[Exp]) -> SchemeResult<Exp> {
    let mut acc = if args.len() < 1 {
        return Err(SchemeError::ArityMismatch(1, 0));
    } else {
        to_number!(args[0])
    };
    for el in &args[1..] {
        acc = acc / to_number!(*el);
    }
    Ok(Exp::Atom(Atom::Number(acc)))
}

pub fn abs(args: &[Exp]) -> SchemeResult<Exp> {
    ensure_arity(1, args)?;
    let n = to_number!(args[0]);
    Ok(Exp::Atom(Atom::Number(n.abs())))
}
//                 "abs" => {
//                     unimplemented!()
//                 }
