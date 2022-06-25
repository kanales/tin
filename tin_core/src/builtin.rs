use std::convert::TryInto;
use std::env::args;
use std::fs::File;
use std::rc::Rc;

use crate::datum::Datum;
use crate::error::{TinError, TinResult};
use crate::eval::Interner;
use crate::parser::parse_form;
use crate::scanner::tokenize_file;
use crate::value::{App, Environment, Native, TinValue};
use crate::vm::{SymbolDict, TinState};

#[macro_export]
macro_rules! def_closure {
    ($vm:ident, $l:literal, $e:expr) => {
        $vm.define_closure(
            $crate::datum::Symbol::try_new($l).expect("expected indentifier"),
            $e,
        )
    };
}

macro_rules! fold_numbers {
    ($e:expr; $start:expr, $ff:expr) => {{
        let ff = $ff;
        let res = $e.fold(Ok($start), |a, v| {
            let n: f64 = v.as_ref().try_into()?;
            Ok(ff(a?, n))
        })?;
        Ok(res.into())
    }};
}

pub fn builtins(s: &mut TinState) {
    macro_rules! define {
        ($l:literal => $e:expr) => {
            s.define_closure(
                $crate::datum::Symbol::try_new($l).expect("expected indentifier"),
                $e,
            )
        };
    }

    define!("+" => |args| {
        fold_numbers!(args.into_iter(); 0f64, |x, y| { x + y })
    });

    define!("*" => |args| {
        fold_numbers!(args.into_iter(); 0f64, |x, y| { x * y })
    });

    define!("-" => |args| {
        let mut it = args.into_iter();
        if let Some(x) = it.next() {
            let x: f64 = x.as_ref().try_into()?;
            if let Some(y) = it.next() {
                let y: f64 = y.as_ref().try_into()?;
                fold_numbers!(it; x - y, |x, y| { x - y })
            } else {
              Ok(TinValue::number(-x))
            }
        } else {
            Err(TinError::ArityMismatch(1, 0))
        }
    });

    define!("list" => |args| {
        Ok(args.into())
    });

    define!("display" => |args| {
        let arg = args.one()?;
        println!("{:?}", arg);
        Ok(TinValue::default())
    });

    s.define(
        "load".try_into().unwrap(),
        TinValue::Native(Rc::new(Native::DoFile)),
    );
}
