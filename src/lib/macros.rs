// use crate::lib::types::{Atom, Exp, Number, TinError, TinResult};

#[macro_export]
macro_rules! to_number {
    ($e:expr) => {
        match $e {
            Exp::Atom(Atom::Number(n)) => n,
            _ => {
                return Err(TinError::TypeMismatch(
                    "Number".to_string(),
                    format!("{:?}", $e),
                ))
            }
        }
    };
}
