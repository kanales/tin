use crate::{datum::Datum, value::TinValue};

#[derive(Debug, PartialEq)]
pub enum TinError {
    SyntaxError(String),
    SymbolExpected(String),
    IntegerExpected(Box<Datum>),
    ArityMismatch(usize, usize), // (want, got)
    NotAProc(Box<Datum>),
    NotASymbol(Box<Datum>),
    NotANumber,
    NotFormals(Box<Datum>),
    NotAnIdentifier(Box<TinValue>),
    NotAString(Box<TinValue>),
    UndefinedSymbol(String),
    IOError,

    GenericError(String),
    EOF,
}

pub type TinResult<T> = Result<T, TinError>;

impl<T> From<TinError> for TinResult<T> {
    fn from(e: TinError) -> Self {
        Err(e)
    }
}

#[macro_export]
macro_rules! ensure_head {
    ($vec:expr) => {{
        let v = $vec;
        (v[0], &v[1..])
    }};
}

#[macro_export]
macro_rules! ensure_arity {
    (1, $it:expr) => {{
        let mut it = $it;
        let mut c = 1;
        let mut p = || {
            let a = it.next()?;
            c += 1;
            Some(a)
        };
        match p() {
            Some(x) => x,
            None => Err(TinError::ArityMismatch(2, c))?,
        }
    }};
    (2, $it:expr) => {{
        let mut c = 0;
        let mut p = || {
            let mut it = $it;
            let a = it.next()?;
            c += 1;
            let b = it.next()?;
            c += 1;
            Some((a, b))
        };
        match p() {
            Some(x) => x,
            None => Err(TinError::ArityMismatch(2, c))?,
        }
    }};
}
