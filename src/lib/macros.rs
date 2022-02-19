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

#[macro_export]
macro_rules! list {
    () => {
        $crate::lib::types::List::new()
    };

    ($($x:expr),*) => {
            List::from(vec![ $( $x, )* ])
    };

}

#[macro_export]
macro_rules! make_hash {
    () => {
        ::std::collections::HashMap::new()
    };
    {$($k:literal => $v:expr ),*} => {{
        let mut m = make_hash! {};
        $( m.insert($k.into(), $v.into()); )*
        m
    }};
}
