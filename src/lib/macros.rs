// use crate::lib::types::{Atom, Exp, Number, TinError, TinResult};

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
