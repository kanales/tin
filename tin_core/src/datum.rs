// #![rustfmt::skip::macros(datum)]
use std::convert::TryFrom;
use std::fmt;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Symbol(String);

impl Symbol {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

use crate::error::{TinError, TinResult};
use crate::scanner;

impl Symbol {
    pub fn try_new(s: &str) -> TinResult<Self> {
        if Symbol::is_symbol(s) {
            Ok(Symbol(s.to_string()))
        } else {
            Err(TinError::SymbolExpected(s.to_string()))
        }
    }

    pub fn is_symbol(s: &str) -> bool {
        let mut cs = s.chars();
        if let Some(c) = cs.next() {
            if scanner::is_initial(&c) {
                return cs.all(|x| scanner::is_subsequent(&x));
            }
            if c == '|' {
                unimplemented!("pipe symbols are not implemented yet");
            }
            if scanner::is_explicit_sign(&c) {
                if let Some(c) = cs.next() {
                    if c == '.' {
                        if let Some(c) = cs.next() {
                            return scanner::is_dot_subsequent(&c)
                                && cs.all(|c| scanner::is_subsequent(&c));
                        }
                    }

                    return scanner::is_sign_subsequent(&c)
                        && cs.all(|c| scanner::is_subsequent(&c));
                }
                return true;
            }

            if c == '.' {
                if let Some(c) = cs.next() {
                    return scanner::is_dot_subsequent(&c)
                        && cs.all(|c| scanner::is_subsequent(&c));
                }
            }
        }
        false
    }
}

impl TryFrom<String> for Symbol {
    type Error = TinError;
    fn try_from(s: String) -> TinResult<Self> {
        if Self::is_symbol(&s) {
            Ok(Symbol(s.to_string()))
        } else {
            Err(TinError::SyntaxError(s))
        }
    }
}

impl TryFrom<&str> for Symbol {
    type Error = TinError;
    fn try_from(s: &str) -> TinResult<Self> {
        if Self::is_symbol(s) {
            Ok(Symbol(s.to_string()))
        } else {
            Err(TinError::SymbolExpected(s.to_string()))
        }
    }
}

#[macro_export]
macro_rules! ensure_symbol {
    ($sym:expr) => {{
        use std::convert::TryFrom;
        Symbol::try_from($sym)?
    }};
}

mod list {

    use core::fmt;
    use std::iter::FromIterator;

    use crate::datum::Datum;
    #[derive(Debug, PartialEq)]
    struct Cell {
        car: Box<Datum>,
        cdr: Option<Box<Cell>>,
    }

    #[derive(Debug, PartialEq)]
    pub struct List(Option<Cell>);

    impl List {
        pub fn cons(self, d: Datum) -> Self {
            List(Some(Cell {
                car: Box::new(d),
                cdr: self.0.map(Box::new),
            }))
        }
        pub fn singleton(d: Datum) -> Self {
            List(Some(Cell {
                car: Box::new(d),
                cdr: None,
            }))
        }
        pub fn nil() -> Self {
            List(None)
        }

        pub fn car(&self) -> Option<&Datum> {
            self.0.as_ref().map(|x| &*x.car)
        }
        // pub fn cdr(&self) -> Option<&List> {
        //     self.0.as_ref().and_then(|cell| {
        //         let c = cell.cdr.as_ref()?;
        //         Some(&List(Some(*c)))
        //     })
        // }

        pub fn pair(car: Datum, cdr: Datum) -> Self {
            let cell = Cell {
                car: Box::new(car),
                cdr: Some(Box::new(Cell {
                    car: Box::new(cdr),
                    cdr: None,
                })),
            };
            List(Some(cell))
        }

        pub fn from_reversed<I>(src: I) -> Self
        where
            I: IntoIterator<Item = Datum>,
        {
            let mut cell = None;
            for d in src.into_iter() {
                cell = Some(Cell {
                    car: Box::new(d),
                    cdr: cell.map(Box::new),
                });
            }
            List(cell)
        }
    }
    pub struct IntoIter(Option<Cell>);
    impl Iterator for IntoIter {
        type Item = Datum;

        fn next(&mut self) -> Option<Self::Item> {
            let head = self.0.take()?;
            self.0 = head.cdr.map(|x| *x);
            Some(*head.car)
        }
    }

    impl IntoIterator for List {
        type Item = Datum;

        type IntoIter = IntoIter;

        fn into_iter(self) -> Self::IntoIter {
            todo!()
        }
    }

    pub struct IntoIterRef<'a>(Option<&'a Cell>);
    impl<'a> Iterator for IntoIterRef<'a> {
        type Item = &'a Datum;

        fn next(&mut self) -> Option<Self::Item> {
            let head = self.0.take()?;
            self.0 = head.cdr.as_deref().map(|x| &*x);
            Some(&*head.car)
        }
    }

    impl<'a> IntoIterator for &'a List {
        type Item = &'a Datum;

        type IntoIter = IntoIterRef<'a>;

        fn into_iter(self) -> Self::IntoIter {
            IntoIterRef(self.0.as_ref())
        }
    }

    // struct IntoIterMut<'a>(Option<&'a mut Cell>);
    // impl<'a> Iterator for IntoIterMut<'a> {
    //     type Item = &'a Datum;

    //     fn next(&mut self) -> Option<Self::Item> {
    //         let head = self.0.take()?;
    //         self.0 = head.cdr.as_deref().map(|x| &mut *x);
    //         Some(&*head.car)
    //     }
    // }

    // impl<'a> IntoIterator for &'a mut List {
    //     type Item = &'a Datum;

    //     type IntoIter = IntoIterMut<'a>;

    //     fn into_iter(self) -> Self::IntoIter {
    //         todo!()
    //     }
    // }

    impl fmt::Display for List {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "(")?;
            for datum in self {
                write!(f, " {}", datum)?;
            }
            write!(f, " )")
        }
    }

    impl FromIterator<Datum> for List {
        fn from_iter<T>(iter: T) -> Self
        where
            T: IntoIterator<Item = Datum>,
        {
            let datums: Vec<_> = iter.into_iter().collect();
            List::from_reversed(datums)
        }
    }
}
pub type DatumList = Vec<Datum>;
pub type Vector = Vec<Datum>;

#[derive(Debug, PartialEq, Clone)]
pub enum Datum {
    // literals
    Bool(bool),
    Number(f64),
    Char(char),
    String(String),
    Symbol(Symbol),
    Bytes(Vec<u8>),
    Nil,

    // composites
    List(Vec<Datum>),
    DotList(Vec<Datum>, Box<Datum>),
    Vector(Vector),
}

impl TryFrom<Datum> for Symbol {
    type Error = TinError;

    fn try_from(value: Datum) -> Result<Self, Self::Error> {
        if let Datum::Symbol(s) = value {
            Ok(s)
        } else {
            Err(TinError::NotASymbol(Box::new(value)))
        }
    }
}

impl fmt::Display for Datum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Datum::Nil => write!(f, "()"),
            Datum::Bool(b) => {
                if *b {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }
            Datum::Number(n) => write!(f, "{}", n),
            Datum::Char(c) => write!(f, "#\\{}", c),
            Datum::String(s) => write!(f, "\"{}\"", s),
            Datum::Symbol(s) => write!(f, "{}", s),
            Datum::Bytes(_) => unimplemented!("byte vector display"),
            Datum::List(lst) => {
                write!(f, "(")?;
                let mut idx = 0;
                for el in lst {
                    if idx > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", el)?;
                    idx += 1;
                }
                write!(f, ")")
            }
            Datum::DotList(args, tail) => {
                write!(f, "(")?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, " . {} )", tail)
            }
            Datum::Vector(v) => {
                write!(f, "#(")?;
                for el in v.iter() {
                    write!(f, " {}", el)?;
                }
                write!(f, " )")
            }
        }
    }
}

macro_rules! from_impl {
    ( $([$f:ty => $var:path]),* ) => {
        $(
        impl From<$f> for Datum {
          fn from(c: $f) -> Self {
            $var(c.into())
          }
        }
        )*
    };
}

from_impl! {
   [char => Datum::Char],

   [f64 => Datum::Number],
   [f32 => Datum::Number],
   [i32 => Datum::Number],
   [i16 => Datum::Number],
   [i8 => Datum::Number],

   [bool => Datum::Bool],
   [Symbol => Datum::Symbol]
}

impl Datum {
    pub fn string(s: String) -> Self {
        Datum::String(s)
    }

    pub fn symbol(s: Symbol) -> Self {
        Datum::Symbol(s)
    }

    pub fn list(lst: Vec<Datum>) -> Self {
        Datum::List(lst)
    }

    pub fn dotlist(lst: Vec<Datum>, rest: Datum) -> Self {
        Datum::DotList(lst, Box::new(rest))
    }

    pub fn is_list(&self) -> bool {
        if let Datum::List(_) = self {
            true
        } else {
            false
        }
    }
}

#[macro_export]
macro_rules! datum {
    ($t:literal) => {
        $crate::datum::Datum::from($t)
    };
    ($t:ident) => {
        $crate::datum::Datum::symbol($crate::symbol!($t))
    };
    ([$($t:tt)+]) => {
        $crate::datum::Datum::symbol( $crate::symbol!($($t)+))
    };
    (([$($t:tt)*] . [$($s:tt)+])) => {{
        let t = vec!($(datum!($t),)*);
        let s = $crate::symbol!($($s)*);
        $crate::datum::Datum::DotList(t, Box::new(s))
    }};
    (($($t:tt)*)) => {{
        let t = vec!($(datum!($t),)*);
        $crate::datum::Datum::list(t)
    }};


}

#[macro_export]
macro_rules! list {
    ( $( $e:expr,)* ) => {{
        $crate::datum::List::from_reversed(vec![$($e,)*].into_iter().rev())
    }};
    ( $( $e:expr),* ) => {
        $crate::list![$($e,)*]
    };
}

#[macro_export]
macro_rules! symbol {
    ($($t:tt)+) => {
        $crate::datum::Symbol::try_new(concat!($(stringify!($t),)+ )).unwrap().into()
    };
}
