#![feature(custom_inner_attributes)]

// mod macros;
pub mod datum;
pub mod error;
pub mod eval;
pub mod parser;
pub mod scanner;
pub mod value;
pub mod vm;

#[cfg(test)]
mod tests {
    use crate::datum;
    #[test]
    fn test_symbol_expand() {
        let _s = datum! {[syntax-define]};
        let _s = datum! {([ab] [cd])};
        // let _s = symbol!( |hel-lo| );
    }
}
