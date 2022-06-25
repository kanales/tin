use std::convert::TryInto;
use std::io::{stdin, BufRead};

use tin_core::datum;
use tin_core::def_closure;
use tin_core::error::TinError;
use tin_core::parser;
use tin_core::scanner;
use tin_core::vm::TinState;

fn main() {
    let mut acc = String::new();
    let mut state = TinState::new();

    def_closure!(state, "+", |vals| {
        let num = vals.into_iter().fold(Ok(0f64), |a, v| {
            let n: f64 = v.as_ref().try_into()?;
            Ok(a? + n)
        })?;
        Ok(num.into())
    });

    eprint!("> ");
    for line in stdin().lock().lines() {
        if let Ok(line) = line {
            acc.push_str(&line);

            match state.eval_str(acc.as_str()) {
                Ok(r) => eprintln!("{:?}", r),
                Err(TinError::EOF) => continue,
                Err(e) => eprintln!("{:?}", e),
            }

            acc.clear();
        }
        eprint!("> ");
    }
}

#[cfg(test)]
mod tests {
    use tin_core::datum;

    #[test]
    fn test_expand() {
        let a = datum! { [ab-cd] };
        println!("{}", a);
        let a = datum! { (1 2 3) };
        println!("{}", a);
        let a = datum! {  ([[a] 1 [b]] . [c]) };
        println!("{}", a);

        println!("{}", a);
    }
}
