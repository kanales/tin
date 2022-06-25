use std::convert::TryInto;
use std::fs::File;
use std::io::{stdin, BufRead};

use tin_core::datum;
use tin_core::def_closure;
use tin_core::error::TinError;
use tin_core::parser;
use tin_core::scanner;
use tin_core::vm::TinState;

fn repl() {
    let mut acc = String::new();
    let mut state = TinState::new();

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

fn main() {
    let mut args = std::env::args();
    args.next();

    if let Some(file) = args.next() {
        let f = File::open(file).unwrap();
        let mut state = TinState::new();
        state.do_file(f).unwrap();
    } else {
        repl()
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
