use std::{
    fmt::format,
    io::{stdin, BufRead},
};

mod scanner;

fn main() {
    for line in stdin().lock().lines() {
        if let Ok(line) = line {
            let tstream = scanner::tokenize(&line);
            let debug: Vec<_> = tstream.map(|x| format!("{:?}", x)).collect();
            println!("{:?}", debug);
        }
    }
}
