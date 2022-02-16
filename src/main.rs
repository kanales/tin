mod lib;

use crate::lib::types::Environment;

fn main() {
    let mut interpreter = Environment::new();
    eprint!("> ");
    loop {
        let mut buffer = String::new();
        match std::io::stdin().read_line(&mut buffer) {
            Ok(_) => {
                let exp = lib::parser::parse(&buffer).unwrap();
                let res = interpreter.eval(&exp).unwrap();
                println!("{}", res);
                eprint!("> ");
            }
            Err(error) => println!("error: {}", error),
        }
    }
}
