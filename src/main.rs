mod lib;

use crate::lib::Tin;

fn main() {
    let mut tin = Tin::new();
    eprint!("> ");
    loop {
        let mut buffer = String::new();
        match std::io::stdin().read_line(&mut buffer) {
            Ok(_) => {
                match tin.eval_str(&buffer) {
                    Ok(r) => {
                        if !r.is_null() {
                            eprintln!("{}", r)
                        }
                    }
                    Err(e) => eprintln!("Error: {:?}", e),
                }
                eprint!("> ");
            }
            Err(error) => println!("error: {}", error),
        }
    }
}
