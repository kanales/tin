mod lib;

use crate::lib::Tin;

fn main() {
    let mut tin = Tin::new();
    eprint!("> ");
    loop {
        let mut buffer = String::new();
        match std::io::stdin().read_line(&mut buffer) {
            Ok(n) => {
                if n == 0 {
                    return;
                }
                match tin.eval_str(&buffer) {
                    Ok(r) => {
                        if !r.is_null() {
                            eprintln!("{}", r)
                        }
                    }
                    Err(e) => eprintln!("\x1b[31mError:\x1b[0m {}", e),
                }
                eprint!("> ");
            }
            Err(error) => println!("error: {}", error),
        }
    }
}
