mod lib;

use std::io::Read;

use crate::lib::Tin;

const PROMPT: &'static str = "λ> ";
fn main() {
    let mut tin = Tin::new();

    eprintln!(
        "{}",
        concat!(
            "Press Ctrl+C to exit\n",
            "note: `tin` uses readline under the hood, consider using\n",
            "the external editor (Ctrl+^ by default) for a better experience\n"
        )
    );
    eprint!("{}", PROMPT);
    loop {
        let mut buffer = String::new();
        match std::io::stdin().read_to_string(&mut buffer) {
            Ok(n) => {
                if n == 0 {
                    return;
                }
                eprint!("  \r"); // clear
                match tin.eval_str(&buffer) {
                    Ok(r) => {
                        if !r.is_null() {
                            eprintln!("{}   ", r)
                        }
                    }
                    Err(e) => eprintln!("\x1b[31mError:\x1b[0m {}", e),
                }
                eprint!("{}", PROMPT);
            }
            Err(error) => println!("\x1b[31mError:\x1b[0m {}", error),
        }
    }
}
