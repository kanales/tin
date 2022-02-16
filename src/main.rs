mod lib;

use crate::lib::types::EnvironmentRef;

fn main() {
    let env = EnvironmentRef::new();
    eprint!("> ");
    loop {
        let mut buffer = String::new();
        match std::io::stdin().read_line(&mut buffer) {
            Ok(_) => {
                if let Ok(exp) = lib::parser::parse(&buffer) {
                    let res = lib::eval::eval(env.clone(), &exp);
                    match res {
                        Ok(r) => eprintln!("{}", r),
                        Err(e) => eprintln!("Error: {:?}", e),
                    }
                } else {
                    eprintln!("SyntaxError");
                }
                eprint!("> ");
            }
            Err(error) => println!("error: {}", error),
        }
    }
}
