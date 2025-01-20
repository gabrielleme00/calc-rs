mod calculator;

use calculator::*;

fn main() -> Result<(), CalcError> {
    loop {
        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {
                match calculate(input) {
                    Ok(result) => println!("{}", result),
                    Err(error) => println!("Error: {}", error),
                }
            }
            Err(error) => println!("Error: {}", error),
        }
    }
}
