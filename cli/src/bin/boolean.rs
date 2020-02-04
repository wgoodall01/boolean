use linefeed::{Interface, ReadResult};
use std::time::Instant;

use boolean::parser;

pub fn main() {
    let reader = Interface::new("boolean-repl").unwrap();
    reader.set_prompt("> ").unwrap();

    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        if input == ".exit" {
            break;
        }

        let pre_parse = Instant::now();
        let parsed = match parser::parse_str(&input) {
            Ok(parsed) => parsed,
            Err(err) => {
                println!("invalid: {}\n", err);
                continue;
            }
        };
        let parse_dur = pre_parse.elapsed();

        let pre_eval = Instant::now();
        let evaluated = parsed.eval();
        let eval_dur = pre_eval.elapsed();

        println!("{}", evaluated);
        if parse_dur.as_millis() + eval_dur.as_millis() > 200 {
            println!(
                "parse:{}ms eval:{}ms",
                parse_dur.as_millis(),
                eval_dur.as_millis(),
            );
        }
        println!();
    }
}
