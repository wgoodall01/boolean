#[macro_use]
extern crate simple_error;

pub mod expr;
pub mod lexer;
pub mod parser;
pub mod truth_table;

pub fn eval_str(input: String) -> String {
    let parsed = match parser::parse_str(&input) {
        Ok(parsed) => parsed,
        Err(err) => {
            return format!("invalid: {}\n", err);
        }
    };

    let evaluated = parsed.eval();
    format!("{}", evaluated)
}
