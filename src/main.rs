#[macro_use]
extern crate simple_error;
use std::io;
use std::io::prelude::*;
use std::time::{Duration, Instant};

pub mod expr;
pub mod lexer;
pub mod parser;

pub fn main() {
    for input in io::stdin().lock().lines() {
        let input = input.unwrap();

        let pre_parse = Instant::now();
        let parsed = parser::parse_str(&input).unwrap();
        let parse_dur = pre_parse.elapsed();

        let pre_eval = Instant::now();
        let evaluated = parsed.eval();
        let eval_dur = pre_eval.elapsed();

        println!("{:?}", evaluated);
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
