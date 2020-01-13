#[macro_use]
extern crate simple_error;
use std::io;
use std::io::prelude::*;

pub mod expr;
pub mod lexer;
pub mod parser;

pub fn main() {
    for input in io::stdin().lock().lines() {
        let line = input.unwrap();
        let input = lexer::tokenize(&line).unwrap();
        let parsed = parser::parse(input).unwrap();
        println!("{:?}", parsed.eval());
    }
}
