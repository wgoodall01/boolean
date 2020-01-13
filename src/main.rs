#[macro_use]
extern crate simple_error;
use std::io;
use std::io::prelude::*;

pub mod expr;
pub mod token;

pub fn main() {
    for input in io::stdin().lock().lines() {
        let line = input.unwrap();
        let input = token::tokenize(&line).unwrap();
        let parsed = expr::parse(input).unwrap();
        println!("{:?}", parsed);
    }
}
