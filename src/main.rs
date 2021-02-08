#![allow(dead_code)]
#![feature(box_syntax)]

use repl::repl;

mod interpreter;
mod parser;
mod repl;
mod error;

fn main() {
    repl();
}
