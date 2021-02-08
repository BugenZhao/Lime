#![allow(dead_code)]
#![feature(box_syntax)]

use repl::repl;

mod interpreter;
mod parser;
mod repl;

fn main() {
    repl();
}
