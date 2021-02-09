#![feature(box_syntax)]

mod error;
mod interpreter;
mod parser;
mod repl;

pub use error::Result;
pub use interpreter::Interpreter;
pub use repl::repl;
