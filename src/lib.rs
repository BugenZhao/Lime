#![feature(box_syntax)]

mod env;
mod error;
mod interpreter;
mod parser;
mod repl;

pub use error::Result;
pub use interpreter::Interpreter;
pub use parser::Value;
pub use repl::repl;
