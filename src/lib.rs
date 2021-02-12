#![feature(box_syntax)]

mod env;
mod error;
mod interpreter;
mod lime_std;
mod parser;
mod repl;
mod resolver;
mod value;

pub use error::{Error, LimeError, Result};
pub use interpreter::Interpreter;
pub use repl::repl;
pub use value::{Func, Value};
