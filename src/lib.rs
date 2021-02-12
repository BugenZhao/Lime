#![feature(box_syntax)]

mod env;
mod error;
mod interpreter;
mod lime_std;
mod parser;
mod preprocessor;
mod repl;
mod resolver;
mod value;

pub use error::{Error, LimeError, Result};
pub use interpreter::Interpreter;
pub use parser::parse_and_resolve;
pub use repl::repl;
pub use value::{Func, Value};
