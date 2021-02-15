#![feature(box_syntax)]
#![feature(bindings_after_at)]

mod env;
mod error;
mod interpreter;
mod lime_std;
mod parser;
mod repl;
mod repl_helper;
mod resolver;
mod value;

pub use error::{ErrType, LimeError, Result};
pub use interpreter::Interpreter;
pub use parser::parse_and_resolve;
pub use repl::repl;
pub use value::{Func, Value};
