#![feature(box_syntax)]
#![feature(bindings_after_at)]

mod ast;
mod env;
mod error;
mod interpreter;
mod lime_std;
mod parser;
mod repl;
mod resolver;
mod value;

pub use error::{ErrType, Result};
pub use interpreter::Interpreter;
pub use parser::{parse_and_resolve, KEYWORDS};
pub use repl::repl;
pub use value::{Func, Value};
