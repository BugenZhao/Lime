#![feature(box_syntax)]

use std::{fs::read_to_string, path::PathBuf};

use colored::*;
use interpreter::Interpreter;
use repl::repl;
use structopt::StructOpt;

use error::Result;

mod error;
mod interpreter;
mod parser;
mod repl;

#[derive(Debug, structopt::StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    input: Option<PathBuf>,
}

fn main() -> Result<()> {
    match Opt::from_args().input {
        Some(path) => {
            if let Err(e) = Interpreter::new().eval(&read_to_string(path)?) {
                println!("{}", e.to_string().red());
            }
        }
        None => {
            repl();
        }
    }
    Ok(())
}
