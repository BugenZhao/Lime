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
    #[structopt(parse(from_os_str), help = "Program file")]
    input: Option<PathBuf>,
    #[structopt(short = "c", long = "continue", help = "Continue after program exited")]
    cont: bool,
}

fn main() -> Result<()> {
    let intp = Interpreter::new();
    let opt = Opt::from_args();
    if let Some(path) = opt.input {
        if let Err(e) = intp.eval(&read_to_string(path)?) {
            println!("{}", e.to_string().red());
        }
        if opt.cont {
            repl(intp);
        }
    } else {
        repl(intp);
    }

    Ok(())
}
