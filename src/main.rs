use colored::*;
use lime::{repl, Interpreter};
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, structopt::StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str), help = "Lime program file")]
    input: Option<PathBuf>,
    #[structopt(short = "c", long = "continue", help = "Continue after program exited")]
    continue_: bool,
}

fn main() {
    let intp = Interpreter::new();
    let opt = Opt::from_args();
    if let Some(path) = opt.input {
        if let Err(e) = intp.eval_file(path) {
            println!("{}", e.to_string().red());
        }
        if opt.continue_ {
            repl(intp);
        }
    } else {
        repl(intp);
    }
}
