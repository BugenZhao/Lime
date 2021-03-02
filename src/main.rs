use lime::{repl, Interpreter};
use std::{fs::read_to_string, path::PathBuf};
use structopt::StructOpt;

#[derive(Debug, structopt::StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str), help = "Lime program file")]
    input: Option<PathBuf>,
    #[structopt(short = "c", long = "continue", help = "Continue after program exited")]
    continue_: bool,
}

fn main() {
    let mut intp = Interpreter::new();
    let opt = Opt::from_args();
    if let Some(path) = opt.input {
        let name = path.clone().into_os_string().into_string().unwrap();
        let text = read_to_string(path).unwrap();
        if let Err(e) = intp.eval_with_name(&name, &text) {
            println!("{}", intp.fmt_error(e));
        }
        if opt.continue_ {
            repl(intp);
        }
    } else {
        repl(intp);
    }
}
