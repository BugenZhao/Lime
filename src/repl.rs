use crate::interpreter::Interpreter;
use colored::*;
use rustyline::{error::ReadlineError, Editor};

pub fn repl(intp: Interpreter) {
    let mut rl = Editor::<()>::new();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(mut line) => {
                rl.add_history_entry(&line);
                line.push(';');
                match intp.eval(&line) {
                    Ok(Some(val)) => println!("{:?}", val),
                    Ok(None) => {}
                    Err(err) => println!("{}", err.to_string().red()),
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Readline error: {}", err.to_string().red());
                break;
            }
        }
    }
}
