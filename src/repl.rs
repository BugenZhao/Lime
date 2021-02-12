use crate::{interpreter::Interpreter, repl_helper::editor, Value};
use colored::*;
use rustyline::error::ReadlineError;

pub fn repl(intp: Interpreter) {
    let mut rl = editor();

    loop {
        rl.helper_mut().unwrap().hints = intp.hints();
        let readline = rl.readline(">> ");
        match readline {
            Ok(mut line) => {
                line.push(';');
                match intp.eval(&line) {
                    Ok(Value::Nil) => println!(),
                    Ok(val) => println!("{:?}", val),
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
