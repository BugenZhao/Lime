use crate::{interpreter::Interpreter, parser::Value};
use colored::*;
use rustyline::{error::ReadlineError, Editor};

pub fn repl(mut intp: Interpreter) {
    let mut rl = Editor::<()>::new();

    let mut ml_buf = String::new();
    loop {
        let readline = rl.readline(if ml_buf.is_empty() { ">> " } else { ".. " });
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);

                if let Some(stripped) = line.trim_end().strip_suffix("\\") {
                    ml_buf.push_str(stripped);
                    ml_buf.push('\n');
                } else {
                    ml_buf.push_str(&line);
                    ml_buf.push(';');
                    match intp.eval(&ml_buf) {
                        Ok(Value::Nil) => {}
                        Ok(val) => println!("{:?}", val),
                        Err(err) => println!("{}", err.to_string().red()),
                    }
                    ml_buf.drain(..);
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
