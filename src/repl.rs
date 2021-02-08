use crate::interpreter::Interpreter;
use rustyline::{error::ReadlineError, Editor};

pub fn repl() {
    let mut rl = Editor::<()>::new();
    let intp = Interpreter::new();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(mut line) => {
                rl.add_history_entry(&line);
                line.push(';');
                match intp.eval(&line) {
                    Ok(Some(val)) => println!("{:?}", val),
                    Ok(None) => {}
                    Err(err) => println!("Error: {:?}", err),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
