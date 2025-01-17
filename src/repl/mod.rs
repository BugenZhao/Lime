mod helper;

use crate::{interpreter::Interpreter, Value};
use colored::*;
use rustyline::error::ReadlineError;

pub fn repl(mut intp: Interpreter) {
    let mut rl = helper::editor();
    let mut counter = 0u64;

    println!(
        "{} {} ({})\nType `:help` for more information.\n",
        "Lime".bright_green().bold(),
        env!("CARGO_PKG_VERSION"),
        option_env!("RUSTUP_TOOLCHAIN").unwrap_or("?")
    );

    loop {
        rl.helper_mut().unwrap().hints = intp.hints();
        counter = counter.saturating_add(1);
        let prompt = format!("[{}]>> ", counter);

        match rl.readline(&prompt) {
            Ok(line) if line.starts_with(':') => match line.as_str() {
                ":ls" => println!("{:#?}", intp.global_map()),
                ":help" => println!("Type `:ls` to check the global name map."),
                _ => {}
            },
            Ok(mut line) => {
                line.push(';');
                match intp.eval_with_name(&format!("<repl:{}>", counter), &line) {
                    Ok(Value::Nil(None)) => println!(),
                    Ok(val) => println!("{:?}", val),
                    Err(err) => println!("{}", intp.fmt_error(err)),
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
