use std::{fs::read_to_string, path::Path};

use crate::parser::{self, Value};
use crate::{env::Env, error::Result};

pub struct Interpreter {
    env: Env<'static>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Env::new_global(),
        }
    }
}

impl Interpreter {
    pub fn eval_file<P: AsRef<Path>>(&mut self, path: P) -> Result<Value> {
        self.eval(&read_to_string(path)?)
    }

    pub fn eval(&mut self, text: &str) -> Result<Value> {
        let stmts = parser::parse(text)?;
        self.env.eval_stmts(&stmts)
    }
}

#[cfg(test)]
mod test {
    use crate::parser::Ident;

    use super::*;

    #[test]
    fn test() {
        let mut intp = Interpreter::new();
        let _r = intp.eval("var a = 1 + 2 * 3;").unwrap();
        assert_eq!(intp.env.get(&Ident("a".to_owned())).unwrap(), Value::Int(7));
    }
}
