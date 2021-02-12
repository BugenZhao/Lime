use std::{fs::read_to_string, path::Path, rc::Rc};

use crate::{
    env::{Env, Eval},
    error::Result,
};
use crate::{parser, Value};

pub struct Interpreter {
    env: Rc<Env>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Env::new_global(),
        }
    }
}

impl Interpreter {
    pub fn eval_file<P: AsRef<Path>>(&self, path: P) -> Result<Value> {
        self.eval(&read_to_string(path)?)
    }

    pub fn eval(&self, text: &str) -> Result<Value> {
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
        let intp = Interpreter::new();
        let _r = intp.eval("var a = 1 + 2 * 3;").unwrap();
        assert_eq!(intp.env.get(&Ident("a".to_owned(), None)).unwrap(), Value::Int(7));
    }
}
