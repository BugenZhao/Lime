use crate::{env::Env, error::Result, parser, parser::Stmt, Value};
use std::{collections::HashMap, fs::read_to_string, path::Path, rc::Rc};

pub struct Interpreter {
    env: Rc<Env>,
}

#[allow(clippy::new_without_default)]
impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Env::new_global_std(),
        }
    }
}

impl Interpreter {
    pub fn eval_file<P: AsRef<Path>>(&self, path: P) -> Result<Value> {
        self.eval(&read_to_string(path)?)
    }

    pub fn eval(&self, text: &str) -> Result<Value> {
        let stmts = parser::parse_and_resolve(text)?;
        self.env.eval_stmts(&stmts)
    }

    pub fn eval_stmts(&self, stmts: &[Stmt]) -> Result<Value> {
        self.env.eval_stmts(&stmts)
    }

    pub fn hints(&self) -> Vec<String> {
        let mut r = self.env.names();
        r.append(&mut parser::KEYWORDS.clone());
        r.drain(..).filter(|n| !n.starts_with('_')).collect()
    }

    pub fn global_map(&self) -> HashMap<String, Value> {
        self.env.vars.borrow().clone()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::Ident;

    #[test]
    fn test() {
        let intp = Interpreter::new();
        let _r = intp.eval("var a = 1 + 2 * 3;").unwrap();
        assert_eq!(
            intp.env.get(&Ident("a".to_owned(), None)).unwrap(),
            Value::Int(7)
        );
    }
}
