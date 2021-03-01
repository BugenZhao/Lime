use crate::{ast::Stmt, env::Env, error::Result, parse_and_resolve, Value, KEYWORDS};
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
    // pub fn eval_file<P: AsRef<Path>>(&self, path: P) -> Result<Value> {
    //     self.eval(&read_to_string(path)?)
    // }

    pub fn eval(&self, text: &str) -> Result<Value> {
        let stmts = parse_and_resolve(&text)?;
        self.eval_stmts(&stmts)
    }

    pub fn eval_stmts(&self, stmts: &[Stmt]) -> Result<Value> {
        self.env.eval_stmts(&stmts)
    }

    pub fn hints(&self) -> Vec<String> {
        let mut r = self.env.names();
        r.append(&mut KEYWORDS.clone());
        r.into_iter().filter(|n| !n.starts_with('_')).collect()
    }

    pub fn global_map(&self) -> HashMap<String, Value> {
        self.env.vars.borrow().clone()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let intp = Interpreter::new();
        let _r = intp.eval("var a = 1 + 2 * 3;").unwrap();
        assert_eq!(intp.env.get(&"a".into()).unwrap(), Value::Int(7));
    }
}
