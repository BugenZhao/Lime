use crate::{
    ast::Stmt, env::Env, error::Result, lime_std::IntpStdExt, parser, resolver::Resolver, Value,
    KEYWORDS,
};
use std::{collections::HashMap, rc::Rc};

pub struct Interpreter {
    env: Rc<Env>,
}

#[allow(clippy::new_without_default)]
impl Interpreter {
    pub fn new() -> Self {
        let mut intp = Self {
            env: Env::new_global(),
        };
        intp.define_std();
        intp
    }

    pub fn env(&self) -> &Rc<Env> {
        &self.env
    }
}

impl Interpreter {
    fn parse_and_resolve(&self, text: &str) -> Result<Vec<Stmt>> {
        let result: std::result::Result<Vec<_>, _> = parser::parse(text);
        let mut stmts = result?;
        Resolver::new_global(text).res_stmts(&mut stmts)?;
        Ok(stmts)
    }

    pub fn eval(&self, text: &str) -> Result<Value> {
        let stmts = self.parse_and_resolve(&text)?;
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
