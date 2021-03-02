use crate::{
    ast::Stmt,
    env::Env,
    error::{Error, Result},
    lime_std::IntpStdExt,
    parser,
    resolver::Resolver,
    Value, KEYWORDS,
};
use std::{collections::HashMap, rc::Rc};

pub struct Source {
    pub name: String,
    pub text: String,
}

pub struct Interpreter {
    env: Rc<Env>,
    resolver: Rc<Resolver>,
    sources: Vec<Source>,
}

#[allow(clippy::new_without_default)]
impl Interpreter {
    pub fn new() -> Self {
        let mut intp = Self {
            env: Env::new_global(),
            resolver: Resolver::new_global(),
            sources: Vec::new(),
        };
        intp.define_std();
        intp
    }

    pub fn env(&self) -> &Rc<Env> {
        &self.env
    }
}

impl Interpreter {
    fn parse_and_resolve(&mut self, name: &str, text: &str) -> Result<Vec<Stmt>> {
        let result: std::result::Result<Vec<_>, _> =
            parser::parse_with_id(text, self.sources.len());
        let mut stmts = result?;

        self.sources.push(Source {
            name: name.to_owned(),
            text: text.to_owned(),
        });
        self.resolver.res_stmts(&mut stmts)?;
        Ok(stmts)
    }

    pub fn eval_with_name(&mut self, name: &str, text: &str) -> Result<Value> {
        let stmts = self.parse_and_resolve(name, &text)?;
        self.env.eval_stmts(&stmts)
    }

    pub fn eval(&mut self, text: &str) -> Result<Value> {
        let stmts = self.parse_and_resolve("<unknown>", &text)?;
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

impl Interpreter {
    pub fn fmt_error(&self, error: Error) -> String {
        let source = self.sources.get(error.span.unwrap().source_id).unwrap();
        error.pretty_fmt(source)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let mut intp = Interpreter::new();
        let _r = intp.eval("var a = 1 + 2 * 3;").unwrap();
        assert_eq!(intp.env.get(&"a".into()).unwrap(), Value::Int(7));
    }
}
