use std::collections::HashMap;

use crate::{
    error::{Error, Result},
    parser::{Ident, Value},
};

pub struct Env {
    vars: HashMap<Ident, Value>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    pub fn get(&self, ident: &Ident) -> Option<&Value> {
        self.vars.get(ident)
    }

    pub fn decl(&mut self, ident: Ident, val: Value) {
        self.vars.insert(ident, val);
    }

    pub fn assign(&mut self, ident: &Ident, val: Value) -> Result<()> {
        if let Some(v) = self.vars.get_mut(ident) {
            *v = val.clone();
            Ok(())
        } else {
            Err(Error::CannotFindValue(ident.0.to_owned()))
        }
    }
}
