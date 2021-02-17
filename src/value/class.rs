use super::Value;
use crate::{
    ba_rc, err,
    parser::{Expr, Ident},
    ErrType, Result,
};
use itertools::Itertools;
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    fmt::Display,
};

#[derive(PartialEq)]
pub struct Class {
    pub name: String,
    pub fields: Vec<String>,
    pub statics: HashMap<String, Value>,
}

impl std::fmt::Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{{{}}}", self.name, self.fields.iter().join(", "))
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl Class {
    pub fn decl_static(&mut self, k: String, mut v: Value) -> Result<()> {
        match self.statics.entry(k.clone()) {
            Entry::Occupied(_) => Err(err!(ErrType::DefinedMutlipleTimes(k))),
            Entry::Vacant(e) => {
                if let Value::Func(func) = &v {
                    v = Value::Func(ba_rc!(func
                        .as_ref()
                        .clone()
                        .with_name(format!("{}.{}", self.name, k))))
                }
                if matches!(v, Value::Nil(..)) && !k.ends_with('?') {
                    return Err(err!(ErrType::CannotHaveValue(k, v)));
                }
                e.insert(v);
                Ok(())
            }
        }
    }

    pub fn set_static(&mut self, k: &str, v: Value) -> Result<()> {
        if matches!(v, Value::Nil(..)) && !k.ends_with('?') {
            return Err(err!(ErrType::CannotHaveValue(k.to_owned(), v)));
        }

        let entry = self.statics.get_mut(k);
        match entry {
            Some(field) => {
                *field = v;
                Ok(())
            }
            None => Err(err!(ErrType::NoFieldToSet(v, k.to_owned()))),
        }
    }

    pub fn get_static(&self, k: &str) -> Option<Value> {
        self.statics.get(k).cloned()
    }

    pub fn check_kvs(&self, kvs: &[(Ident, Expr)]) -> bool {
        let keys_set = kvs
            .iter()
            .map(|(k, _)| &k.0)
            .cloned()
            .collect::<HashSet<_>>();
        let expected_keys_set = self.fields.iter().cloned().collect::<HashSet<_>>();

        keys_set == expected_keys_set
    }
}