use super::Value;
use crate::{
    ast::{IdentExt, Expr, Ident},
    err, ErrType, Result,
};
use itertools::Itertools;
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap, HashSet},
    fmt::Display,
    rc::Rc,
};

#[derive(PartialEq)]
struct Class {
    name: String,
    fields: Vec<String>,
    statics: HashMap<String, Value>,
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

#[derive(Clone)]
pub struct WrClass(Rc<RefCell<Class>>);

impl std::fmt::Debug for WrClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0.borrow(), f)
    }
}

impl Display for WrClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0.borrow(), f)
    }
}

impl WrClass {
    pub fn new(name: String, fields: Vec<String>) -> Self {
        let class = Class {
            name,
            fields,
            statics: HashMap::new(),
        };

        Self(Rc::new(RefCell::new(class)))
    }

    pub fn decl_static(&self, k: String, mut v: Value) -> Result<()> {
        let mut class = self.0.borrow_mut();
        let name = class.name.clone(); // TODO: avoid cloning

        match class.statics.entry(k.clone()) {
            Entry::Occupied(_) => Err(err!(ErrType::DefinedMutlipleTimes(k))),
            Entry::Vacant(e) => {
                if let Value::Func(func) = v {
                    v = Value::Func(func.try_with_name(format!("{}.{}", name, k)))
                }
                if matches!(v, Value::Nil(..)) && !k.can_hold_nil() {
                    return Err(err!(ErrType::CannotHaveValue(k, v)));
                }
                e.insert(v);
                Ok(())
            }
        }
    }

    pub fn set_static(&self, k: &str, v: Value) -> Result<()> {
        if matches!(v, Value::Nil(..)) && !k.can_hold_nil() {
            return Err(err!(ErrType::CannotHaveValue(k.to_owned(), v)));
        }

        let mut class = self.0.borrow_mut();
        let entry = class.statics.get_mut(k);
        match entry {
            Some(field) => {
                *field = v;
                Ok(())
            }
            None => Err(err!(ErrType::NoFieldToSet(v, k.to_owned()))),
        }
    }

    pub fn get_static(&self, k: &str) -> Option<Value> {
        self.0.borrow().statics.get(k).cloned()
    }

    pub fn check_kvs(&self, kvs: &[(Ident, Expr)]) -> bool {
        let keys_set = kvs
            .iter()
            .map(|(k, _)| &k.0)
            .cloned()
            .collect::<HashSet<_>>();
        let expected_keys_set = self
            .0
            .borrow()
            .fields
            .iter()
            .cloned()
            .collect::<HashSet<_>>();

        keys_set == expected_keys_set
    }

    pub fn name(&self) -> String {
        self.0.borrow().name.clone()
    }

    pub fn equals_fn(&self) -> Option<Value> {
        self.get_static("equals")
    }

    pub fn finalize_fn(&self) -> Option<Value> {
        self.get_static("finalize")
    }

    pub fn to_string_fn(&self) -> Option<Value> {
        self.get_static("to_string")
    }
}

impl PartialEq for WrClass {
    // by address
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
