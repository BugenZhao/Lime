use super::{Class, Func, Value};
use crate::{ba_rc, err, ErrType, Result};
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

#[derive(Clone, PartialEq)]
pub struct Object {
    pub class: Rc<RefCell<Class>>,
    pub fields: HashMap<String, Value>,
}

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{{{}}}",
            self.class.borrow().name,
            self.fields
                .iter()
                .sorted_by_key(|p| p.0)
                .map(|(k, v)| format!("{}: {}", k, v))
                .join(", ")
        )
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl Object {
    pub fn set_field(&mut self, k: &str, v: Value) -> Result<()> {
        if matches!(v, Value::Nil(..)) && !k.ends_with('?') {
            return Err(err!(ErrType::CannotHaveValue(k.to_owned(), v)));
        }

        let entry = self.fields.get_mut(k);
        match entry {
            Some(field) => {
                *field = v;
                Ok(())
            }
            None => Err(err!(ErrType::NoFieldToSet(v, k.to_owned()))),
        }
    }

    pub fn get_field(&self, rc_refcell_self: Rc<RefCell<Self>>, k: &str) -> Result<Option<Value>> {
        let val = if let Some(field_val) = self.fields.get(k).cloned() {
            Some(field_val)
        } else if let Some(static_val) = self.class.borrow().statics.get(k).cloned() {
            if let Value::Func(func) = static_val {
                Some(Value::Func(ba_rc!(Func::new_parital_apply(
                    func.as_ref().clone(),
                    Value::Object(rc_refcell_self)
                )?)))
            } else {
                Some(static_val)
            }
        } else {
            None
        };

        Ok(val)
    }
}
