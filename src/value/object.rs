use super::{Class, Func, Value};
use crate::{ast::CanHoldNil, ba_rc, err, ErrType, Result};
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};
use uuid::Uuid;

#[derive(Clone)]
pub struct Object {
    pub class: Rc<RefCell<Class>>,
    pub fields: HashMap<String, Value>,
    pub uuid: Uuid,
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

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        if self.class != other.class {
            return false;
        }

        let equals = self.class.borrow().equals.clone();
        if let Some(eq_func) = equals {
            return eq_func(self, other);
        }

        self.fields == other.fields
    }
}

impl Object {
    pub fn new(class: Rc<RefCell<Class>>, fields: HashMap<String, Value>) -> Self {
        Self {
            class,
            fields,
            uuid: Uuid::new_v4(),
        }
    }

    pub fn set_field(&mut self, k: &str, v: Value) -> Result<()> {
        if matches!(v, Value::Nil(..)) && !k.can_hold_nil() {
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

impl Drop for Object {
    fn drop(&mut self) {
        let finalize = self.class.borrow().finalize.clone();
        if let Some(finalize_func) = finalize {
            finalize_func(self);
        };
    }
}
