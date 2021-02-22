use super::{Class, Func, Value};
use crate::{ast::CanHoldNil, err, ErrType, Result};
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};
use uuid::Uuid;

#[derive(Clone)]
struct Object {
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

#[derive(Clone)]
pub struct WrObject(Rc<RefCell<Object>>);

impl std::fmt::Debug for WrObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0.borrow(), f)
    }
}

impl Display for WrObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0.borrow(), f)
    }
}

impl WrObject {
    pub fn new(class: Rc<RefCell<Class>>, fields: HashMap<String, Value>) -> Self {
        let object = Object {
            class,
            fields,
            uuid: Uuid::new_v4(),
        };

        Self(Rc::new(RefCell::new(object)))
    }

    pub fn new_copy(src: &Self) -> Self {
        let mut object = src.0.borrow().clone();
        for (_, v) in object.fields.iter_mut() {
            *v = super::copy(v.clone());
        }

        Self(Rc::new(RefCell::new(object)))
    }

    pub fn set_field(&mut self, k: &str, v: Value) -> Result<()> {
        if matches!(v, Value::Nil(..)) && !k.can_hold_nil() {
            return Err(err!(ErrType::CannotHaveValue(k.to_owned(), v)));
        }

        let mut object = self.0.borrow_mut();
        let entry = object.fields.get_mut(k);
        match entry {
            Some(field) => {
                *field = v;
                Ok(())
            }
            None => Err(err!(ErrType::NoFieldToSet(v, k.to_owned()))),
        }
    }

    pub fn get_field(&self, k: &str) -> Result<Option<Value>> {
        let object = self.0.borrow();

        let val = if let Some(field_val) = object.fields.get(k).cloned() {
            Some(field_val)
        } else if let Some(static_val) = object.class.borrow().statics.get(k).cloned() {
            if let Value::Func(func) = static_val {
                Some(Value::Func(Func::new_parital_apply(
                    func,
                    Value::Object(Self(Rc::clone(&self.0))),
                )?))
            } else {
                Some(static_val)
            }
        } else {
            None
        };

        Ok(val)
    }

    pub fn uuid(&self) -> Uuid {
        self.0.borrow().uuid
    }

    pub fn class_eq(&self, other: &Self) -> bool {
        self.0.borrow().class.eq(&other.0.borrow().class)
    }

    pub fn ref_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl PartialEq for WrObject {
    fn eq(&self, other: &Self) -> bool {
        let this = self.0.borrow();
        let that = other.0.borrow();

        if this.class != that.class {
            return false;
        }

        let equals = this.class.borrow().equals.clone();
        if let Some(eq_func) = equals {
            return eq_func(self, other);
        }

        this.fields == that.fields
    }
}

impl Drop for Object {
    fn drop(&mut self) {
        let finalize = self.class.borrow().finalize.clone();
        if let Some(_finalize_func) = finalize {
            // finalize_func(self);
            // FIXME: finalize
        };
    }
}
