use super::{Value, WrClass, WrFunc};
use crate::{ast::CanHoldNil, err, ErrType, Result};
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};
use uuid::Uuid;

#[derive(Clone)]
pub struct Object {
    class: WrClass,
    fields: HashMap<String, Value>,
    uuid: Uuid,
}

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{{{}}}",
            self.class.name(),
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
    pub fn new(class: WrClass, fields: HashMap<String, Value>) -> Self {
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
        } else if let Some(static_val) = object.class.get_static(k) {
            if let Value::Func(func) = static_val {
                Some(Value::Func(WrFunc::new_parital_apply(
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
        if !self.class_eq(other) {
            false
        } else if let Some(Value::Func(eq_func)) = self.0.borrow().class.equals_fn() {
            match eq_func
                .call(vec![
                    Value::Object(self.clone()),
                    Value::Object(other.clone()),
                ])
                .unwrap()
            {
                Value::Bool(v) => Ok(v),
                v => Err(err!(ErrType::TypeError("Bool".to_owned(), v))),
            }
            .unwrap() // TODO: try not panicking
        } else {
            self.0.borrow().fields == other.0.borrow().fields
        }
    }
}

impl Drop for WrObject {
    fn drop(&mut self) {
        if Rc::strong_count(&self.0) == 1 {
            let finalize = self.0.borrow().class.finalize_fn();
            if let Some(Value::Func(finalize_func)) = finalize {
                finalize_func
                    .call(vec![Value::Object(self.clone())])
                    .unwrap();
            };
        }
    }
}
