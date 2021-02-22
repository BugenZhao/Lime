mod class;
mod func;
mod object;
mod utils;

use by_address::ByAddress;
use std::{cell::RefCell, fmt::Display, rc::Rc};

pub use class::*;
pub use func::*;
pub use object::*;

pub const N_MAX_ARGS: usize = 255;

pub type Ba<T> = ByAddress<T>;

#[derive(Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Func(Func),
    Class(Ba<Rc<RefCell<Class>>>),
    Object(WrObject),
    Nil(Option<String>),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "Int({:?})", v),
            Value::Float(v) => write!(f, "Float({:?})", v),
            Value::Bool(v) => write!(f, "Bool({:?})", v),
            Value::String(v) => write!(f, "String({:?})", v),
            Value::Func(v) => write!(f, "Func({:?})", v),
            Value::Class(ByAddress(v)) => write!(f, "Class({:?})", v.borrow()),
            Value::Object(v) => write!(f, "Object({:?})", v),
            Value::Nil(c) => write!(f, "Nil({:?})", c),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Func(v) => write!(f, "{}", v),
            Value::Class(ByAddress(v)) => write!(f, "{}", v.borrow()),
            Value::Object(v) => write!(f, "{}", v),
            Value::Nil(None) => write!(f, "nil"),
            Value::Nil(Some(cause)) => write!(f, "nil with cause `{}`", cause),
        }
    }
}

pub fn copy(v: Value) -> Value {
    match v {
        Value::Int(_) => v,
        Value::Float(_) => v,
        Value::Bool(_) => v,
        Value::String(_) => v,
        Value::Object(obj) => Value::Object(WrObject::new_copy(&obj)),
        Value::Nil(_) => v, // FIXME: check this
        Value::Func(_) | Value::Class(_) => v,
    }
}
