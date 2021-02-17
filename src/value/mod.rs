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
    Func(Ba<Rc<Func>>),
    Class(Ba<Rc<RefCell<Class>>>),
    Object(Rc<RefCell<Object>>),
    Nil(Option<String>),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "Int({:?})", v),
            Value::Float(v) => write!(f, "Float({:?})", v),
            Value::Bool(v) => write!(f, "Bool({:?})", v),
            Value::String(v) => write!(f, "String({:?})", v),
            Value::Func(ByAddress(v)) => write!(f, "Func({:?})", v),
            Value::Class(ByAddress(v)) => write!(f, "Class({:?})", v.borrow()),
            Value::Object(v) => write!(f, "Object({:?})", v.borrow()),
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
            Value::Func(ByAddress(v)) => write!(f, "{}", v),
            Value::Class(ByAddress(v)) => write!(f, "{}", v.borrow()),
            Value::Object(v) => write!(f, "{}", v.borrow()),
            Value::Nil(None) => write!(f, "nil"),
            Value::Nil(Some(cause)) => write!(f, "nil with cause `{}`", cause),
        }
    }
}
