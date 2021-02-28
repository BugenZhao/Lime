mod class;
mod func;
mod object;

use enum_as_inner::EnumAsInner;
use std::fmt::Display;

pub use class::*;
pub use func::*;
pub use object::*;

pub const N_MAX_ARGS: usize = 255;

#[derive(Clone, PartialEq, EnumAsInner)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Func(WrFunc),
    Class(WrClass),
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
            Value::Class(v) => write!(f, "Class({:?})", v),
            Value::Object(v) => write!(f, "Object({}, {})", v.class_name(), v.to_lime_string()),
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
            Value::Class(v) => write!(f, "{}", v),
            Value::Object(v) => write!(f, "{}", v.to_lime_string()),
            Value::Nil(None) => write!(f, "nil"),
            Value::Nil(Some(cause)) => write!(f, "nil with cause `{}`", cause),
        }
    }
}

impl Value {
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            Value::Int(..)
                | Value::Float(..)
                | Value::Bool(..)
                | Value::String(..)
                | Value::Nil(..)
        )
    }

    pub fn class_name(&self) -> String {
        match self {
            Value::Int(..) => "Int".to_owned(),
            Value::Float(..) => "Float".to_owned(),
            Value::Bool(..) => "Bool".to_owned(),
            Value::String(..) => "String".to_owned(),
            Value::Func(..) => "".to_owned(), // FIXME: change this
            Value::Class(class) => class.name(),
            Value::Object(object) => object.class_name(),
            Value::Nil(..) => "Nil".to_owned(),
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
        Value::Nil(_) => v,
        Value::Func(_) => v,
        Value::Class(_) => v,
    }
}
