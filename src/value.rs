use std::{fmt::Display, sync::Arc};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Func(Func, usize),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Func(v, _) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Clone)]
pub struct RustFn(pub Arc<dyn Fn(Vec<Value>) -> Value>);

impl std::fmt::Debug for RustFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Pointer::fmt(&self, f)
    }
}

impl PartialEq for RustFn {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Func {
    BuiltIn(RustFn, String),
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Func::BuiltIn(_, name) => write!(f, "<built-in func `{}`>", name),
        }
    }
}
