#![macro_use]

use crate::{
    env::Env,
    parser::{Ident, Stmt},
    Error, Result,
};
use by_address::ByAddress;
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, fmt::Display, ops::RangeInclusive, rc::Rc};

pub const N_MAX_ARGS: usize = 255;

#[macro_export]
macro_rules! ba_rc {
    ($w:expr) => {
        by_address::ByAddress(std::rc::Rc::new($w))
    };
}

#[macro_export]
macro_rules! rc_refcell {
    ($w:expr) => {
        std::rc::Rc::new(std::cell::RefCell::new($w))
    };
}

pub type Ba<T> = ByAddress<T>;

#[derive(Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Func(Ba<Rc<Func>>),
    Class(Ba<Rc<Class>>),
    Object(Rc<RefCell<Object>>),
    Nil,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "Int({:?})", v),
            Value::Float(v) => write!(f, "Float({:?})", v),
            Value::Bool(v) => write!(f, "Bool({:?})", v),
            Value::String(v) => write!(f, "String({:?})", v),
            Value::Func(ByAddress(v)) => write!(f, "Func({:?})", v),
            Value::Class(ByAddress(v)) => write!(f, "Class({:?})", v),
            Value::Object(v) => write!(f, "Object({:?})", v.borrow()),
            Value::Nil => write!(f, "nil"),
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
            Value::Class(ByAddress(v)) => write!(f, "{}", v),
            Value::Object(v) => write!(f, "{}", v.borrow()),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Clone)]
pub struct RustFn(pub Rc<dyn Fn(Vec<Value>) -> Result<Value>>);

impl std::fmt::Debug for RustFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Pointer::fmt(&self.0, f)
    }
}

#[derive(Clone, Debug)]
pub enum FuncType {
    BuiltIn(RustFn),
    Composed(Box<Func>, Box<Func>),
    Lime(Vec<Ident>, Vec<Stmt>),
}

#[derive(Clone)]
pub struct Func {
    pub tp: FuncType,
    pub arity: RangeInclusive<usize>,
    pub env: Rc<Env>,
    pub name: Option<String>,
}

impl std::fmt::Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name.as_deref().unwrap_or("");
        match &self.tp {
            FuncType::BuiltIn(rf) => write!(f, "<built-in({:?})> {}|{:?}|", rf, name, self.arity),
            FuncType::Composed(..) => write!(f, "<composed> {}|{:?}|", name, self.arity),
            FuncType::Lime(params, _) => write!(
                f,
                "{}|{}|",
                name,
                params.iter().map(|i| i.0.to_owned()).join(", ")
            ),
        }
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl Func {
    pub fn with_name(self, name: String) -> Self {
        if self.name.is_some() {
            panic!("Func's name is Some(..). Check in advance.")
        } else {
            Self {
                name: Some(name),
                ..self
            }
        }
    }

    pub fn call(&self, args: Vec<Value>) -> Result<Value> {
        match &self.tp {
            FuncType::BuiltIn(f) => (f.0)(args),
            FuncType::Composed(f, g) => f.call(vec![g.call(args)?]),
            FuncType::Lime(params, body) => {
                let fn_env = Rc::new(Env::new(Rc::clone(&self.env)));
                for (param, arg) in params.clone().into_iter().zip(args) {
                    fn_env.decl(param, arg)?;
                }
                fn_env.eval_stmts(body)
            }
        }
    }

    pub fn compose(f: Self, g: Self) -> Result<Self> {
        let arity = g.arity.clone();
        let _ = f.check(1)?;
        Ok(Self {
            tp: FuncType::Composed(box f, box g),
            arity,
            env: Rc::new(Env::new_empty()),
            name: None,
        })
    }

    pub fn check(&self, supp: usize) -> Result<()> {
        if self.arity.contains(&supp) {
            Ok(())
        } else {
            Err(Error::WrongArguments {
                f: self.clone(),
                take: self.arity.clone(),
                supp,
            })
        }
    }
}

#[derive(PartialEq)]
pub struct Class {
    pub name: String,
    pub fields: Vec<String>,
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

#[derive(Clone, PartialEq)]
pub struct Object {
    pub class: Rc<Class>,
    pub fields: HashMap<String, Value>,
}

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{{{}}}",
            self.class.name,
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
