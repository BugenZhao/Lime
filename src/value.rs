use std::{fmt::Display, ops::RangeInclusive, rc::Rc, sync::Arc};

use crate::{
    env::Env,
    parser::{Ident, Stmt},
    Error, Result,
};

pub const N_MAX_ARGS: usize = 255;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Func(Func),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Func(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Clone)]
pub struct RustFn(pub Arc<dyn Fn(Vec<Value>) -> Result<Value>>);

impl std::fmt::Debug for RustFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Pointer::fmt(&self.0, f)
    }
}

impl PartialEq for RustFn {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
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

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.tp == other.tp && self.arity == other.arity && Rc::ptr_eq(&self.env, &other.env)
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name.as_deref().unwrap_or("");
        match &self.tp {
            FuncType::BuiltIn(_) => write!(f, "<built-in> {}|{:?}|", name, self.arity),
            FuncType::Composed(..) => write!(f, "<composed> {}|{:?}|", name, self.arity),
            FuncType::Lime(params, _) => write!(
                f,
                "{}|{}|",
                name,
                params
                    .iter()
                    .map(|i| i.0.to_owned())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl std::fmt::Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name.as_deref().unwrap_or("");
        match &self.tp {
            FuncType::BuiltIn(rf) => write!(f, "<built-in({:?})> {}|{:?}|", rf, name, self.arity),
            _ => std::fmt::Display::fmt(self, f),
        }
    }
}

impl Func {
    pub fn with_name(self, name: String) -> Self {
        if self.name.is_some() {
            self
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
