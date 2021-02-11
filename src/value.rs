use std::{fmt::Display, rc::Rc, sync::Arc};

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
pub struct RustFn(pub Arc<dyn Fn(Vec<Value>) -> Value>);

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
    BuiltIn(RustFn, String),
    Composed(Box<Func>, Box<Func>),
    Lime(Vec<Ident>, Vec<Stmt>),
}

#[derive(Clone)]
pub struct Func {
    pub tp: FuncType,
    pub arity: (usize, usize),
    pub env: Rc<Env>,
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.tp == other.tp && self.arity == other.arity && Rc::ptr_eq(&self.env, &other.env)
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.tp {
            FuncType::BuiltIn(_, name) => write!(f, "built-in func `{}`|{:?}|", name, self.arity),
            FuncType::Composed(..) => write!(f, "composed func |{:?}|", self.arity),
            FuncType::Lime(params, _) => write!(
                f,
                "func |{}|",
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
        std::fmt::Display::fmt(self, f)
    }
}

impl Func {
    #[inline]
    pub fn call(&self, args: Vec<Value>) -> Result<Value> {
        match &self.tp {
            FuncType::BuiltIn(f, _) => Ok((f.0)(args)),
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
        let arity = g.arity;
        let _ = f.check(1)?;
        Ok(Self {
            tp: FuncType::Composed(box f, box g),
            arity,
            env: Rc::new(Env::new_empty()),
        })
    }

    pub fn check(&self, supp: usize) -> Result<()> {
        if supp >= self.arity.0 && supp <= self.arity.1 {
            Ok(())
        } else {
            Err(Error::WrongArguments {
                f: self.clone(),
                take: self.arity,
                supp,
            })
        }
    }
}
