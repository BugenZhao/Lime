use super::Value;
use crate::{
    env::Env,
    err,
    ast::{Ident, Stmt},
    ErrType, Result,
};
use itertools::Itertools;
use std::{fmt::Display, ops::RangeInclusive, rc::Rc};

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
    PartialApplied(Box<Func>, Value),
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
            FuncType::PartialApplied(..) => {
                write!(f, "<partial-applied> {}|{:?}|", name, self.arity)
            }
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

    pub fn call(&self, mut args: Vec<Value>) -> Result<Value> {
        match &self.tp {
            FuncType::BuiltIn(f) => (f.0)(args),
            FuncType::Composed(f, g) => f.call(vec![g.call(args)?]),
            FuncType::PartialApplied(f, arg) => {
                let mut real_args = vec![arg.clone()];
                real_args.append(&mut args);
                f.call(real_args)
            }
            FuncType::Lime(params, body) => {
                let fn_env = Rc::new(Env::new(Rc::clone(&self.env)));
                for (param, arg) in params.clone().into_iter().zip(args) {
                    fn_env.decl(param, arg)?;
                }
                fn_env.eval_stmts(body)
            }
        }
    }

    pub fn new_compose(f: Self, g: Self) -> Result<Self> {
        let arity = g.arity.clone();
        let _ = f.check(1)?;
        Ok(Self {
            tp: FuncType::Composed(box f, box g),
            arity,
            env: Rc::new(Env::new_empty()),
            name: None,
        })
    }

    pub fn new_parital_apply(f: Self, arg: Value) -> Result<Self> {
        let min = f.arity.start();
        let max = f.arity.end();

        if *max >= 1 {
            Ok(Self {
                tp: FuncType::PartialApplied(box f.clone(), arg),
                arity: min.saturating_sub(1)..=max.saturating_sub(1),
                env: f.env,
                name: f.name,
            })
        } else {
            Err(err!(ErrType::CannotPartialApply(f)))
        }
    }

    pub fn check(&self, supp: usize) -> Result<()> {
        if self.arity.contains(&supp) {
            Ok(())
        } else {
            Err(err!(ErrType::WrongArguments {
                f: self.clone(),
                take: self.arity.clone(),
                supp,
            }))
        }
    }
}
