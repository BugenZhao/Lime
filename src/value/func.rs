use super::Value;
use crate::{
    ast::{Ident, Stmt},
    env::Env,
    err, ErrType, Result,
};
use itertools::Itertools;
use std::{fmt::Display, ops::RangeInclusive, rc::Rc};

type RustFn = Rc<dyn Fn(Vec<Value>) -> Result<Value>>;

#[derive(Clone)]
pub enum FuncType {
    BuiltIn(RustFn),
    Composed(Box<Func>, Box<Func>),
    PartialApplied(Box<Func>, Value),
    Lime(Vec<Ident>, Vec<Stmt>),
}

#[derive(Clone)]
struct RealFunc {
    tp: FuncType,
    arity: RangeInclusive<usize>,
    env: Rc<Env>,
    name: Option<String>,
}

impl std::fmt::Debug for RealFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name.as_deref().unwrap_or("");
        match &self.tp {
            FuncType::BuiltIn(rf) => write!(f, "<built-in({:p})> {}|{:?}|", rf, name, self.arity),
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

impl Display for RealFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

#[derive(Clone)]
pub struct Func(Rc<RealFunc>);

impl std::fmt::Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl Func {
    pub fn name(&self) -> &Option<String> {
        &self.0.name
    }

    pub fn try_with_name(self, name: String) -> Self {
        if self.0.name.is_none() {
            let mut new = self.0.as_ref().clone();
            new.name = Some(name);
            Self(Rc::new(new))
        } else {
            self
        }
    }

    pub fn call(&self, mut args: Vec<Value>) -> Result<Value> {
        match &self.0.tp {
            FuncType::BuiltIn(f) => f(args),
            FuncType::Composed(f, g) => f.call(vec![g.call(args)?]),
            FuncType::PartialApplied(f, arg) => {
                let mut real_args = vec![arg.clone()];
                real_args.append(&mut args);
                f.call(real_args)
            }
            FuncType::Lime(params, body) => {
                let fn_env = Rc::new(Env::new(Rc::clone(&self.0.env)));
                for (param, arg) in params.clone().into_iter().zip(args) {
                    fn_env.decl(param, arg)?;
                }
                fn_env.eval_stmts(body)
            }
        }
    }

    pub fn new_builtin(
        name: Option<String>,
        rf: RustFn,
        arity: RangeInclusive<usize>,
        env: Rc<Env>,
    ) -> Self {
        let func = RealFunc {
            tp: FuncType::BuiltIn(rf),
            arity,
            env,
            name,
        };

        Self(Rc::new(func))
    }

    pub fn new_lime(
        params: Vec<Ident>,
        body: Vec<Stmt>,
        arity: RangeInclusive<usize>,
        env: Rc<Env>,
    ) -> Self {
        let func = RealFunc {
            tp: FuncType::Lime(params, body),
            arity,
            env,
            name: None,
        };

        Self(Rc::new(func))
    }

    pub fn new_compose(f: Self, g: Self) -> Result<Self> {
        let arity = g.0.arity.clone();
        let _ = f.check(1)?;
        let func = RealFunc {
            tp: FuncType::Composed(box f, box g),
            arity,
            env: Rc::new(Env::new_empty()),
            name: None,
        };

        Ok(Self(Rc::new(func)))
    }

    pub fn new_parital_apply(f: Self, arg: Value) -> Result<Self> {
        let min = *f.0.arity.start();
        let max = *f.0.arity.end();

        if max >= 1 {
            let env = f.0.env.clone();
            let name = f.0.name.clone();

            let func = RealFunc {
                tp: FuncType::PartialApplied(box f, arg),
                arity: min.saturating_sub(1)..=max.saturating_sub(1),
                env,
                name,
            };
            Ok(Self(Rc::new(func)))
        } else {
            Err(err!(ErrType::CannotPartialApply(f)))
        }
    }

    pub fn check(&self, supp: usize) -> Result<()> {
        if self.0.arity.contains(&supp) {
            Ok(())
        } else {
            Err(err!(ErrType::WrongArguments {
                f: self.clone(),
                take: self.0.arity.clone(),
                supp,
            }))
        }
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
