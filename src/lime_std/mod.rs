use std::rc::Rc;
use std::sync::Arc;

use crate::{
    env::Env,
    parser::{self, Ident},
    value::{FuncType, RustFn, N_MAX_ARGS},
    Error, Func,
    LimeError::*,
    Result, Value,
};

fn print(args: Vec<Value>) -> Result<Value> {
    println!(
        "{}",
        args.iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    );
    Ok(Value::Nil)
}

fn time(_args: Vec<Value>) -> Result<Value> {
    use std::time::{SystemTime, UNIX_EPOCH};

    Ok(Value::Float(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    ))
}

fn panic(args: Vec<Value>) -> Result<Value> {
    Err(Error::Lime(Panic(
        args.iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(" "),
    )))
}

fn define_builtin(env: &Rc<Env>) {
    macro_rules! built_in_fn {
        ($func:expr, $name:expr, $arity:expr) => {
            Value::Func(Func {
                tp: FuncType::BuiltIn(RustFn(Arc::new($func)), $name.to_owned()),
                arity: $arity,
                env: Rc::clone(env),
            })
        };
    }

    {
        env.decl(
            Ident("print".to_owned()),
            built_in_fn!(print, "print", (0, N_MAX_ARGS)),
        )
        .unwrap();
    }
    {
        env.decl(Ident("time".to_owned()), built_in_fn!(time, "time", (0, 0)))
            .unwrap();
    }
    {
        use std::sync::atomic::{AtomicI64, Ordering};
        let v = AtomicI64::new(1);
        env.decl(
            Ident("count".to_owned()),
            built_in_fn!(
                move |_| { Ok(Value::Int(v.fetch_add(1, Ordering::SeqCst))) },
                "count",
                (0, 0)
            ),
        )
        .unwrap();
    }
    {
        env.decl(
            Ident("panic".to_owned()),
            built_in_fn!(panic, "panic", (1, 255)),
        )
        .unwrap();
    }
}

pub fn define_prelude(env: &Rc<Env>) {
    const PRELUDE_LM: &str = include_str!("prelude.lm");

    let stmts = parser::parse(PRELUDE_LM).unwrap();
    env.eval_stmts(&stmts).unwrap();
}

pub fn define_std(env: &Rc<Env>) {
    define_builtin(env);
    define_prelude(env);
}
