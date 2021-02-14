#![allow(clippy::unnecessary_wraps)]

use crate::{
    ba_rc,
    env::Env,
    lime_error,
    parser::{self, Ident},
    value::{FuncType, RustFn, N_MAX_ARGS},
    Func,
    LimeError::*,
    Result, Value,
};
use itertools::Itertools;
use std::{
    io::{stdout, Write},
    rc::Rc,
};

macro_rules! join {
    ($args:expr) => {
        $args.iter().map(|v| v.to_string()).join(" ")
    };
}

fn print(args: Vec<Value>) -> Result<Value> {
    print!("{}", join!(args));
    stdout().flush()?;
    Ok(Value::Nil)
}

fn println(args: Vec<Value>) -> Result<Value> {
    println!("{}", join!(args));
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
    Err(lime_error!(Panic(join!(args))))
}

fn copy(args: Vec<Value>) -> Result<Value> {
    let v = args.into_iter().next().unwrap();
    match v {
        Value::Int(_) => Ok(v),
        Value::Float(_) => Ok(v),
        Value::Bool(_) => Ok(v),
        Value::String(_) => Ok(v),
        Value::Object(rc_obj) => {
            let obj = (*rc_obj).clone();
            for (_, v) in obj.borrow_mut().fields.iter_mut() {
                *v = copy(vec![v.clone()])?;
            }
            Ok(Value::Object(Rc::new(obj)))
        }
        Value::Func(_) | Value::Class(_) | Value::Nil => {
            Err(lime_error!(Panic(format!("Cannot copy `{:?}`", v))))
        }
    }
}

fn define_builtin(env: &Rc<Env>) {
    macro_rules! def {
        ($func:expr, $name:expr, $arity:expr) => {
            env.decl(
                Ident($name.to_owned(), None),
                Value::Func(ba_rc!(Func {
                    tp: FuncType::BuiltIn(RustFn(Rc::new($func))),
                    arity: $arity,
                    env: Rc::clone(env),
                    name: Some($name.to_owned()),
                })),
            )
            .unwrap();
        };
    }

    def!(print, "print", 0..=N_MAX_ARGS);
    def!(println, "println", 0..=N_MAX_ARGS);
    def!(time, "time", 0..=0);
    def!(panic, "panic", 1..=N_MAX_ARGS);
    def!(copy, "copy", 1..=1);
    def!(
        |args| {
            print(args)?;
            let mut buf = String::new();
            std::io::stdin().read_line(&mut buf)?;
            buf.pop();
            Ok(Value::String(buf))
        },
        "readln",
        0..=N_MAX_ARGS
    );
    def!(
        |args| { Ok(Value::String(format!("{:?}", args[0]))) },
        "dbg",
        1..=1
    );
    def!(
        |_| { Ok(Value::String(format!("Lime {}", env!("CARGO_PKG_VERSION")))) },
        "version",
        0..=0
    );
    {
        use std::sync::atomic::{AtomicI64, Ordering};
        let v = AtomicI64::new(1);
        def!(
            move |_| { Ok(Value::Int(v.fetch_add(1, Ordering::SeqCst))) },
            "count",
            0..=0
        );
    }
}

fn define_prelude(env: &Rc<Env>) {
    const PRELUDE_LM: &str = include_str!("prelude.lm");

    let stmts = parser::parse_and_resolve(PRELUDE_LM).unwrap();
    env.eval_stmts(&stmts).unwrap();
}

pub fn define_std(env: &Rc<Env>) {
    define_builtin(env);
    define_prelude(env);
}
