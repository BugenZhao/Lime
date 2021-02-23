#![allow(clippy::unnecessary_wraps)]

mod primitive;
mod vec;

use crate::{
    ast::Ident,
    env::Env,
    err, parse_and_resolve,
    value::{self, WrFunc, N_MAX_ARGS},
    ErrType, Result, Value,
};
use itertools::Itertools;
use std::{
    io::{stdout, Write},
    rc::Rc,
};
use vec::define_std_class;

use self::primitive::define_primitive;

macro_rules! join {
    ($args:expr) => {
        $args.iter().map(|v| v.to_string()).join(" ")
    };
}

fn print(args: Vec<Value>) -> Result<Value> {
    print!("{}", join!(args));
    stdout().flush()?;
    Ok(Value::Nil(None))
}

fn println(args: Vec<Value>) -> Result<Value> {
    println!("{}", join!(args));
    Ok(Value::Nil(None))
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
    Err(err!(ErrType::LimePanic(join!(args))))
}

fn copy(args: Vec<Value>) -> Result<Value> {
    let v = args.into_iter().next().unwrap();
    Ok(value::copy(v))
}

fn __expect(mut args: Vec<Value>) -> Result<Value> {
    let is_nil = {
        let (v, args) = args.split_first_mut().unwrap();
        if let Value::Nil(cause) = v {
            if !args.is_empty() {
                *cause = Some(join!(args))
            }
            true
        } else {
            false
        }
    };
    let v = args.into_iter().next().unwrap();
    if is_nil {
        Err(err!(ErrType::Expect(v)))
    } else {
        Ok(v)
    }
}

fn __is_some(args: Vec<Value>) -> Result<Value> {
    let v = args.into_iter().next().unwrap();
    Ok(Value::Bool(!matches!(v, Value::Nil(..))))
}

fn __is_nil(args: Vec<Value>) -> Result<Value> {
    let v = args.into_iter().next().unwrap();
    Ok(Value::Bool(matches!(v, Value::Nil(..))))
}

fn __cause(args: Vec<Value>) -> Result<Value> {
    let v = args.into_iter().next().unwrap();
    if let Value::Nil(cause) = v {
        Ok(Value::String(cause.unwrap_or_else(|| "".to_owned())))
    } else {
        Err(err!(ErrType::LimePanic(format!(
            "`{:?}` does not have cause since it is not `nil`",
            v
        ))))
    }
}

fn __to_string(args: Vec<Value>) -> Result<Value> {
    Ok(Value::String(format!("{}", args[0])))
}

fn define_builtin(env: &Rc<Env>) {
    macro_rules! def {
        ($func:expr, $name:expr, $arity:expr) => {
            env.decl(
                Ident($name.to_owned(), None),
                Value::Func(WrFunc::new_builtin(
                    Some($name.to_owned()),
                    Rc::new($func),
                    $arity,
                    Rc::clone(env),
                )),
            )
            .unwrap();
        };
    }

    def!(print, "print", 0..=N_MAX_ARGS);
    def!(println, "println", 0..=N_MAX_ARGS);
    def!(time, "time", 0..=0);
    def!(panic, "panic", 1..=N_MAX_ARGS);
    def!(copy, "copy", 1..=1);
    def!(__expect, "__expect", 1..=N_MAX_ARGS);
    def!(__is_some, "__is_some", 1..=1);
    def!(__is_nil, "__is_nil", 1..=1);
    def!(__cause, "__cause", 1..=1);
    def!(__to_string, "__to_string", 1..=1);

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

    let stmts = parse_and_resolve(PRELUDE_LM).unwrap();
    env.eval_stmts(&stmts).unwrap();
}

pub fn define_std(env: &Rc<Env>) {
    define_primitive(env);
    define_builtin(env);
    define_std_class(env);
    define_prelude(env);
}
