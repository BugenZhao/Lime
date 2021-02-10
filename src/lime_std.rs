use std::{
    sync::Arc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{env::Env, parser::Ident, value::RustFn, Func, Value};

macro_rules! built_in_fn {
    ($func:expr, $name:expr, $arity:expr) => {
        Value::Func(
            Func::BuiltIn(RustFn(Arc::new($func)), $name.to_owned()),
            $arity,
        )
    };
}

fn print(args: Vec<Value>) -> Value {
    println!("{}", args[0]);
    Value::Nil
}

fn time(_args: Vec<Value>) -> Value {
    Value::Float(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )
}

fn count_gen() -> Value {
    static mut V: i64 = 0;
    built_in_fn!(
        |_| unsafe {
            V += 1;
            Value::Int(V)
        },
        "count",
        0
    )
}

pub fn define_std(env: &mut Env) {
    env.decl(Ident("print".to_owned()), built_in_fn!(print, "print", 1))
        .unwrap();

    env.decl(Ident("time".to_owned()), built_in_fn!(time, "time", 0))
        .unwrap();

    env.decl(Ident("count".to_owned()), count_gen()).unwrap();
}
