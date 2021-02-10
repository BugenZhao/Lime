use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    env::Env,
    parser::{Func, Ident},
    Value,
};

fn print_rs(args: Vec<Value>) -> Value {
    println!("{}", args[0]);
    Value::Nil
}

fn time(_: Vec<Value>) -> Value {
    Value::Float(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )
}

pub fn define_std(env: &mut Env) {
    env.decl(
        Ident("print_rs".to_owned()),
        Value::Func(Func::RustFn(print_rs), 1),
    )
    .unwrap();

    env.decl(Ident("time".to_owned()), Value::Func(Func::RustFn(time), 0))
        .unwrap();
}
