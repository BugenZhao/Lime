use std::rc::Rc;
use std::sync::Arc;

use crate::{
    env::Env, parser::Ident, value::FuncType, value::RustFn, value::N_MAX_ARGS, Func, Value,
};

fn print(args: Vec<Value>) -> Value {
    println!(
        "{}",
        args.iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    );
    Value::Nil
}

fn time(_args: Vec<Value>) -> Value {
    use std::time::{SystemTime, UNIX_EPOCH};

    Value::Float(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )
}

pub fn define_std(env: &Rc<Env>) {
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
                move |_| { Value::Int(v.fetch_add(1, Ordering::SeqCst)) },
                "count",
                (0, 0)
            ),
        )
        .unwrap();
    }
}
