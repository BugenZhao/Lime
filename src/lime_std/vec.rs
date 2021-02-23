use crate::{
    env::Env,
    err,
    value::{WrClass, WrFunc},
    ErrType, Result, Value,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub fn define_std_class(env: &Rc<Env>) {
    env.decl("Vec".into(), build_vec_class(env)).unwrap();
}

pub fn build_vec_class(env: &Rc<Env>) -> Value {
    macro_rules! assoc_func {
        ($func:expr, $arity:expr) => {
            Value::Func(WrFunc::new_builtin(
                None,
                Rc::new($func),
                $arity,
                Rc::clone(env),
            ))
        };
    }

    macro_rules! obj {
        ($args:expr) => {
            if let Value::Object(obj) = $args.get(0).unwrap() {
                obj
            } else {
                unreachable!()
            }
        };
        ($args:expr, $n:expr) => {
            if let Value::Object(obj) = $args.get($n).unwrap() {
                obj
            } else {
                unreachable!()
            }
        };
    }

    type VecPool = HashMap<usize, Vec<Value>>;
    let pool = Rc::new(RefCell::new(VecPool::new()));

    let vec_class = WrClass::new("Vec".to_owned(), Vec::new());

    {
        let pool = Rc::clone(&pool);
        let get = move |args: Vec<Value>| -> Result<Value> {
            let key = obj!(args).ptr();

            let idx_v = args.get(1).unwrap();
            let idx = if let Value::Int(idx) = idx_v {
                *idx as usize
            } else {
                return Err(err!(ErrType::TypeError("Int".to_owned(), idx_v.clone())));
            };

            let mut pool = pool.borrow_mut();
            let entry = pool.entry(key).or_insert_with(Vec::new);

            let ret = if let Some(v) = entry.get(idx).cloned() {
                v
            } else {
                Value::Nil(Some("Index out of range".to_owned()))
            };

            Ok(ret)
        };
        vec_class
            .decl_static("get".to_owned(), assoc_func!(get, 2..=2))
            .unwrap();
    }

    {
        let pool = Rc::clone(&pool);
        let set = move |args: Vec<Value>| -> Result<Value> {
            let key = obj!(args).ptr();

            let idx_v = args.get(1).unwrap();
            let idx = if let Value::Int(idx) = idx_v {
                *idx as usize
            } else {
                return Err(err!(ErrType::TypeError("Int".to_owned(), idx_v.clone())));
            };

            let to_set = args.get(2).unwrap().clone();

            let mut pool = pool.borrow_mut();
            let entry = pool.entry(key).or_insert_with(Vec::new);

            if let Some(v) = entry.get_mut(idx as usize) {
                *v = to_set.clone();
                Ok(to_set)
            } else {
                Ok(Value::Nil(Some("index out of range".to_owned())))
            }
        };
        vec_class
            .decl_static("set".to_owned(), assoc_func!(set, 3..=3))
            .unwrap();
    }

    {
        let pool = Rc::clone(&pool);
        let len = move |args: Vec<Value>| -> Result<Value> {
            let key = obj!(args).ptr();

            let mut pool = pool.borrow_mut();
            let entry = pool.entry(key).or_insert_with(Vec::new);

            Ok(Value::Int(entry.len() as i64))
        };
        vec_class
            .decl_static("len".to_owned(), assoc_func!(len, 1..=1))
            .unwrap();
    }

    {
        let pool = Rc::clone(&pool);
        let push = move |args: Vec<Value>| -> Result<Value> {
            let key = obj!(args).ptr();

            let to_push = args.get(1).unwrap().clone();

            let mut pool = pool.borrow_mut();
            let entry = pool.entry(key).or_insert_with(Vec::new);

            entry.push(to_push.clone());

            Ok(to_push)
        };
        vec_class
            .decl_static("push".to_owned(), assoc_func!(push, 2..=2))
            .unwrap();
    }

    {
        let pool = Rc::clone(&pool);
        let pop = move |args: Vec<Value>| -> Result<Value> {
            let key = obj!(args).ptr();

            let mut pool = pool.borrow_mut();
            let entry = pool.entry(key).or_insert_with(Vec::new);

            Ok(entry
                .pop()
                .unwrap_or_else(|| Value::Nil(Some("vec is empty".to_owned()))))
        };
        vec_class
            .decl_static("pop".to_owned(), assoc_func!(pop, 1..=1))
            .unwrap();
    }

    {
        let pool = Rc::clone(&pool);
        let finalize = move |args: Vec<Value>| -> Result<Value> {
            let key = obj!(args).ptr();

            let removed = {
                let mut pool = pool.borrow_mut();
                pool.remove(&key)
            };
            drop(removed); // avoid `already borrowed`

            // println!("finalized {}", key);

            Ok(Value::Nil(None))
        };
        vec_class
            .decl_static("finalize".to_owned(), assoc_func!(finalize, 1..=1))
            .unwrap();
    }

    {
        let pool = Rc::clone(&pool);
        let equals = move |args: Vec<Value>| -> Result<Value> {
            let this = obj!(args, 0);
            let that = obj!(args, 1);

            pool.borrow_mut().entry(this.ptr()).or_insert_with(Vec::new);
            pool.borrow_mut().entry(that.ptr()).or_insert_with(Vec::new);

            let pool = pool.borrow();
            let this_vec = pool.get(&this.ptr());
            let that_vec = pool.get(&that.ptr());

            Ok(Value::Bool(this_vec == that_vec))
        };
        vec_class
            .decl_static("equals".to_owned(), assoc_func!(equals, 2..=2))
            .unwrap();
    }

    Value::Class(vec_class)
}
