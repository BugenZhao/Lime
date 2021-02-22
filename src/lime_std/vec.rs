use crate::{
    ba_rc,
    env::Env,
    err,
    value::{Class, WrFunc},
    ErrType, Result, Value,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use uuid::Uuid;

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

    macro_rules! uuid {
        ($args:expr) => {
            if let Value::Object(obj) = $args.get(0).unwrap() {
                obj.uuid()
            } else {
                unreachable!()
            }
        };
    }

    type VecMap = HashMap<Uuid, Vec<Value>>;
    let vec_map = Rc::new(RefCell::new(VecMap::new()));

    let mut vec_class = Class::new("Vec".to_owned(), Vec::new());

    {
        let vec_map = Rc::clone(&vec_map);
        let get = move |args: Vec<Value>| -> Result<Value> {
            let uuid = uuid!(args);

            let idx_v = args.get(1).unwrap();
            let idx = if let Value::Int(idx) = idx_v {
                *idx as usize
            } else {
                return Err(err!(ErrType::TypeError("Int".to_owned(), idx_v.clone())));
            };

            let mut map = vec_map.borrow_mut();
            let entry = map.entry(uuid).or_insert_with(Vec::new);

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
        let vec_map = Rc::clone(&vec_map);
        let set = move |args: Vec<Value>| -> Result<Value> {
            let uuid = uuid!(args);

            let idx_v = args.get(1).unwrap();
            let idx = if let Value::Int(idx) = idx_v {
                *idx as usize
            } else {
                return Err(err!(ErrType::TypeError("Int".to_owned(), idx_v.clone())));
            };

            let to_set = args.get(2).unwrap().clone();

            let mut map = vec_map.borrow_mut();
            let entry = map.entry(uuid).or_insert_with(Vec::new);

            if let Some(v) = entry.get_mut(idx as usize) {
                *v = to_set.clone();
                Ok(to_set)
            } else {
                Ok(Value::Nil(Some("Index out of range".to_owned())))
            }
        };
        vec_class
            .decl_static("set".to_owned(), assoc_func!(set, 3..=3))
            .unwrap();
    }

    {
        let vec_map = Rc::clone(&vec_map);
        let len = move |args: Vec<Value>| -> Result<Value> {
            let uuid = uuid!(args);

            let mut map = vec_map.borrow_mut();
            let entry = map.entry(uuid).or_insert_with(Vec::new);

            Ok(Value::Int(entry.len() as i64))
        };
        vec_class
            .decl_static("len".to_owned(), assoc_func!(len, 1..=1))
            .unwrap();
    }

    {
        let vec_map = Rc::clone(&vec_map);
        let push = move |args: Vec<Value>| -> Result<Value> {
            let uuid = uuid!(args);

            let to_push = args.get(1).unwrap().clone();

            let mut map = vec_map.borrow_mut();
            let entry = map.entry(uuid).or_insert_with(Vec::new);

            entry.push(to_push.clone());

            Ok(to_push)
        };
        vec_class
            .decl_static("push".to_owned(), assoc_func!(push, 2..=2))
            .unwrap();
    }

    {
        let vec_map = Rc::clone(&vec_map);
        let pop = move |args: Vec<Value>| -> Result<Value> {
            let uuid = uuid!(args);

            let mut map = vec_map.borrow_mut();
            let entry = map.entry(uuid).or_insert_with(Vec::new);

            Ok(entry
                .pop()
                .unwrap_or_else(|| Value::Nil(Some("No element to pop".to_owned()))))
        };
        vec_class
            .decl_static("pop".to_owned(), assoc_func!(pop, 1..=1))
            .unwrap();
    }

    {
        let vec_map = Rc::clone(&vec_map);
        vec_class.finalize = Some(ba_rc!(move |obj| {
            vec_map.borrow_mut().remove(&obj.uuid());
        }));
    }

    {
        let vec_map = Rc::clone(&vec_map);
        vec_class.equals = Some(ba_rc!(move |this, that| {
            vec_map
                .borrow_mut()
                .entry(this.uuid())
                .or_insert_with(Vec::new);
            vec_map
                .borrow_mut()
                .entry(that.uuid())
                .or_insert_with(Vec::new);

            let vec_map = vec_map.borrow();
            let this_vec = vec_map.get(&this.uuid());
            let that_vec = vec_map.get(&that.uuid());

            this_vec == that_vec
        }));
    }

    Value::Class(ba_rc!(RefCell::new(vec_class)))
}
