use std::rc::Rc;

use crate::{env::Env, value::WrClass, Value};

pub fn define_primitive(env: &Rc<Env>) {
    for &name in ["Int", "Float", "Bool", "String", "Nil"].iter() {
        env.decl(
            name.into(),
            Value::Class(WrClass::new(name.to_owned(), vec![])),
        )
        .unwrap();
    }
}
