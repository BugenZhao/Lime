use crate::{
    ba_rc,
    error::{Error, Result},
    lime_std::define_std,
    parser::{BinaryOp, Expr, Ident, Stmt, UnaryOp},
    rc_refcell,
    value::{Class, FuncType, Object},
    Func, Value,
};
use by_address::ByAddress;
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap, HashSet},
    ops::Deref,
    rc::Rc,
};

pub struct Env {
    vars: RefCell<HashMap<String, Value>>,
    enclosing: Option<Rc<Self>>,
    safe: bool,
}

impl Env {
    pub fn new_global_std() -> Rc<Self> {
        let env = Rc::new(Self {
            vars: RefCell::new(HashMap::new()),
            enclosing: None,
            safe: true,
        });
        define_std(&env);
        env
    }

    pub fn new(enclosing: Rc<Self>) -> Self {
        Self {
            vars: RefCell::new(HashMap::new()),
            enclosing: Some(enclosing),
            safe: true,
        }
    }

    pub fn new_empty() -> Self {
        Self {
            vars: RefCell::new(HashMap::new()),
            enclosing: None,
            safe: true,
        }
    }

    pub fn get(&self, ident: &Ident) -> Option<Value> {
        match ident.1 {
            Some(step) => Some(self.get_at_step(&ident.0, step)),
            None => self.get_raw(&ident.0),
        }
    }

    fn get_raw(&self, id_name: &str) -> Option<Value> {
        let r = self.vars.borrow().get(id_name).cloned();

        if r.is_some() {
            r
        } else if let Some(enclosing) = self.enclosing.as_deref() {
            enclosing.get_raw(id_name)
        } else {
            None
        }
    }

    fn get_at_step(&self, id_name: &str, step: usize) -> Value {
        if step == 0 {
            self.vars.borrow().get(id_name).unwrap().to_owned()
        } else {
            self.enclosing
                .as_deref()
                .unwrap()
                .get_at_step(id_name, step - 1)
        }
    }

    pub fn decl(&self, ident: Ident, mut val: Value) -> Result<()> {
        if self.safe {
            if let Value::Nil = val {
                return Err(Error::CannotHaveValue(ident.0, val));
            }
        }
        if let Value::Func(func) = &val {
            if func.name.is_none() {
                val = Value::Func(ba_rc!(func.as_ref().clone().with_name(ident.0.clone())))
            }
        }
        self.vars.borrow_mut().insert(ident.0, val);
        Ok(())
    }

    fn decl_class(&self, ident: Ident, val: Value) -> Result<()> {
        assert!(matches!(val, Value::Class(..)));
        match self.vars.borrow_mut().entry(ident.0.clone()) {
            Entry::Occupied(_) => Err(Error::DefinedMutlipleTimes(ident.0)),
            Entry::Vacant(e) => {
                e.insert(val);
                Ok(())
            }
        }
    }

    fn assign(&self, ident: &Ident, mut val: Value) -> Result<()> {
        if self.safe {
            if let Value::Nil = val {
                return Err(Error::CannotHaveValue(ident.0.to_owned(), val));
            }
        }
        if let Some(v) = self.vars.borrow_mut().get_mut(&ident.0) {
            if let Value::Func(func) = &val {
                if func.name.is_none() {
                    val = Value::Func(ba_rc!(func.as_ref().clone().with_name(ident.0.clone())))
                }
            }
            *v = val;
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.assign(ident, val)
        } else {
            Err(Error::CannotFindValue(ident.0.to_owned()))
        }
    }
}

impl Env {
    pub fn names(&self) -> Vec<String> {
        self.vars.borrow().keys().cloned().collect::<Vec<_>>()
    }
}

impl Env {
    fn is_truthy(self: &Rc<Self>, expr: &Expr) -> Result<bool> {
        match self.eval_expr(expr)? {
            Value::Bool(true) => Ok(true),
            Value::Bool(false) => Ok(false),
            v => Err(Error::CannotBeCondition(v)),
        }
    }

    pub fn eval_stmts(self: &Rc<Self>, stmts: &[Stmt]) -> Result<Value> {
        let mut ret = Value::Nil;

        for stmt in stmts.iter() {
            match self.eval_stmt(stmt) {
                Ok(ov) => {
                    ret = ov;
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }

        Ok(ret)
    }

    fn eval_stmt(self: &Rc<Self>, stmt: &Stmt) -> Result<Value> {
        match stmt {
            Stmt::VarDecl(ident, val) => {
                let val = self.eval_expr(val)?;
                self.decl(ident.clone(), val)?;
                Ok(Value::Nil)
            }
            Stmt::Expr(expr) => match self.eval_expr(expr) {
                Ok(v) => Ok(v),
                Err(e) => Err(e),
            },
            Stmt::Print(expr) => match self.eval_expr(expr) {
                Ok(v) => {
                    println!("{}", v);
                    Ok(Value::Nil)
                }
                Err(e) => Err(e),
            },
            Stmt::Assert(_, _, text, expr) => {
                let val = self.eval_expr(expr)?;
                if val != Value::Bool(true) {
                    Err(Error::AssertionFailed(
                        text.to_owned(),
                        val,
                        Value::Bool(true),
                    ))
                } else {
                    Ok(Value::Nil)
                }
            }
            Stmt::Break(expr) => {
                let val = if let Some(e) = expr {
                    self.eval_expr(e)?
                } else {
                    Value::Nil
                };
                Err(Error::Break(val))
            }
            Stmt::Continue(expr) => {
                let val = if let Some(e) = expr {
                    self.eval_expr(e)?
                } else {
                    Value::Nil
                };
                Err(Error::Continue(val))
            }
            Stmt::Return(expr) => {
                let val = if let Some(e) = expr {
                    self.eval_expr(e)?
                } else {
                    Value::Nil
                };
                Err(Error::Return(val))
            }
            Stmt::ClassDecl(ident, fields) => {
                let val = Value::Class(ba_rc!(RefCell::new(Class {
                    name: ident.0.clone(),
                    fields: fields.iter().map(|i| i.0.to_owned()).collect(),
                    statics: HashMap::new(),
                })));
                self.decl_class(ident.clone(), val)?;
                Ok(Value::Nil)
            }
            Stmt::Impl(ident, assocs) => {
                let v = self
                    .get(ident)
                    .ok_or_else(|| Error::CannotFindValue(ident.0.to_owned()))?;
                if let Value::Class(class) = &v {
                    for (i, e) in assocs.iter() {
                        class
                            .borrow_mut()
                            .decl_static(i.0.clone(), self.eval_expr(e)?)?;
                    }
                    Ok(Value::Nil)
                } else {
                    Err(Error::NotAClass(v))
                }
            }
        }
    }

    fn eval_expr(self: &Rc<Self>, expr: &Expr) -> Result<Value> {
        macro_rules! int {
            ($v:expr) => {
                Some(Value::Int($v))
            };
        }
        macro_rules! float {
            ($v:expr) => {
                Some(Value::Float($v))
            };
        }
        macro_rules! bool {
            ($v:expr) => {
                Some(Value::Bool($v))
            };
        }
        macro_rules! string {
            ($v:expr) => {
                Some(Value::String($v))
            };
        }
        macro_rules! class_check {
            ($a:expr, $b:expr, $v:expr) => {
                if $a.borrow().class == $b.borrow().class {
                    $v
                } else {
                    None
                }
            };
            ($a:expr, $b:expr, $v1:expr, $v2:expr) => {
                if $a.borrow().class == $b.borrow().class {
                    $v1
                } else {
                    $v2
                }
            };
        }

        match expr {
            Expr::Variable(ident) => match self.get(ident) {
                Some(value) => Ok(value),
                None => Err(Error::CannotFindValue(ident.0.to_owned())),
            },
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Binary(lhs, op @ BinaryOp::Or, rhs) => {
                let l = self.eval_expr(lhs)?;
                match l {
                    Value::Bool(true) => Ok(Value::Bool(true)),
                    Value::Bool(false) => {
                        let r = self.eval_expr(rhs)?;
                        match r {
                            Value::Bool(b) => Ok(Value::Bool(b)),
                            _ => Err(Error::CannotApplyBinaryOp(op.clone(), l, r)),
                        }
                    }
                    _ => Err(Error::CannotApplyBinaryOpSc(op.clone(), l)),
                }
            }
            Expr::Binary(lhs, op, rhs) => {
                assert_ne!(*op, BinaryOp::Or);
                let (l, r) = (self.eval_expr(lhs)?, self.eval_expr(rhs)?);

                match (l.clone(), r.clone(), op) {
                    (Value::Object(a), Value::Object(b), op) => match op {
                        BinaryOp::Feq => class_check!(a, b, bool!(a == b), bool!(false)),
                        BinaryOp::Fne => class_check!(a, b, bool!(a != b), bool!(true)),
                        BinaryOp::Req => class_check!(a, b, bool!(Rc::ptr_eq(&a, &b))),
                        BinaryOp::Rne => class_check!(a, b, bool!(!Rc::ptr_eq(&a, &b))),

                        BinaryOp::Eq => class_check!(a, b, bool!(a == b)),
                        BinaryOp::Ne => class_check!(a, b, bool!(a != b)),

                        _ => None,
                    },

                    (a, b, BinaryOp::Feq) => bool!(a == b),
                    (a, b, BinaryOp::Fne) => bool!(a != b),

                    (Value::Int(a), Value::Int(b), op) => match op {
                        BinaryOp::Add => int!(a + b),
                        BinaryOp::Sub => int!(a - b),
                        BinaryOp::Mul => int!(a * b),
                        BinaryOp::Div => int!(a / b),
                        BinaryOp::Pow => int!(a.pow(b as u32)),

                        BinaryOp::Feq | BinaryOp::Fne => unreachable!(),
                        BinaryOp::Req => bool!(a == b),
                        BinaryOp::Rne => bool!(a != b),

                        BinaryOp::Eq => bool!(a == b),
                        BinaryOp::Ne => bool!(a != b),
                        BinaryOp::Gt => bool!(a > b),
                        BinaryOp::Ge => bool!(a >= b),
                        BinaryOp::Lt => bool!(a < b),
                        BinaryOp::Le => bool!(a <= b),

                        BinaryOp::And => None,
                        BinaryOp::Or => unreachable!(),
                    },
                    #[allow(clippy::float_cmp)]
                    (Value::Float(a), Value::Float(b), op) => match op {
                        BinaryOp::Add => float!(a + b),
                        BinaryOp::Sub => float!(a - b),
                        BinaryOp::Mul => float!(a * b),
                        BinaryOp::Div => float!(a / b),
                        BinaryOp::Pow => float!(a.powf(b)),

                        BinaryOp::Feq | BinaryOp::Fne => unreachable!(),
                        BinaryOp::Req => bool!(a == b),
                        BinaryOp::Rne => bool!(a != b),

                        BinaryOp::Eq => bool!(a == b),
                        BinaryOp::Ne => bool!(a != b),
                        BinaryOp::Gt => bool!(a > b),
                        BinaryOp::Ge => bool!(a >= b),
                        BinaryOp::Lt => bool!(a < b),
                        BinaryOp::Le => bool!(a <= b),

                        BinaryOp::And => None,
                        BinaryOp::Or => unreachable!(),
                    },
                    #[allow(clippy::bool_comparison)]
                    (Value::Bool(a), Value::Bool(b), op) => match op {
                        BinaryOp::Feq | BinaryOp::Fne => unreachable!(),
                        BinaryOp::Req => bool!(a == b),
                        BinaryOp::Rne => bool!(a != b),

                        BinaryOp::Eq => bool!(a == b),
                        BinaryOp::Ne => bool!(a != b),
                        BinaryOp::Gt => bool!(a > b),
                        BinaryOp::Ge => bool!(a >= b),
                        BinaryOp::Lt => bool!(a < b),
                        BinaryOp::Le => bool!(a <= b),

                        BinaryOp::And => bool!(a && b),
                        BinaryOp::Or => unreachable!(),

                        _ => None,
                    },
                    (Value::String(a), Value::String(b), op) => match op {
                        BinaryOp::Add => string!(a + &b),

                        BinaryOp::Feq | BinaryOp::Fne => unreachable!(),
                        BinaryOp::Req => bool!(a == b),
                        BinaryOp::Rne => bool!(a != b),

                        BinaryOp::Eq => bool!(a == b),
                        BinaryOp::Ne => bool!(a != b),
                        BinaryOp::Gt => bool!(a > b),
                        BinaryOp::Ge => bool!(a >= b),
                        BinaryOp::Lt => bool!(a < b),
                        BinaryOp::Le => bool!(a <= b),

                        _ => None,
                    },
                    (Value::Func(a), Value::Func(b), op) => match op {
                        // TODO: composed func
                        BinaryOp::Mul => todo!("func composition"),

                        BinaryOp::Feq | BinaryOp::Fne => unreachable!(),
                        BinaryOp::Req => bool!(a == b),
                        BinaryOp::Rne => bool!(a != b),

                        BinaryOp::Eq => bool!(a == b),
                        BinaryOp::Ne => bool!(a != b),

                        _ => None,
                    },
                    (Value::Class(a), Value::Class(b), op) => match op {
                        BinaryOp::Feq | BinaryOp::Fne => unreachable!(),
                        BinaryOp::Req => bool!(a == b),
                        BinaryOp::Rne => bool!(a != b),

                        BinaryOp::Eq => bool!(a == b),
                        BinaryOp::Ne => bool!(a != b),

                        _ => None,
                    },

                    (Value::Float(a), Value::Int(b), BinaryOp::Pow) => {
                        float!(a.powi(b as i32))
                    }

                    (_, _, _) => None,
                }
                .ok_or_else(|| Error::CannotApplyBinaryOp(op.clone(), l, r))
            }
            Expr::Unary(op, val) => match (self.eval_expr(val)?, op) {
                (Value::Int(x), UnaryOp::Neg) => Ok(Value::Int(-x)),
                (Value::Float(x), UnaryOp::Neg) => Ok(Value::Float(-x)),
                (Value::Bool(x), UnaryOp::Not) => Ok(Value::Bool(!x)),
                (v, op) => Err(Error::CannotApplyUnaryOp(op.clone(), v)),
            },
            Expr::Assign(ident, val) => {
                let val = self.eval_expr(val)?;
                self.assign(ident, val.clone()).map(|_| val)
            }
            Expr::Cast(val, ident) => {
                let val = self.eval_expr(val)?;
                let tp = ident.0.as_str();
                match tp {
                    "Int" => match val {
                        Value::Int(x) => Some(Value::Int(x as i64)),
                        Value::Float(x) => Some(Value::Int(x as i64)),
                        Value::Bool(x) => Some(Value::Int(x as i64)),
                        Value::String(_)
                        | Value::Nil
                        | Value::Func(..)
                        | Value::Class(..)
                        | Value::Object(..) => None,
                    },
                    "Float" => match val {
                        Value::Int(x) => Some(Value::Float(x as f64)),
                        Value::Float(x) => Some(Value::Float(x as f64)),
                        Value::Bool(_) => None,
                        Value::String(_)
                        | Value::Nil
                        | Value::Func(..)
                        | Value::Class(..)
                        | Value::Object(..) => None,
                    },
                    "Bool" => None,
                    "String" => Some(Value::String(format!("{}", val))),
                    _ => None,
                }
                .ok_or_else(|| Error::CannotCast(val, tp.to_owned()))
            }
            Expr::Block(stmts) => {
                let new_env = Rc::new(Env::new(Rc::clone(&self)));
                new_env.eval_stmts(stmts)
            }
            Expr::If(cond, then, else_) => {
                if self.is_truthy(cond)? {
                    self.eval_expr(then)
                } else if let Some(else_) = else_.deref() {
                    self.eval_expr(else_)
                } else {
                    Ok(Value::Nil)
                }
            }
            Expr::While(cond, body, default) => {
                let mut ret = Value::Nil;
                let mut looped = false;

                while self.is_truthy(cond)? {
                    looped = true;
                    match self.eval_expr(body) {
                        Ok(v) | Err(Error::Continue(v)) => ret = v,
                        Err(Error::Break(v)) => {
                            ret = v;
                            break;
                        }
                        err @ Err(_) => {
                            return err;
                        }
                    }
                }

                if looped {
                    Ok(ret)
                } else if let Some(default) = default.deref() {
                    self.eval_expr(default)
                } else {
                    Ok(Value::Nil)
                }
            }
            Expr::Call(callee, arg_exprs) => match self.eval_expr(callee)? {
                Value::Func(lime_f) => {
                    let n_arg = arg_exprs.len();
                    let _ = lime_f.check(n_arg)?;

                    let mut args = vec![];
                    for arg_expr in arg_exprs.iter() {
                        args.push(self.eval_expr(arg_expr)?);
                    }

                    match lime_f.call(args) {
                        Ok(v) | Err(Error::Return(v)) => Ok(v),
                        err @ Err(_) => err,
                    }
                }
                v => Err(Error::NotCallable(v)),
            },
            Expr::Func(params, body) => Ok(Value::Func(ba_rc!(Func {
                tp: FuncType::Lime(params.clone(), body.clone()),
                arity: params.len()..=params.len(),
                env: Rc::clone(&self),
                name: None,
            }))),
            Expr::Construct(ident, kvs) => {
                let v = self
                    .get(ident)
                    .ok_or_else(|| Error::CannotFindValue(ident.0.to_owned()))?;
                if let Value::Class(ByAddress(class)) = &v {
                    let keys_set = kvs
                        .iter()
                        .map(|(k, _)| &k.0)
                        .cloned()
                        .collect::<HashSet<_>>();
                    let expected_keys_set = class
                        .borrow()
                        .fields
                        .iter()
                        .cloned()
                        .collect::<HashSet<_>>();

                    if keys_set == expected_keys_set {
                        let mut values = vec![];
                        for expr in kvs.iter().map(|(_, v)| v) {
                            values.push(self.eval_expr(expr)?);
                        }
                        let fields = kvs
                            .iter()
                            .map(|(k, _)| k.0.to_owned())
                            .zip(values.into_iter())
                            .collect();
                        Ok(Value::Object(rc_refcell!(Object {
                            class: Rc::clone(class),
                            fields,
                        })))
                    } else {
                        Err(Error::WrongFields(v))
                    }
                } else {
                    Err(Error::NotAClass(v))
                }
            }
            Expr::Get(expr, field) => {
                let v = self.eval_expr(expr)?;
                match &v {
                    Value::Class(class) => class.borrow().statics.get(&field.0).cloned(),
                    Value::Object(obj) => {
                        if let Some(field_val) = obj.borrow().fields.get(&field.0).cloned() {
                            Some(field_val)
                        } else if let Some(static_val) =
                            obj.borrow().class.borrow().statics.get(&field.0).cloned()
                        {
                            if let Value::Func(func) = static_val {
                                Some(Value::Func(ba_rc!(Func::new_parital_apply(
                                    func.as_ref().clone(),
                                    Value::Object(Rc::clone(obj))
                                )?)))
                            } else {
                                Some(static_val)
                            }
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
                .ok_or_else(|| Error::NoField(v.clone(), field.0.clone()))
            }
            Expr::Set(expr, field, val) => {
                let v = self.eval_expr(expr)?;
                match &v {
                    Value::Class(class) => {
                        // val may be related to obj, do not borrow mutably too early
                        let val = self.eval_expr(val)?;

                        let mut class = class.borrow_mut();
                        let field = class.statics.get_mut(&field.0);
                        match field {
                            Some(field) => {
                                *field = val.clone();
                                Some(val)
                            }
                            None => None,
                        }
                    }
                    Value::Object(obj) => {
                        // val may be related to obj, do not borrow mutably too early
                        let val = self.eval_expr(val)?;

                        let mut obj = obj.borrow_mut();
                        let field = obj.fields.get_mut(&field.0);
                        match field {
                            Some(field) => {
                                *field = val.clone();
                                Some(val)
                            }
                            None => None,
                            // TODO: give clearer error reporting for setting a static field through object
                        }
                    }
                    _ => None,
                }
                .ok_or_else(|| Error::NoFieldToSet(v, field.0.clone()))
            }
        }
    }
}
