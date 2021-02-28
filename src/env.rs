use crate::{
    ast::{BinaryOp, Expr, Ident, IdentExt, Stmt, UnaryOp},
    err,
    error::{ErrType, Result},
    lime_std::define_std,
    value::{Value, WrClass, WrFunc, WrObject},
};
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    ops::Deref,
    rc::Rc,
};

pub struct Env {
    pub(crate) vars: RefCell<HashMap<String, Value>>,
    enclosing: Option<Rc<Self>>,
}

impl Env {
    pub fn new_global_std() -> Rc<Self> {
        let env = Rc::new(Self {
            vars: RefCell::new(HashMap::new()),
            enclosing: None,
        });
        define_std(&env);
        env
    }

    pub fn new(enclosing: Rc<Self>) -> Self {
        Self {
            vars: RefCell::new(HashMap::new()),
            enclosing: Some(enclosing),
        }
    }

    pub fn new_empty() -> Self {
        Self {
            vars: RefCell::new(HashMap::new()),
            enclosing: None,
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
        if matches!(val, Value::Nil(..)) && !ident.can_hold_nil() {
            return Err(err!(ErrType::CannotHaveValue(ident.0, val)));
        }
        if ident.is_ignored() {
            return Ok(());
        }

        if let Value::Func(func) = val.clone() {
            val = Value::Func(func.try_with_name(ident.0.clone()))
        }
        self.vars.borrow_mut().insert(ident.0, val);
        Ok(())
    }

    fn decl_class(&self, ident: Ident, val: Value) -> Result<()> {
        assert!(matches!(val, Value::Class(..)));
        match self.vars.borrow_mut().entry(ident.0.clone()) {
            Entry::Occupied(_) => Err(err!(ErrType::DefinedMutlipleTimes(ident.0))),
            Entry::Vacant(e) => {
                e.insert(val);
                Ok(())
            }
        }
    }

    fn assign(&self, ident: &Ident, mut val: Value) -> Result<()> {
        if matches!(val, Value::Nil(..)) && !ident.can_hold_nil() {
            return Err(err!(ErrType::CannotHaveValue(ident.0.to_owned(), val)));
        }
        if let Some(v) = self.vars.borrow_mut().get_mut(&ident.0) {
            if let Value::Func(func) = val.clone() {
                val = Value::Func(func.try_with_name(ident.0.clone()))
            }
            *v = val;
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.assign(ident, val)
        } else {
            Err(err!(ErrType::CannotFindValue(ident.0.to_owned())))
        }
    }
}

impl Env {
    pub fn names(&self) -> Vec<String> {
        self.vars.borrow().keys().cloned().collect::<Vec<_>>()
    }
}

impl Env {
    pub fn get_internal_assoc(&self, obj: &Value, name: &str) -> Option<Value> {
        let func = self.get_raw(&format!("__{}", name))?;

        if let Value::Func(func) = func {
            Some(Value::Func(
                WrFunc::new_parital_apply(func, obj.clone()).unwrap(),
            ))
        } else {
            // TODO: more types of assoc?
            None
        }
    }
}

macro_rules! assoc_call {
    ($obj:expr, $assoc:expr) => {
        Expr::Call(
            box Expr::Get(box Expr::Literal($obj), $assoc.into()),
            vec![],
        )
    };
}

impl Env {
    pub fn eval_stmts(self: &Rc<Self>, stmts: &[Stmt]) -> Result<Value> {
        let mut ret = Value::Nil(None);

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
                Ok(Value::Nil(None))
            }
            Stmt::Expr(expr) => match self.eval_expr(expr) {
                Ok(v) => Ok(v),
                Err(e) => Err(e),
            },
            Stmt::Print(expr) => match self.eval_expr(expr) {
                Ok(v) => {
                    println!("{}", v);
                    Ok(Value::Nil(None))
                }
                Err(e) => Err(e),
            },
            Stmt::Assert(_, _, text, expr) => {
                let val = self.eval_expr(expr)?;
                if val != Value::Bool(true) {
                    Err(err!(ErrType::AssertionFailed(
                        text.to_owned(),
                        val,
                        Value::Bool(true),
                    )))
                } else {
                    Ok(Value::Nil(None))
                }
            }
            Stmt::Break(expr) => {
                let val = if let Some(e) = expr {
                    self.eval_expr(e)?
                } else {
                    Value::Nil(None)
                };
                Err(err!(ErrType::Break(val)))
            }
            Stmt::Continue(expr) => {
                let val = if let Some(e) = expr {
                    self.eval_expr(e)?
                } else {
                    Value::Nil(None)
                };
                Err(err!(ErrType::Continue(val)))
            }
            Stmt::Return(expr) => {
                let val = if let Some(e) = expr {
                    self.eval_expr(e)?
                } else {
                    Value::Nil(None)
                };
                Err(err!(ErrType::Return(val)))
            }
            Stmt::ClassDecl(ident, fields) => {
                let val = Value::Class(WrClass::new(
                    ident.0.clone(),
                    fields.iter().map(|i| i.0.to_owned()).collect(),
                ));
                self.decl_class(ident.clone(), val)?;
                Ok(Value::Nil(None))
            }
            Stmt::Impl(ident, assocs) => {
                let v = self
                    .get(ident)
                    .ok_or_else(|| err!(ErrType::CannotFindValue(ident.0.to_owned())))?;
                if let Value::Class(class) = &v {
                    for (i, e) in assocs.iter() {
                        class.decl_static(i.0.clone(), self.eval_expr(e)?)?;
                    }
                    Ok(Value::Nil(None))
                } else {
                    Err(err!(ErrType::NotAClass(v)))
                }
            }
        }
    }

    fn eval_expr(self: &Rc<Self>, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Variable(ident) => match self.get(ident) {
                Some(value) => Ok(value),
                None => Err(err!(ErrType::CannotFindValue(ident.0.to_owned()))),
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
                            _ => Err(err!(ErrType::CannotApplyBinaryOp(op.clone(), l, r))),
                        }
                    }
                    _ => Err(err!(ErrType::CannotApplyBinaryOpSc(op.clone(), l))),
                }
            }
            Expr::Binary(lhs, op, rhs) => {
                assert_ne!(*op, BinaryOp::Or);
                let (l, r) = (self.eval_expr(lhs)?, self.eval_expr(rhs)?);

                self.eval_normal_binary_op(l, r, op)
            }
            Expr::Unary(op, val) => match (self.eval_expr(val)?, op) {
                (Value::Int(x), UnaryOp::Neg) => Ok(Value::Int(-x)),
                (Value::Float(x), UnaryOp::Neg) => Ok(Value::Float(-x)),
                (Value::Bool(x), UnaryOp::Not) => Ok(Value::Bool(!x)),
                (v, op) => Err(err!(ErrType::CannotApplyUnaryOp(op.clone(), v))),
            },
            Expr::Assign(ident, val) => {
                let val = self.eval_expr(val)?;
                self.assign(ident, val.clone()).map(|_| val)
            }
            Expr::Cast(val, ident) => {
                let val = self.eval_expr(val)?;
                self.eval_cast(ident, val)
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
                    Ok(Value::Nil(None))
                }
            }
            Expr::IfVar(ident, expr, then, else_) => {
                let val = self.eval_expr(expr)?;
                if !matches!(val, Value::Nil(..)) {
                    match then.as_ref() {
                        Expr::Block(stmts) => {
                            let new_env = Rc::new(Env::new(Rc::clone(&self)));
                            new_env.decl(ident.clone(), val)?;
                            new_env.eval_stmts(stmts)
                        }
                        _ => unreachable!(),
                    }
                } else if let Some(else_) = else_.deref() {
                    self.eval_expr(else_)
                } else {
                    Ok(Value::Nil(None))
                }
            }
            Expr::While(cond, body, default) => {
                let mut ret = Value::Nil(None);
                let mut looped = false;

                while self.is_truthy(cond)? {
                    looped = true;
                    match self.eval_expr(body) {
                        Ok(v) => ret = v,
                        Err(e) => match e.tp {
                            ErrType::Continue(v) => ret = v,
                            ErrType::Break(v) => {
                                ret = v;
                                break;
                            }
                            _ => return Err(e),
                        },
                    }
                }

                if looped {
                    Ok(ret)
                } else if let Some(default) = default.deref() {
                    self.eval_expr(default)
                } else {
                    Ok(Value::Nil(None))
                }
            }
            Expr::WhileVar(ident, expr, body, default) => {
                let mut ret = Value::Nil(None);
                let mut looped = false;

                loop {
                    let val = self.eval_expr(expr)?;
                    if matches!(val, Value::Nil(..)) {
                        break;
                    }
                    looped = true;

                    let result = match body.as_ref() {
                        Expr::Block(stmts) => {
                            let new_env = Rc::new(Env::new(Rc::clone(&self)));
                            new_env.decl(ident.clone(), val)?;
                            new_env.eval_stmts(stmts)
                        }
                        _ => unreachable!(),
                    };

                    match result {
                        Ok(v) => ret = v,
                        Err(e) => match e.tp {
                            ErrType::Continue(v) => ret = v,
                            ErrType::Break(v) => {
                                ret = v;
                                break;
                            }
                            _ => return Err(e),
                        },
                    }
                }

                if looped {
                    Ok(ret)
                } else if let Some(default) = default.deref() {
                    self.eval_expr(default)
                } else {
                    Ok(Value::Nil(None))
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
                        Ok(v) => Ok(v),
                        Err(mut e) => match e.tp {
                            ErrType::Return(v) | ErrType::ErrorReturn(v) => Ok(v),
                            ErrType::Expect(v) => Err(err!(ErrType::ErrorReturn(v))),
                            _ => {
                                e.push(&lime_f);
                                Err(e)
                            }
                        },
                    }
                }
                v => Err(err!(ErrType::NotCallable(v))),
            },
            Expr::Func(params, body) => Ok(Value::Func(WrFunc::new_lime(
                params.clone(),
                body.as_block().unwrap().clone(),
                params.len()..=params.len(),
                Rc::clone(&self),
            ))),
            Expr::Construct(ident, kvs) => {
                let v = self
                    .get(ident)
                    .ok_or_else(|| err!(ErrType::CannotFindValue(ident.0.to_owned())))?;
                if let Value::Class(class) = &v {
                    if class.check_kvs(kvs) {
                        let mut values = vec![];
                        for (ident, expr) in kvs.iter() {
                            let v = self.eval_expr(expr)?;
                            let k = &ident.0;
                            if matches!(v, Value::Nil(..)) && !ident.can_hold_nil() {
                                return Err(err!(ErrType::CannotHaveValue(k.to_owned(), v)));
                            }
                            values.push(v);
                        }
                        let fields = kvs
                            .iter()
                            .map(|(k, _)| k.0.to_owned())
                            .zip(values.into_iter())
                            .collect();
                        Ok(Value::Object(WrObject::new(class.clone(), fields)))
                    } else {
                        Err(err!(ErrType::WrongFields(v)))
                    }
                } else {
                    Err(err!(ErrType::NotAClass(v)))
                }
            }
            Expr::Get(expr, field) => {
                let v = self.eval_expr(expr)?;
                if let Some(assoc) = self.get_internal_assoc(&v, &field.0) {
                    return Ok(assoc);
                }
                match &v {
                    Value::Class(class) => class.get_static(&field.0),
                    Value::Object(obj) => obj.get_field(&field.0)?,
                    v if v.is_primitive() => self.primitive_get_field(v.clone(), &field.0)?,
                    _ => None,
                }
                .ok_or_else(|| err!(ErrType::NoField(v.clone(), field.0.clone())))
            }
            Expr::Set(expr, field, val) => {
                let v = self.eval_expr(expr)?;
                match v.clone() {
                    Value::Class(class) => {
                        // val may be related to obj, do not borrow mutably too early
                        let val = self.eval_expr(val)?;
                        class.set_static(&field.0, val.clone())?;
                        Some(val)
                    }
                    Value::Object(mut obj) => {
                        // val may be related to obj, do not borrow mutably too early
                        let val = self.eval_expr(val)?;
                        obj.set_field(&field.0, val.clone())?;
                        Some(val)
                    }
                    _ => None,
                }
                .ok_or_else(|| err!(ErrType::NoFieldToSet(v, field.0.clone())))
            }
            Expr::VecLiteral(exprs) => {
                // TODO: more elegant
                let vec_obj = self.eval_expr(&Expr::Construct("Vec".into(), vec![]))?;
                let mut push_expr = assoc_call!(vec_obj.clone(), "push");

                for expr in exprs {
                    match &mut push_expr {
                        Expr::Call(_, args) => *args = vec![expr.clone()],
                        _ => unreachable!(),
                    }
                    self.eval_expr(&push_expr)?;
                }

                Ok(vec_obj)
            }
            Expr::RangeLiteral(lo, hi, inclusive) => {
                let construct_obj = Expr::Construct(
                    "Range".into(),
                    vec![
                        ("lo".into(), lo.as_ref().clone()),
                        ("hi".into(), hi.as_ref().clone()),
                        ("inclusive".into(), Expr::Literal(Value::Bool(*inclusive))),
                    ],
                );
                let range_obj = self.eval_expr(&construct_obj)?;

                Ok(range_obj)
            }
            Expr::For(ident, expr, body, default) => {
                let mut ret = Value::Nil(None);
                let mut looped = false;

                let obj = self.eval_expr(expr)?;
                let iter_expr = assoc_call!(obj, "iter");
                let iter = self.eval_expr(&iter_expr)?;
                let next_expr = assoc_call!(iter, "next");

                loop {
                    match self.eval_expr(&next_expr)? {
                        Value::Nil(Some(cause)) if cause == "stop iteration" => {
                            break;
                        }
                        val => match body.as_ref() {
                            Expr::Block(stmts) => {
                                looped = true;
                                let new_env = Rc::new(Env::new(Rc::clone(&self)));
                                new_env.decl(ident.clone(), val)?;

                                match new_env.eval_stmts(stmts) {
                                    Ok(v) => ret = v,
                                    Err(e) => match e.tp {
                                        ErrType::Continue(v) => ret = v,
                                        ErrType::Break(v) => {
                                            ret = v;
                                            break;
                                        }
                                        _ => return Err(e),
                                    },
                                }
                            }
                            _ => unreachable!(),
                        },
                    }
                }

                if looped {
                    Ok(ret)
                } else if let Some(default) = default.deref() {
                    self.eval_expr(default)
                } else {
                    Ok(Value::Nil(None))
                }
            }
        }
    }
}

impl Env {
    fn is_truthy(self: &Rc<Self>, expr: &Expr) -> Result<bool> {
        match self.eval_expr(expr)? {
            Value::Bool(true) => Ok(true),
            Value::Bool(false) => Ok(false),
            v => Err(err!(ErrType::CannotBeCondition(v))),
        }
    }

    fn eval_normal_binary_op(&self, l: Value, r: Value, op: &BinaryOp) -> Result<Value> {
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
                if $a.class_eq(&$b) {
                    $v
                } else {
                    None
                }
            };
            ($a:expr, $b:expr, $v1:expr, $v2:expr) => {
                if $a.class_eq(&$b) {
                    $v1
                } else {
                    $v2
                }
            };
        }

        match (l.clone(), r.clone(), op) {
            (Value::Object(a), Value::Object(b), op) => match op {
                BinaryOp::Feq => class_check!(a, b, bool!(a == b), bool!(false)),
                BinaryOp::Fne => class_check!(a, b, bool!(a != b), bool!(true)),
                BinaryOp::Req => class_check!(a, b, bool!(a.ref_eq(&b))),
                BinaryOp::Rne => class_check!(a, b, bool!(!a.ref_eq(&b))),

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
        .ok_or_else(|| err!(ErrType::CannotApplyBinaryOp(op.clone(), l, r)))
    }

    fn eval_cast(&self, ident: &Ident, val: Value) -> Result<Value> {
        let tp = ident.0.as_str();
        match tp {
            "Int" => match val {
                Value::Int(x) => Some(Value::Int(x as i64)),
                Value::Float(x) => Some(Value::Int(x as i64)),
                Value::Bool(x) => Some(Value::Int(x as i64)),
                Value::String(..)
                | Value::Nil(..)
                | Value::Func(..)
                | Value::Class(..)
                | Value::Object(..) => None,
            },
            "Float" => match val {
                Value::Int(x) => Some(Value::Float(x as f64)),
                Value::Float(x) => Some(Value::Float(x as f64)),
                Value::Bool(..)
                | Value::String(..)
                | Value::Nil(..)
                | Value::Func(..)
                | Value::Class(..)
                | Value::Object(..) => None,
            },
            "Bool" => None,
            "String" => Some(Value::String(format!("{}", val))),
            _ => None,
        }
        .ok_or_else(|| err!(ErrType::CannotCast(val, tp.to_owned())))
    }

    pub fn primitive_get_field(&self, v: Value, k: &str) -> Result<Option<Value>> {
        if !v.is_primitive() {
            panic!("call `primitive_get_field` on non-primitive value");
        }

        let val = if let Some(Value::Class(class)) = self.get(&v.class_name().into()) {
            if let Some(static_val) = class.get_static(k) {
                if let Value::Func(func) = static_val {
                    Some(Value::Func(WrFunc::new_parital_apply(func, v)?))
                } else {
                    Some(static_val)
                }
            } else {
                None
            }
        } else {
            None
        };

        Ok(val)
    }
}
