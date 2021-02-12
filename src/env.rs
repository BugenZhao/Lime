use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};

use parser::UnaryOp;

use crate::{
    error::{Error, Result},
    lime_std::define_std,
    parser::{self, BinaryOp, Expr, Ident, Stmt},
    value::FuncType,
    Func, Value,
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
            self.vars.borrow().get(id_name).cloned().unwrap()
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
                return Err(Error::CannotHaveValue(ident.0.to_owned(), val));
            }
        }
        if let Value::Func(func) = val {
            val = Value::Func(func.with_name(ident.0.clone()))
        }
        self.vars.borrow_mut().insert(ident.0, val);
        Ok(())
    }

    fn assign(&self, ident: &Ident, val: Value) -> Result<()> {
        if self.safe {
            if let Value::Nil = val {
                return Err(Error::CannotHaveValue(ident.0.to_owned(), val));
            }
        }
        if let Some(v) = self.vars.borrow_mut().get_mut(&ident.0) {
            *v = val.clone();
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
            v @ _ => Err(Error::CannotBeCondition(v)),
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
                self.decl(ident.clone(), val.clone())?;
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

        match expr {
            Expr::Variable(ident) => match self.get(ident) {
                Some(value) => Ok(value.clone()),
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
                    (a, b, BinaryOp::Teq) => bool!(a == b),
                    (a, b, BinaryOp::Tne) => bool!(a != b),
                    (Value::Int(a), Value::Int(b), op) => match op {
                        BinaryOp::Add => int!(a + b),
                        BinaryOp::Sub => int!(a - b),
                        BinaryOp::Mul => int!(a * b),
                        BinaryOp::Div => int!(a / b),
                        BinaryOp::Pow => int!(a.pow(b as u32)),

                        BinaryOp::Teq | BinaryOp::Tne => unreachable!(),
                        BinaryOp::Eq => bool!(a == b),
                        BinaryOp::Ne => bool!(a != b),
                        BinaryOp::Gt => bool!(a > b),
                        BinaryOp::Ge => bool!(a >= b),
                        BinaryOp::Lt => bool!(a < b),
                        BinaryOp::Le => bool!(a <= b),

                        BinaryOp::And => None,
                        BinaryOp::Or => unreachable!(),
                    },
                    (Value::Float(a), Value::Float(b), op) => match op {
                        BinaryOp::Add => float!(a + b),
                        BinaryOp::Sub => float!(a - b),
                        BinaryOp::Mul => float!(a * b),
                        BinaryOp::Div => float!(a / b),
                        BinaryOp::Pow => float!(a.powf(b)),

                        BinaryOp::Teq | BinaryOp::Tne => unreachable!(),
                        BinaryOp::Eq => bool!(a == b),
                        BinaryOp::Ne => bool!(a != b),
                        BinaryOp::Gt => bool!(a > b),
                        BinaryOp::Ge => bool!(a >= b),
                        BinaryOp::Lt => bool!(a < b),
                        BinaryOp::Le => bool!(a <= b),

                        BinaryOp::And => None,
                        BinaryOp::Or => unreachable!(),
                    },
                    (Value::Bool(a), Value::Bool(b), op) => match op {
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

                        BinaryOp::Eq => bool!(a == b),
                        BinaryOp::Ne => bool!(a != b),

                        _ => None,
                    },
                    (Value::Float(a), Value::Int(b), BinaryOp::Pow) => {
                        float!(a.powi(b as i32))
                    }

                    (_, _, _) => None,
                }
                .ok_or(Error::CannotApplyBinaryOp(op.clone(), l, r))
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
                        Value::String(_) | Value::Nil | Value::Func(..) => None,
                    },
                    "Float" => match val {
                        Value::Int(x) => Some(Value::Float(x as f64)),
                        Value::Float(x) => Some(Value::Float(x as f64)),
                        Value::Bool(_) => None,
                        Value::String(_) | Value::Nil | Value::Func(..) => None,
                    },
                    "Bool" => None,
                    "String" => Some(Value::String(format!("{}", val))),
                    _ => None,
                }
                .ok_or(Error::CannotCast(val, tp.to_owned()))
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
                v @ _ => Err(Error::NotCallable(v)),
            },
            Expr::Func(params, body) => Ok(Value::Func(Func {
                tp: FuncType::Lime(params.clone(), body.clone()),
                arity: params.len()..=params.len(),
                env: Rc::clone(&self),
                name: None,
            })),
        }
    }
}
