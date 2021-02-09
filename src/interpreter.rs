use std::{fs::read_to_string, path::Path, str::Chars, sync::Mutex};

use crate::{
    env::Env,
    error::{Error, Result},
};

use crate::parser::{self, BinaryOp, Expr, Stmt, Value};

pub struct Interpreter {
    env: Mutex<Env>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Mutex::new(Env::new()),
        }
    }
}

impl Interpreter {
    pub fn eval_file<P: AsRef<Path>>(&self, path: P) -> Result<Option<Value>> {
        self.eval(&read_to_string(path)?)
    }

    pub fn eval(&self, text: &str) -> Result<Option<Value>> {
        let stmts = parser::parse(text)?;
        self.eval_stmts(&stmts, &text.chars().to_owned())
    }

    fn eval_stmts(&self, stmts: &[Stmt], text: &Chars) -> Result<Option<Value>> {
        let mut ret = None;

        for stmt in stmts.iter() {
            match self.eval_stmt(stmt, text) {
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

    fn eval_stmt(&self, stmt: &Stmt, text: &Chars) -> Result<Option<Value>> {
        match stmt {
            Stmt::Expr(expr) => match self.eval_expr(expr) {
                Ok(v) => Ok(Some(v)),
                Err(e) => Err(e),
            },
            Stmt::VarDecl(ident, val) => {
                let val = self.eval_expr(val)?;
                self.env.lock().unwrap().decl(ident.clone(), val.clone());
                Ok(None)
            }
            Stmt::Print(expr) => match self.eval_expr(expr) {
                Ok(v) => {
                    println!("{}", v);
                    Ok(None)
                }
                Err(e) => Err(e),
            },
            Stmt::Assert(start, end, expr) => {
                let val = self.eval_expr(expr)?;
                if val != Value::Bool(true) {
                    Err(Error::AssertionFailed(
                        text.to_owned().skip(*start).take(*end - *start).collect(),
                        val,
                        Value::Bool(true),
                    ))
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn eval_expr(&self, expr: &Expr) -> Result<Value> {
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
            Expr::Variable(ident) => match self.env.lock().unwrap().get(ident) {
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
                    (Value::Int(a), Value::Int(b), op) => match op {
                        BinaryOp::Add => int!(a + b),
                        BinaryOp::Sub => int!(a - b),
                        BinaryOp::Mul => int!(a * b),
                        BinaryOp::Div => int!(a / b),
                        BinaryOp::Pow => int!(a.pow(b as u32)),

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

                        BinaryOp::Eq => bool!(a == b),
                        BinaryOp::Ne => bool!(a != b),
                        BinaryOp::Gt => bool!(a > b),
                        BinaryOp::Ge => bool!(a >= b),
                        BinaryOp::Lt => bool!(a < b),
                        BinaryOp::Le => bool!(a <= b),

                        BinaryOp::And => None,
                        BinaryOp::Or => unreachable!(),
                    },
                    (Value::Float(a), Value::Int(b), BinaryOp::Pow) => {
                        float!(a.powi(b as i32))
                    }
                    (Value::Bool(a), Value::Bool(b), op) => match op {
                        BinaryOp::And => bool!(a && b),
                        BinaryOp::Or => unreachable!(),
                        _ => None,
                    },
                    (Value::String(a), Value::String(b), BinaryOp::Add) => {
                        string!(a + &b)
                    }
                    (_, _, _) => None,
                }
                .ok_or(Error::CannotApplyBinaryOp(op.clone(), l, r))
            }
            Expr::Unary(op, val) => match (self.eval_expr(val)?, op) {
                (Value::Int(x), parser::UnaryOp::Neg) => Ok(Value::Int(-x)),
                (Value::Float(x), parser::UnaryOp::Neg) => Ok(Value::Float(-x)),
                (Value::Bool(x), parser::UnaryOp::Not) => Ok(Value::Bool(!x)),
                (v, op) => Err(Error::CannotApplyUnaryOp(op.clone(), v)),
            },
            Expr::Assign(ident, val) => {
                let val = self.eval_expr(val)?;
                self.env
                    .lock()
                    .unwrap()
                    .assign(ident, val.clone())
                    .map(|_| val)
            }
            Expr::Cast(val, ident) => {
                let val = self.eval_expr(val)?;
                let tp = ident.0.as_str();
                match tp {
                    "Int" => match val {
                        Value::Int(x) => Some(Value::Int(x as i64)),
                        Value::Float(x) => Some(Value::Int(x as i64)),
                        Value::Bool(x) => Some(Value::Int(x as i64)),
                        Value::String(_) => None,
                    },
                    "Float" => match val {
                        Value::Int(x) => Some(Value::Float(x as f64)),
                        Value::Float(x) => Some(Value::Float(x as f64)),
                        Value::Bool(_) => None,
                        Value::String(_) => None,
                    },
                    "Bool" => None,
                    "String" => Some(Value::String(format!("{}", val))),
                    _ => None,
                }
                .ok_or(Error::CannotCast(val, tp.to_owned()))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::Ident;

    use super::*;

    #[test]
    fn test() {
        let intp = Interpreter::new();
        let _r = intp.eval("var a = 1 + 2 * 3;").unwrap();
        assert_eq!(
            intp.env
                .lock()
                .unwrap()
                .get(&Ident("a".to_owned()))
                .unwrap(),
            &Value::Int(7)
        );
    }
}
