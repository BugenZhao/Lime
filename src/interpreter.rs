use std::{collections::HashMap, sync::Mutex};

use crate::{
    error::{Error, Result},
    parser::Ident,
};

use crate::parser::{self, Expr, Op, Stmt, Value};

pub struct Interpreter {
    vars: Mutex<HashMap<Ident, Value>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            vars: Mutex::new(HashMap::new()),
        }
    }
}

impl Interpreter {
    pub fn eval(&self, text: &str) -> Result<Option<Value>> {
        let stmts = parser::parse(text)?;
        self.eval_stmts(&stmts)
    }

    fn eval_stmts(&self, stmts: &[Stmt]) -> Result<Option<Value>> {
        let mut ret = None;

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

    fn eval_stmt(&self, stmt: &Stmt) -> Result<Option<Value>> {
        match stmt {
            Stmt::Expr(expr) => match self.eval_expr(expr) {
                Ok(v) => Ok(Some(v)),
                Err(e) => Err(e),
            },
            Stmt::VarDecl(ident, val) => {
                let val = self.eval_expr(val)?;
                self.vars.lock().unwrap().insert(ident.clone(), val.clone());
                Ok(None)
            }
            Stmt::Print(expr) => match self.eval_expr(expr) {
                Ok(v) => {
                    println!("{}", v);
                    Ok(None)
                }
                Err(e) => Err(e),
            },
        }
    }

    fn eval_expr(&self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Variable(ident) => match self.vars.lock().unwrap().get(ident) {
                Some(value) => Ok(value.clone()),
                None => Err(Error::CannotFindValue(ident.0.to_owned())),
            },
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Binary(lhs, op, rhs) => match (self.eval_expr(lhs)?, self.eval_expr(rhs)?, op) {
                (Value::Int(a), Value::Int(b), op) => Ok(Value::Int(match op {
                    Op::Add => a + b,
                    Op::Sub => a - b,
                    Op::Mul => a * b,
                    Op::Div => a / b,
                    Op::Pow => a.pow(b as u32),
                })),
                (Value::Float(a), Value::Float(b), op) => Ok(Value::Float(match op {
                    Op::Add => a + b,
                    Op::Sub => a - b,
                    Op::Mul => a * b,
                    Op::Div => a / b,
                    Op::Pow => a.powf(b),
                })),
                (Value::Float(a), Value::Int(b), Op::Pow) => Ok(Value::Float(a.powi(b as i32))),
                (l, r, op) => Err(Error::CannotApplyBinaryOp(op.clone(), l, r)),
            },
            Expr::Assign(ident, val) => {
                let val = self.eval_expr(val)?;
                if let Some(v) = self.vars.lock().unwrap().get_mut(ident) {
                    *v = val.clone();
                    Ok(val)
                } else {
                    Err(Error::CannotFindValue(ident.0.to_owned()))
                }
            }
            Expr::Cast(val, ident) => {
                let val = self.eval_expr(val)?;
                match ident.0.as_str() {
                    "Int" => match val {
                        v @ Value::Int(_) => Ok(v),
                        Value::Float(f) => Ok(Value::Int(f as i64)),
                    },
                    "Float" => match val {
                        Value::Int(i) => Ok(Value::Float(i as f64)),
                        v @ Value::Float(_) => Ok(v),
                    },
                    tp @ _ => Err(Error::CannotCast(val, tp.to_owned())),
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let intp = Interpreter::new();
        let _r = intp.eval("var a = 1 + 2 * 3;").unwrap();
        assert_eq!(
            intp.vars
                .lock()
                .unwrap()
                .get(&Ident("a".to_owned()))
                .unwrap(),
            &Value::Int(7)
        );
    }
}
