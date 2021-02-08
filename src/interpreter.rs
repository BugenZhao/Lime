use std::{collections::HashMap, sync::Mutex};

use crate::error::{Error, Result};

use crate::parser::{self, Expr, Op, Stmt, Value};

pub struct Interpreter {
    vars: Mutex<HashMap<String, Value>>,
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
            Stmt::VarDecl(var, val) => {
                if let Expr::Variable(name) = var.as_ref() {
                    let val = self.eval_expr(val)?;
                    self.vars.lock().unwrap().insert(name.clone(), val.clone());
                    Ok(None)
                } else {
                    Err(Error::InvalidLhsAssignment(format!("{:?}", var)))
                }
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
            Expr::Variable(name) => match self.vars.lock().unwrap().get(name) {
                Some(value) => Ok(value.clone()),
                None => Err(Error::CannotFindValue(name.to_owned())),
            },
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Binary(lhs, op, rhs) => match (self.eval_expr(lhs)?, self.eval_expr(rhs)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(match op {
                    Op::Add => a + b,
                    Op::Sub => a - b,
                    Op::Mul => a * b,
                    Op::Div => a - b,
                    Op::Pow => a.pow(b as u32),
                })),
            },
            Expr::Assign(var, val) => {
                if let Expr::Variable(name) = var.as_ref() {
                    let val = self.eval_expr(val)?;
                    if let Some(v) = self.vars.lock().unwrap().get_mut(name) {
                        *v = val.clone();
                        Ok(val)
                    } else {
                        Err(Error::CannotFindValue(name.to_owned()))
                    }
                } else {
                    Err(Error::InvalidLhsAssignment(format!("{:?}", var)))
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
        assert_eq!(intp.vars.lock().unwrap().get("a").unwrap(), &Value::Int(7));
    }
}
