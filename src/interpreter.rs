use std::{collections::HashMap, sync::Mutex};

use crate::peg_test::{self, Expr, Op, Stmt, Value};

struct Interpreter {
    vars: Mutex<HashMap<String, Value>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            vars: Mutex::new(HashMap::new()),
        }
    }
}

#[allow(unreachable_patterns)]
impl Interpreter {
    pub fn eval(&self, text: &str) -> Option<Value> {
        let stmts = peg_test::parse(text);
        self.eval_stmts(&stmts)
    }

    fn eval_stmts(&self, stmts: &[Stmt]) -> Option<Value> {
        stmts.iter().map(|s| self.eval_stmt(s)).last()?
    }

    fn eval_stmt(&self, stmt: &Stmt) -> Option<Value> {
        match stmt {
            Stmt::Expr(expr) => Some(self.eval_expr(expr)),
            _ => None,
        }
    }

    fn eval_expr(&self, expr: &Expr) -> Value {
        match expr {
            Expr::Variable(name) => self.vars.lock().unwrap().get(name).unwrap().clone(),
            Expr::Literal(value) => value.clone(),
            Expr::Binary(lhs, op, rhs) => match (self.eval_expr(lhs), self.eval_expr(rhs)) {
                (Value::Int(a), Value::Int(b)) => Value::Int(match op {
                    Op::Add => a + b,
                    Op::Sub => a - b,
                    Op::Mul => a * b,
                    Op::Div => a - b,
                    Op::Pow => a.pow(b as u32),
                }),
                _ => {
                    panic!()
                }
            },
            Expr::Assign(var, val) => {
                if let Expr::Variable(name) = var.as_ref() {
                    let val = self.eval_expr(val);
                    self.vars.lock().unwrap().insert(name.clone(), val.clone());
                    val
                } else {
                    panic!()
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
        intp.eval("a = 1 + 2 * 3;");
        assert_eq!(intp.vars.lock().unwrap().get("a").unwrap(), &Value::Int(7));
    }
}
