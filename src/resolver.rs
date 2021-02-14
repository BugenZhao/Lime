use crate::{
    error::Result,
    parser::{Expr, Ident, Stmt},
};
use std::{cell::RefCell, collections::HashSet, ops::DerefMut, rc::Rc};

pub struct Resolver<'a> {
    text: &'a str,
    vars: RefCell<HashSet<&'a str>>,
    enclosing: Option<Rc<Self>>,
}

impl<'a> Resolver<'a> {
    pub fn new_global(text: &'a str) -> Rc<Self> {
        Rc::new(Self {
            text,
            vars: RefCell::new(HashSet::new()),
            enclosing: None,
        })
    }

    fn new(enclosing: Rc<Self>) -> Self {
        Self {
            text: enclosing.text,
            vars: RefCell::new(HashSet::new()),
            enclosing: Some(enclosing),
        }
    }

    fn decl(&self, ident: &'a Ident) {
        self.vars.borrow_mut().insert(&ident.0);
    }

    fn get_step(&self, id_name: &str) -> Option<usize> {
        let mut curr = self;
        let mut step = 0;

        loop {
            if curr.vars.borrow().contains(id_name) {
                return Some(step);
            } else if let Some(next) = curr.enclosing.as_deref() {
                curr = next;
                step += 1;
            } else {
                return None;
            }
        }
    }
}

impl<'a> Resolver<'a> {
    pub fn res_stmts(self: &Rc<Self>, stmts: &'a mut [Stmt]) -> Result<()> {
        for stmt in stmts.iter_mut() {
            self.res_stmt(stmt)?;
        }
        Ok(())
    }

    fn res_stmt(self: &Rc<Self>, stmt: &'a mut Stmt) -> Result<()> {
        match stmt {
            Stmt::VarDecl(i, e) => {
                // this leaves functions that are recursive or calling each other UNresolved
                // they will be resolved in runtime instead
                self.res_expr(e)?;
                self.decl(i);
            }
            Stmt::Expr(e) => self.res_expr(e)?,
            Stmt::Print(e) => self.res_expr(e)?,
            Stmt::Assert(start, end, text, e) => {
                *text = self.text.chars().skip(*start).take(*end - *start).collect();
                self.res_expr(e)?;
            }
            Stmt::Break(e) => {
                if let Some(e) = e {
                    self.res_expr(e)?
                }
            }
            Stmt::Continue(e) => {
                if let Some(e) = e {
                    self.res_expr(e)?
                }
            }
            Stmt::Return(e) => {
                if let Some(e) = e {
                    self.res_expr(e)?
                }
            }
            Stmt::ClassDecl(i, _) => {
                self.decl(i);
            }
        }
        Ok(())
    }

    fn res_expr(self: &Rc<Self>, expr: &'a mut Expr) -> Result<()> {
        match expr {
            Expr::Variable(Ident(id_name, step)) => {
                *step = self.get_step(id_name);
            }
            Expr::Literal(_) => {}
            Expr::Binary(l, _, r) => {
                self.res_expr(l)?;
                self.res_expr(r)?;
            }
            Expr::Unary(_, e) => self.res_expr(e)?,
            Expr::Assign(Ident(id_name, step), e) => {
                self.res_expr(e)?;
                *step = self.get_step(id_name);
            }
            Expr::Cast(e, _i) => {
                // FIXME: _i resolution after class support added
                self.res_expr(e)?;
            }
            Expr::Block(ss) => {
                let new_res = Rc::new(Self::new(Rc::clone(&self)));
                new_res.res_stmts(ss)?;
            }
            Expr::If(c, t, e) => {
                self.res_expr(c)?;
                self.res_expr(t)?;
                if let Some(e) = e.deref_mut() {
                    self.res_expr(e)?;
                }
            }
            Expr::While(c, b, d) => {
                self.res_expr(c)?;
                self.res_expr(b)?;
                if let Some(d) = d.deref_mut() {
                    self.res_expr(d)?;
                }
            }
            Expr::Call(c, args) => {
                self.res_expr(c)?;
                for a in args.iter_mut() {
                    self.res_expr(a)?;
                }
            }
            Expr::Func(params, body) => {
                let fn_res = Rc::new(Self::new(Rc::clone(&self)));
                for param in params.iter() {
                    fn_res.decl(param);
                }
                fn_res.res_stmts(body)?;
            }
            Expr::Construct(_, kvs) => {
                for (_, e) in kvs.iter_mut() {
                    self.res_expr(e)?;
                }
            }
            Expr::Get(e, _) => {
                self.res_expr(e)?;
            }
            Expr::Set(o, _, v) => {
                self.res_expr(o)?;
                self.res_expr(v)?;
            }
        }
        Ok(())
    }
}
