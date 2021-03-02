use crate::{
    ast::{Expr, Ident, IdentExt, Stmt, StmtKind},
    err,
    error::Result,
    ErrType,
};
use std::{cell::RefCell, collections::HashSet, ops::DerefMut, rc::Rc};

pub struct Resolver {
    vars: RefCell<HashSet<String>>,
    enclosing: Option<Rc<Self>>,
}

impl Resolver {
    pub fn new_global() -> Rc<Self> {
        Rc::new(Self {
            vars: RefCell::new(HashSet::new()),
            enclosing: None,
        })
    }

    fn new(enclosing: Rc<Self>) -> Self {
        Self {
            vars: RefCell::new(HashSet::new()),
            enclosing: Some(enclosing),
        }
    }

    fn decl(&self, ident: &Ident) {
        if ident.is_ignored() {
            return;
        }
        self.vars.borrow_mut().insert(ident.0.clone());
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

impl Resolver {
    pub fn res_stmts(self: &Rc<Self>, stmts: &mut [Stmt]) -> Result<()> {
        for stmt in stmts.iter_mut() {
            self.res_stmt(stmt)?;
        }
        Ok(())
    }

    fn res_stmt(self: &Rc<Self>, stmt: &mut Stmt) -> Result<()> {
        match &mut stmt.tp {
            StmtKind::VarDecl(i, e) => {
                // this leaves functions that are recursive or calling each other UNresolved
                // they will be resolved in runtime instead
                self.res_expr(e)?;
                self.decl(i);
            }
            StmtKind::Expr(e) => self.res_expr(e)?,
            StmtKind::Print(e) => self.res_expr(e)?,
            StmtKind::Assert(e) => self.res_expr(e)?,
            StmtKind::Break(e) => {
                if let Some(e) = e {
                    self.res_expr(e)?
                }
            }
            StmtKind::Continue(e) => {
                if let Some(e) = e {
                    self.res_expr(e)?
                }
            }
            StmtKind::Return(e) => {
                if let Some(e) = e {
                    self.res_expr(e)?
                }
            }
            StmtKind::ClassDecl(i, _) => {
                if self.enclosing.is_some() {
                    return Err(err!(ErrType::OnlyTopLevel("class".to_owned())));
                }
                self.decl(i);
            }
            StmtKind::Impl(i, afs) => {
                if self.enclosing.is_some() {
                    return Err(err!(ErrType::OnlyTopLevel("impl".to_owned())));
                }
                self.decl(i);
                for f in afs.iter_mut() {
                    self.res_expr(&mut f.1)?;
                }
            }
        }
        Ok(())
    }

    fn res_expr(self: &Rc<Self>, expr: &mut Expr) -> Result<()> {
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
                fn_res.res_stmts(body.as_block_mut().unwrap())?; // DO NOT create new env
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
            Expr::IfVar(_, e, t, el) => {
                self.res_expr(e)?;
                self.res_expr(t)?;
                if let Some(el) = el.deref_mut() {
                    self.res_expr(el)?;
                }
            }
            Expr::VecLiteral(es) => {
                for e in es {
                    self.res_expr(e)?;
                }
            }
            Expr::For(_, e, b, d) => {
                self.res_expr(e)?;
                self.res_expr(b)?;
                if let Some(d) = d.deref_mut() {
                    self.res_expr(d)?;
                }
            }
            Expr::RangeLiteral(lo, hi, _) => {
                self.res_expr(lo)?;
                self.res_expr(hi)?;
            }
            Expr::WhileVar(_, c, b, d) => {
                self.res_expr(c)?;
                self.res_expr(b)?;
                if let Some(d) = d.deref_mut() {
                    self.res_expr(d)?;
                }
            }
        }
        Ok(())
    }
}
