use crate::parser::Stmt;

pub struct Preprocessor<'a> {
    text: &'a str,
}

impl<'a> Preprocessor<'a> {
    pub fn new(text: &'a str) -> Self {
        Self { text }
    }

    pub fn pp_stmts(&self, stmts: &mut Vec<Stmt>) {
        stmts.iter_mut().for_each(|s| self.pp_stmt(s))
    }

    fn pp_stmt(&self, stmt: &mut Stmt) {
        match stmt {
            Stmt::VarDecl(_, _) => {}
            Stmt::Expr(_) => {}
            Stmt::Print(_) => {}
            Stmt::Assert(s, e, text, _) => {
                *text = self.text.chars().skip(*s).take(*e - *s).collect();
            }
            Stmt::Break(_) => {}
            Stmt::Continue(_) => {}
            Stmt::Return(_) => {}
        }
    }
}
