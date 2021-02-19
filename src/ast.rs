use crate::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Pow,

    // Comparison
    Feq, // force comparison without type checker
    Fne,
    Req, // reference comparison
    Rne,
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,

    // Logical
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Ident(pub String, pub Option<usize>);

impl Ident {
    pub fn can_hold_nil(&self) -> bool {
        self.0.ends_with('?')
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Variable(Ident),
    Literal(Value),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Assign(Ident, Box<Expr>),
    Cast(Box<Expr>, Ident),
    Block(Vec<Stmt>),
    If(Box<Expr>, Box<Expr>, Box<Option<Expr>>),
    IfVar(Ident, Box<Expr>, Box<Expr>, Box<Option<Expr>>),
    While(Box<Expr>, Box<Expr>, Box<Option<Expr>>),
    Call(Box<Expr>, Vec<Expr>),
    Func(Vec<Ident>, Vec<Stmt>),
    Construct(Ident, Vec<(Ident, Expr)>),
    Get(Box<Expr>, Ident),
    Set(Box<Expr>, Ident, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(Ident, Expr),
    ClassDecl(Ident, Vec<Ident>),
    Impl(Ident, Vec<(Ident, Expr)>),
    Expr(Expr),
    Print(Expr),
    Assert(usize, usize, String, Expr),
    Break(Option<Expr>),
    Continue(Option<Expr>),
    Return(Option<Expr>),
}
