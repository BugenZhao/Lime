use crate::Value;
use enum_as_inner::EnumAsInner;

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

impl From<&str> for Ident {
    fn from(s: &str) -> Self {
        Self(s.to_owned(), None)
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Self(s, None)
    }
}

pub trait IdentExt {
    fn can_hold_nil(&self) -> bool;
    fn is_ignored(&self) -> bool;
}

impl IdentExt for str {
    fn can_hold_nil(&self) -> bool {
        self.ends_with('?')
    }

    fn is_ignored(&self) -> bool {
        self == "_"
    }
}

impl IdentExt for Ident {
    fn can_hold_nil(&self) -> bool {
        self.0.ends_with('?')
    }

    fn is_ignored(&self) -> bool {
        self.0 == "_"
    }
}

#[derive(Debug, Clone, PartialEq, EnumAsInner)]
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
    WhileVar(Ident, Box<Expr>, Box<Expr>, Box<Option<Expr>>),
    Call(Box<Expr>, Vec<Expr>),
    Func(Vec<Ident>, Box<Expr>), // must be Expr::Block
    Construct(Ident, Vec<(Ident, Expr)>),
    Get(Box<Expr>, Ident),
    Set(Box<Expr>, Ident, Box<Expr>),
    VecLiteral(Vec<Expr>),
    RangeLiteral(Box<Expr>, Box<Expr>, bool),
    For(Ident, Box<Expr>, Box<Expr>, Box<Option<Expr>>),
}

impl Expr {
    pub fn make_block(mut stmts: Vec<Stmt>, last: Option<Stmt>) -> Expr {
        if let Some(e) = last {
            stmts.push(e);
        }
        Self::Block(stmts)
    }
}

#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum StmtKind {
    VarDecl(Ident, Expr),
    ClassDecl(Ident, Vec<Ident>),
    Impl(Ident, Vec<(Ident, Expr)>),
    Expr(Expr),
    Print(Expr),
    Assert(Expr),
    Break(Option<Expr>),
    Continue(Option<Expr>),
    Return(Option<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub tp: StmtKind,
    pub span: Span,
    pub text: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub source_id: usize,
    pub pos: (usize, usize),
}
