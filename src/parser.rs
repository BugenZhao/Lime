use crate::error::{Error, Result};
use peg::str::LineCol;

#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Int(i64),
}

#[derive(Debug)]
pub enum Expr {
    Variable(String),
    Literal(Value),
    Binary(Box<Expr>, Op, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
}
#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    VarDecl(Box<Expr>, Box<Expr>),
}

peg::parser! {
    grammar my_parser() for str {
        // Lexical
        rule ws() = [' ' | '\t' | '\r' | '\n']
        rule comment() = "//" (!"\n" [_])* / "/*" (!"*/" [_])* "*/"
        rule _() = quiet!{ (ws() / comment())* }

        rule semi() = (";" _)
        rule digit() = ['0'..='9']
        rule alpha() = ['a'..='z' | 'A'..='Z' | '_']


        // Keywords
        rule kw_var() = "var"
        rule kw() = kw_var()


        // Primary
        rule integer() -> Expr
            = quiet!{ n:$(digit()+) { Expr::Literal(Value::Int(n.parse().unwrap())) } }
            / expected!("integer")

        rule number() -> Expr = integer()

        rule ident() -> Expr
            = quiet!{ i:$(!kw() (alpha() (alpha() / digit())*)) { Expr::Variable(i.into()) } }
            / expected!("identifier")

        rule primary() -> Expr
            = ident()
            / number()
            / "(" _ e:expr() _ ")" { e }


        // Expr
        rule expr_binary() -> Expr = precedence!{
            x:(@) _ "+" _ y:@ { Expr::Binary(box x, Op::Add, box y) }
            x:(@) _ "-" _ y:@ { Expr::Binary(box x, Op::Sub, box y) }
            --
            x:(@) _ "*" _ y:@ { Expr::Binary(box x, Op::Mul, box y) }
            x:(@) _ "/" _ y:@ { Expr::Binary(box x, Op::Div, box y) }
            --
            x:@ _ "^" _ y:(@) { Expr::Binary(box x, Op::Pow, box y) }
            --
            p:primary() { p }
        }

        rule expr_assign() -> Expr
            = i:ident() _ "=" _ e:expr() { Expr::Assign(box i, box e) }

        rule expr() -> Expr
            = expr_assign()
            / expr_binary()


        // Stmt
        rule stmt_expr() -> Stmt
            = e:expr() _ semi()+ { Stmt::Expr(e) }

        rule stmt_var_decl() -> Stmt
            = kw_var() _ i:ident() _ "=" _ e:expr() _ semi()+ { Stmt::VarDecl(box i, box e) }

        rule stmt() -> Stmt
            = stmt_var_decl()
            / stmt_expr()

        rule raw_stmt() -> Stmt = _ s:stmt() _ { s }

        pub rule stmts() -> Vec<Stmt>
            = ss:(raw_stmt())* semi()? ![_] { ss }
    }
}

#[inline]
pub fn parse(text: &str) -> Result<Vec<Stmt>> {
    let result: std::result::Result<Vec<Stmt>, peg::error::ParseError<LineCol>> =
        my_parser::stmts(text);
    result.or_else(|err| Err(Error::ParseError(err)))
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test() {
        let text = r#"
            b = 1 + (2+3) 
                * 5 ^ 2 ^ 2 + 6 * a; 
        ;
        var /* comment /* here */ c = 6;;  ; ; // or here ; /*
        "#;
        let r = my_parser::stmts(text);
        println!("{:#?}", r);
        assert!(r.is_ok())
    }
}
