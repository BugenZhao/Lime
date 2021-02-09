use std::fmt::Display;

use crate::error::{Error, Result};
use peg::str::LineCol;

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Ident(pub String);

#[derive(Debug)]
pub enum Expr {
    Variable(Ident),
    Literal(Value),
    Binary(Box<Expr>, Op, Box<Expr>),
    Assign(Ident, Box<Expr>),
    Cast(Box<Expr>, Ident),
}
#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    VarDecl(Ident, Box<Expr>),
    Print(Box<Expr>),
}

peg::parser! {
    grammar my_parser() for str {
        // Lexical
        rule ws() = [' ' | '\t' | '\r' | '\n']
        rule comment() = "//" (!"\n" [_])* / "/*" (!"*/" [_])* "*/"
        rule _() = quiet!{ (ws() / comment())* }
        rule __() = quiet!{ (ws() / comment())+ }

        rule semi() = (";" _)
        rule digit() = ['0'..='9']
        rule alpha() = ['a'..='z' | 'A'..='Z' | '_']


        // Keywords
        rule kw_var() = "var"
        rule kw_print() = "print"
        rule kw_as() = "as"
        rule kw_true() = "true"
        rule kw_false() = "false"

        rule kw_int() = "Int"
        rule kw_float() = "Float"
        rule kw_bool() = "Bool"
        rule kw_string() = "String"

        rule kw_NORMAL() = kw_var() / kw_print() / kw_as() / kw_true() / kw_false()
        rule kw_TYPE() = kw_int() / kw_float() / kw_bool() / kw_string()
        rule kw_ALL() = kw_TYPE() / kw_NORMAL()


        // Primary
        rule integer() -> Value
            = quiet!{ n:$(digit()+) { Value::Int(n.parse().unwrap()) } }
            / expected!("integer")

        rule float() -> Value
            = quiet!{ n:$(digit()+ "." digit()+) { Value::Float(n.parse().unwrap()) } }
            / expected!("float")

        rule true_false() -> Value
            = kw_true()  { Value::Bool(true)  }
            / kw_false() { Value::Bool(false) }

        rule string() -> Value
            = s:$("\"" ("\\\"" / !"\"" [_])* "\"") { Value::String(snailquote::unescape(s).unwrap()) }

        rule literal() -> Expr
            = v:(integer() / float() / true_false() / string()) { Expr::Literal(v) }

        rule ident() -> Ident
            = quiet!{ i:$(!kw_ALL() (alpha() (alpha() / digit())*)) { Ident(i.to_owned()) } }
            / expected!("name identifier")

        rule ident_type() -> Ident
            = quiet!{ i:$(!kw_NORMAL() (alpha() (alpha() / digit())*)) { Ident(i.to_owned()) } }
            / expected!("type identifier")

        rule primary() -> Expr
            = i:ident() { Expr::Variable(i) }
            / literal()
            / "(" _ e:expr() _ ")" { e }

        rule cast() -> Expr
            = p:primary() __ kw_as() __ i:ident_type() { Expr::Cast(box p, i) }


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
            c:cast() { c }
            --
            p:primary() { p }
        }

        rule expr_assign() -> Expr
            = i:ident() _ "=" _ e:expr() { Expr::Assign(i, box e) }

        rule expr() -> Expr
            = expr_assign()
            / expr_binary()


        // Stmt
        rule stmt_var_decl() -> Stmt
            = kw_var() __ i:ident() _ "=" _ e:expr() _ semi()+ { Stmt::VarDecl(i, box e) }

        rule stmt_expr() -> Stmt
            = e:expr() _ semi()+ { Stmt::Expr(e) }

        rule stmt_print() -> Stmt
            = kw_print() __ e:expr() _ semi()+ { Stmt::Print(box e) }

        rule stmt() -> Stmt
            = stmt_var_decl()
            / stmt_expr()
            / stmt_print()

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
        print c + 3;
        "#;
        let r = my_parser::stmts(text);
        println!("{:#?}", r);
        assert!(r.is_ok())
    }
}
