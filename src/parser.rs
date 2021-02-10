use std::fmt::Display;

use crate::error::{Error, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Pow,

    // Comparison
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

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Ident(pub String);

#[derive(Debug)]
pub enum Expr {
    Variable(Ident),
    Literal(Value),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Assign(Ident, Box<Expr>),
    Cast(Box<Expr>, Ident),
    Block(Vec<Stmt>),
    If(Box<Expr>, Box<Expr>, Box<Option<Expr>>),
    While(Box<Expr>, Box<Expr>, Box<Option<Expr>>),
}
#[derive(Debug)]
pub enum Stmt {
    VarDecl(Ident, Expr),
    Expr(Expr),
    Print(Expr),
    Assert(usize, usize, String, Expr),
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
        rule kw_assert() = "assert"
        rule kw_as() = "as"
        rule kw_true() = "true"
        rule kw_false() = "false"
        rule kw_or() = "or"
        rule kw_and() = "and"
        rule kw_if() = "if"
        rule kw_else() = "else"
        rule kw_while() = "while"
        rule kw_default() = "default"

        rule kw_int() = "Int"
        rule kw_float() = "Float"
        rule kw_bool() = "Bool"
        rule kw_string() = "String"
        rule kw_nil() = "Nil" / "nil"

        rule kw_NORMAL() = kw_var() / kw_print() / kw_assert() / kw_as() / kw_true() / kw_false() / kw_or() / kw_and()
                           kw_if() / kw_else() / kw_while() / kw_default()
        rule kw_TYPE() = kw_int() / kw_float() / kw_bool() / kw_string() / kw_nil()
        rule kw_ALL() = kw_TYPE() / kw_NORMAL()


        // Primary
        rule integer() -> Value
            = quiet!{ n:$(digit()+) { Value::Int(n.parse().unwrap()) } }
            / expected!("int")

        rule float() -> Value
            = quiet!{ n:$(digit()+ "." digit()+) { Value::Float(n.parse().unwrap()) } }
            / expected!("float")

        rule true_false() -> Value
            = kw_true()  { Value::Bool(true)  }
            / kw_false() { Value::Bool(false) }

        rule string() -> Value
            = quiet!{ s:$("\"" ("\\\"" / !"\"" [_])* "\"") { Value::String(snailquote::unescape(s).unwrap()) } }
            / expected!("string")

        rule literal() -> Expr
            = v:( float() // float first
                / integer()
                / true_false()
                / string()
            ) { Expr::Literal(v) }

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


        // Expr
        rule expr_unary() -> Expr = precedence!{
            "!" _ x:@ { Expr::Unary(UnaryOp::Not, box x) }
            "-" _ x:@ { Expr::Unary(UnaryOp::Neg, box x) }
            --
            p:primary() { p }
        }

        rule expr_cast() -> Expr // TODO: type
            = p:expr_unary() __ kw_as() __ i:$(kw_TYPE()) { Expr::Cast(box p, Ident(i.to_owned())) }

        rule expr_binary() -> Expr = precedence!{
            x:(@) _ kw_or() _ y:@ { Expr::Binary(box x, BinaryOp::Or, box y) }
            --
            x:(@) _ kw_and() _ y:@ { Expr::Binary(box x, BinaryOp::And, box y) }
            --
            x:(@) _ "==" _ y:@ { Expr::Binary(box x, BinaryOp::Eq, box y) }
            x:(@) _ "!=" _ y:@ { Expr::Binary(box x, BinaryOp::Ne, box y) }
            --
            x:(@) _ "<=" _ y:@ { Expr::Binary(box x, BinaryOp::Le, box y) }
            x:(@) _ ">=" _ y:@ { Expr::Binary(box x, BinaryOp::Ge, box y) }
            --
            x:(@) _ "<" _ y:@ { Expr::Binary(box x, BinaryOp::Lt, box y) }
            x:(@) _ ">" _ y:@ { Expr::Binary(box x, BinaryOp::Gt, box y) }
            --
            x:(@) _ "+" _ y:@ { Expr::Binary(box x, BinaryOp::Add, box y) }
            x:(@) _ "-" _ y:@ { Expr::Binary(box x, BinaryOp::Sub, box y) }
            --
            x:(@) _ "*" _ y:@ { Expr::Binary(box x, BinaryOp::Mul, box y) }
            x:(@) _ "/" _ y:@ { Expr::Binary(box x, BinaryOp::Div, box y) }
            --
            x:@ _ "^" _ y:(@) { Expr::Binary(box x, BinaryOp::Pow, box y) }
            --
            c:expr_cast() { c }
            --
            u:expr_unary() { u }
        }

        rule expr_assign() -> Expr
            = i:ident() _ "=" _ e:expr() { Expr::Assign(i, box e) }

        rule block() -> Expr
            = "{" _ ss:stmt()* _ "}" { Expr::Block(ss) }

        rule if_else() -> Expr
            = _ kw_else() _ else_:(expr_if() / block()) { else_ }
        rule expr_if() -> Expr
            = kw_if() __ cond:expr() _ then:block() else_:if_else()? { Expr::If(box cond, box then, box else_) }

        rule while_default() -> Expr
            = _ kw_default() _ default:(expr_NORMAL() / block()) { default }
        // TODO: default
        rule expr_while() -> Expr
            = kw_while() __ cond:expr() _ body:block() default:while_default()? { Expr::While(box cond, box body, box default) }

        rule expr_NORMAL() -> Expr
            = expr_assign()
            / expr_binary()

        rule expr_BLOCK() -> Expr
            = block()
            / expr_if()
            / expr_while()

        // TODO: check the order, must it be `call / block / normal / if / loop`?
        rule expr() -> Expr = expr_BLOCK() / expr_NORMAL()

        // Stmt
        rule stmt_var_decl() -> Stmt
            = kw_var() __ i:ident() _ "=" _ e:expr() _ semi()+ { Stmt::VarDecl(i, e) }

        rule stmt_expr() -> Stmt
            = e:expr_NORMAL() _ semi()+ { Stmt::Expr(e) }
            / e:expr_BLOCK() _ semi()* { Stmt::Expr(e) }

        rule stmt_print() -> Stmt
            = kw_print() __ e:expr() _ semi()+ { Stmt::Print(e) }

        rule stmt_assert() -> Stmt
            = kw_assert() __ start:position!() e:expr() end:position!() _ semi()+ { Stmt::Assert(start, end, "".to_owned(), e) }

        rule stmt() -> Stmt
            = stmt_var_decl()
            / stmt_expr()
            / stmt_print()
            / stmt_assert()

        rule raw_stmt() -> Stmt = _ s:stmt() _ { s }

        pub rule program() -> Vec<Stmt>
            = _ semi()* ss:(raw_stmt())* semi()* ![_] { ss }
    }
}

struct Preprocessor<'a> {
    text: &'a str,
}

impl<'a> Preprocessor<'a> {
    fn new(text: &'a str) -> Self {
        Self { text }
    }

    fn pp_stmts(&self, stmts: &mut Vec<Stmt>) {
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
        }
    }
}

#[inline]
pub fn parse(text: &str) -> Result<Vec<Stmt>> {
    let result: std::result::Result<Vec<Stmt>, _> = my_parser::program(text);
    let mut stmts = result.or_else(|err| Err(Error::ParseError(err)))?;
    Preprocessor::new(text).pp_stmts(&mut stmts);
    Ok(stmts)
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
        let r = parse(text);
        println!("{:#?}", r);
        assert!(r.is_ok())
    }
}
