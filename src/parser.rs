use crate::{ast::*, error::Result, resolver::Resolver, value::*};
use lazy_static::lazy_static;
use std::collections::HashSet;

peg::parser! {
    grammar lime_parser() for str {
        // Lexical
        rule ws() = [' ' | '\t' | '\r' | '\n']
        rule comment() = "//" (!"\n" [_])* / "/*" (!"*/" [_])* "*/"
        rule _() = quiet!{ (ws() / comment())* }
        rule __() = quiet!{ (ws() / comment())+ }

        rule semi() = (";" _)
        rule digit() = ['0'..='9']
        rule alpha() = ['a'..='z' | 'A'..='Z' | '_']
        rule aldig() = alpha() / digit()
        rule non_aldig() = !aldig() [_]


        // Keywords
        rule kw_var() = "var"
        rule kw_print() = "_print"
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
        rule kw_break() = "break"
        rule kw_continue() = "continue"
        rule kw_return() = "return"
        rule kw_class() = "class"
        rule kw_impl() = "impl"
        rule kw_assoc() = "assoc"
        rule kw_nil() = "nil"
        rule kw_for() = "for"
        rule kw_in() = "in"

        rule kw_int() = "Int"
        rule kw_float() = "Float"
        rule kw_bool() = "Bool"
        rule kw_string() = "String"
        rule kw_object() = "Object"
        rule kw_big_nil() = "Nil"
        rule kw_big_class() = "Class"
        rule kw_big_func() = "Func"

        rule kw_NORMAL() = kw_var() / kw_print() / kw_assert() / kw_as() / kw_true() / kw_false() / kw_or() / kw_and()
                         / kw_if() / kw_else() / kw_while() / kw_default() / kw_break() / kw_continue() / kw_return()
                         / kw_class() / kw_assoc() / kw_nil() / kw_for() / kw_in()
        rule kw_TYPE()   = kw_int() / kw_float() / kw_bool() / kw_string() / kw_object() / kw_big_nil() / kw_big_class()
                         / kw_big_func()
        pub rule kw_ALL() = kw_NORMAL() / kw_TYPE()


        // For hinter
        rule token() -> (usize, &'input str) = pos:position!() token:$(aldig()+) { (pos, token) }
        pub rule tokens() -> Vec<(usize, &'input str)>
            = _ non_aldig()* ts:(token() ** (non_aldig()+)) non_aldig()* _ { ts }


        // Primary
        rule integer() -> Value
            = n:quiet!{ $(digit()+) } {?
                if let Ok(int) = n.parse() { Ok(Value::Int(int)) }
                else { Err("reasonable int") }
            }
            / expected!("int")

        rule float() -> Value
            = n:quiet!{ $(digit()+ "." digit()+) } {?
                if let Ok(float) = n.parse() { Ok(Value::Float(float)) }
                else { Err("reasonable float") }
            }
            / expected!("float")

        rule true_false() -> Value
            = kw_true()  { Value::Bool(true)  }
            / kw_false() { Value::Bool(false) }

        rule nil() -> Value = kw_nil() { Value::Nil(None) }

        rule string() -> Value
            = quiet!{ s:$("\"" ("\\\"" / !"\"" [_])* "\"") { Value::String(snailquote::unescape(s).unwrap()) } }
            / expected!("string")

        rule literal() -> Expr
            = v:( float() // float first
                / integer()
                / true_false()
                / nil()
                / string()
            ) { Expr::Literal(v) }

        rule raw_ident() -> &'input str
            = $(alpha() aldig()* "?"*)
        rule ident() -> Ident // TODO: more elegant
            = quiet!{ i:$(!(kw_ALL() !aldig()) raw_ident()) { Ident(i.to_owned(), None) } }
            / expected!("name identifier")
        rule ident_type() -> Ident
            = quiet!{ i:$(!(kw_NORMAL() !aldig()) raw_ident()) { Ident(i.to_owned(), None) } }
            / expected!("type identifier")

        rule comma_expr_list() -> Vec<Expr>
            = exprs:(expr() ** (_ "," _)) (_ "," _)? { exprs }
        rule vec_literal() -> Expr
            = "[" _ es:comma_expr_list() _ "]" { Expr::VecLiteral(es) }

        rule primary() -> Expr
            = i:ident_type() { Expr::Variable(i) }
            / literal()
            / vec_literal()
            / "(" _ e:expr() _ ")" { e }


        // Expr
        rule arg_list() -> Vec<Expr>
            = args:(expr() ** (_ "," _)) {?
                if args.len() <= N_MAX_ARGS { Ok(args) } else { Err("fewer arguments") }
            }
        rule expr_call() -> Expr = precedence!{
            o:(@) _ "." _ f:ident() _ "=" _ v:expr() { Expr::Set(box o, f, box v) }
            --
            f:(@) _ "(" _ args:arg_list() _ ")" { Expr::Call(box f, args) }
            o:(@) _ "." _ f:ident() { Expr::Get(box o, f) }
            --
            p:primary() { p }
        }

        rule expr_range() -> Expr
            = lo:expr_call() _ "..=" _ hi:expr_call() { Expr::RangeLiteral(box lo, box hi, true) }
            / lo:expr_call() _ ".."  _ hi:expr_call() { Expr::RangeLiteral(box lo, box hi, false) }
            / expr_call()

        rule expr_unary() -> Expr = precedence!{
            "!" _ x:@ { Expr::Unary(UnaryOp::Not, box x) }
            "-" _ x:@ { Expr::Unary(UnaryOp::Neg, box x) }
            --
            r:expr_range() { r }
        }

        rule expr_cast() -> Expr // TODO: type
            = p:expr_unary() __ kw_as() __ i:$(kw_TYPE()) { Expr::Cast(box p, Ident(i.to_owned(), None)) }

        rule expr_binary() -> Expr = precedence!{
            x:(@) _ kw_or() _ y:@ { Expr::Binary(box x, BinaryOp::Or, box y) }
            --
            x:(@) _ kw_and() _ y:@ { Expr::Binary(box x, BinaryOp::And, box y) }
            --
            x:(@) _ "&==" _ y:@ { Expr::Binary(box x, BinaryOp::Feq, box y) }
            x:(@) _ "&!=" _ y:@ { Expr::Binary(box x, BinaryOp::Fne, box y) }
            x:(@) _ "===" _ y:@ { Expr::Binary(box x, BinaryOp::Req, box y) }
            x:(@) _ "!==" _ y:@ { Expr::Binary(box x, BinaryOp::Rne, box y) }
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
            = "{" _ semi()* ss:(raw_stmt())* e:raw_stmt_expr_optional_semi()? semi()* _ "}" {
                Expr::make_block(ss, e)
            }

        rule if_else() -> Expr
            = _ kw_else() _ else_:(expr_if() / expr_if_var() / block()) { else_ }
        rule expr_if() -> Expr
            = kw_if() __ cond:expr() _ then:block() else_:if_else()? { Expr::If(box cond, box then, box else_) }
        rule expr_if_var() -> Expr
            = kw_if() __ kw_var() __ i:ident() _ "=" _ e:expr() _ then:block() else_:if_else()? {
                Expr::IfVar(i, box e, box then, box else_)
            }
        rule expr_cond() -> Expr
            = expr_if() / expr_if_var()

        rule loop_default() -> Expr
            = _ kw_default() _ default:expr() { default }
        rule expr_while() -> Expr
            = kw_while() __ cond:expr() _ body:block() default:loop_default()? { Expr::While(box cond, box body, box default) }
        rule expr_while_var() -> Expr
            = kw_while() __ kw_var() __ i:ident() _ "=" _ e:expr() _ body:block() default:loop_default()? {
                Expr::WhileVar(i, box e, box body, box default)
            }
        rule expr_for() -> Expr
            = kw_for() __ ident:ident() __ kw_in() __ expr:expr() _ body:block() default:loop_default()? {
                Expr::For(ident, box expr, box body, box default)
            }
        rule expr_loop() -> Expr
            = expr_while() / expr_while_var() / expr_for()

        rule param_list() -> Vec<Ident>
            = params:(ident() ** (_ "," _)) (_ "," _)? {?
                if params.len() > N_MAX_ARGS {
                    Err("fewer parameters")
                } else if params.iter().collect::<HashSet<_>>().len() < params.len() {
                    Err("unique identifiers")
                } else {
                    Ok(params)
                }
            }
        rule expr_func() -> Expr
            = "|" _ p:param_list() _ "|" _ body:block() { Expr::Func(p, box body) }

        rule colon_kv() -> (Ident, Expr)
            = k:ident() _ ":" _ v:expr() { (k, v) }
        rule construct_list() -> Vec<(Ident, Expr)>
            = kvs:(colon_kv() ** (_ "," _)) (_ "," _)? {?
                if kvs.iter().map(|p| &p.0).collect::<HashSet<_>>().len() < kvs.len() {
                    Err("unique identifiers")
                } else {
                    Ok(kvs)
                }
            }
        rule expr_object_init() -> Expr
            = class:ident() _ "{" _ kvs:construct_list() _ "}" { Expr::Construct(class, kvs) }

        rule expr_NORMAL() -> Expr
            = expr_assign()
            / expr_object_init()
            / expr_binary()
            / expr_func()

        rule expr_BLOCK() -> Expr  // optional semicolon
            = block()
            / expr_cond()
            / expr_loop()

        rule expr() -> Expr = expr_BLOCK() / expr_NORMAL()

        // Stmt
        rule stmt_var_decl() -> Stmt
            = kw_var() __ i:ident() _ "=" _ e:expr() _ semi()+ { Stmt::VarDecl(i, e) }

        rule field_list() -> Vec<Ident>
            = fields:(ident() ** (_ "," _)) (_ "," _)? {?
                if fields.iter().collect::<HashSet<_>>().len() < fields.len() {
                    Err("unique identifiers")
                } else {
                    Ok(fields)
                }
            }
        rule stmt_class_decl() -> Stmt
            = kw_class() __ i:ident() _ "{" _ f:field_list() _ "}" _ semi()* { Stmt::ClassDecl(i, f) }

        rule assoc() -> (Ident, Expr)
            = kw_assoc() __ i:ident() _ "=" _ e:expr() _ semi()+ { (i, e) }
        rule stmt_impl() -> Stmt
            = kw_impl() __ i:ident_type() _ "{" _ semi()* ms:assoc()* semi()* _ "}" _ semi()* { Stmt::Impl(i, ms) }

        rule stmt_expr() -> Stmt
            = e:expr_NORMAL() _ semi()+ { Stmt::Expr(e) }
            / e:expr_BLOCK() _ semi()* { Stmt::Expr(e) }
        rule stmt_expr_optional_semi() -> Stmt
            = e:(expr_NORMAL() / expr_BLOCK()) _ semi()* { Stmt::Expr(e) }

        rule stmt_print() -> Stmt
            = kw_print() __ e:expr() _ semi()+ { Stmt::Print(e) }
            / kw_print() _ "(" _ e:expr() _ ")" _ semi()+ { Stmt::Print(e) }

        rule stmt_assert() -> Stmt
            = kw_assert() __ start:position!() e:expr() end:position!() _ semi()+ { Stmt::Assert(start, end, "".to_owned(), e) }

        rule bcr_val() -> Expr
            = __ e:expr() { e }
        rule stmt_break() -> Stmt
            = kw_break() e:bcr_val()? _ semi()+ { Stmt::Break(e) }
        rule stmt_continue() -> Stmt
            = kw_continue() e:bcr_val()? _ semi()+ { Stmt::Continue(e) }
        rule stmt_return() -> Stmt
            = kw_return() e:bcr_val()? _ semi()+ { Stmt::Return(e) }

        rule stmt() -> Stmt
            = stmt_var_decl()
            / stmt_class_decl()
            / stmt_impl()
            / stmt_print()
            / stmt_assert()
            / stmt_break()
            / stmt_continue()
            / stmt_return()
            / stmt_expr()

        rule raw_stmt() -> WrStmt
            = _ start:position!() s:stmt() end:position!() _ { WrStmt {
                tp: s,
                span: (start, end),
                text: None,
            } }
        rule raw_stmt_expr_optional_semi() -> WrStmt
            = _ start:position!() s:stmt_expr_optional_semi() end:position!() _ { WrStmt {
                tp: s,
                span: (start, end),
                text: None,
            } }

        pub rule program() -> Vec<WrStmt>
            = _ semi()* ss:(raw_stmt())* semi()* ![_] { ss }
    }
}

lazy_static! {
    pub static ref KEYWORDS: Vec<String> = lime_parser::kw_ALL("")
        .unwrap_err()
        .expected
        .tokens()
        .map(|s| s.to_owned().replace("\"", ""))
        .collect();
}

pub fn tokens(text: &str) -> Vec<(usize, &str)> {
    lime_parser::tokens(text).unwrap()
}

pub fn parse_and_resolve(text: &str) -> Result<Vec<WrStmt>> {
    let result: std::result::Result<Vec<_>, _> = lime_parser::program(text);
    let mut stmts = result?;
    Resolver::new_global(text).res_stmts(&mut stmts)?;
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
        _print c + 3;
        "#;
        let r = parse_and_resolve(text);
        println!("{:#?}", r);
        assert!(r.is_ok())
    }

    #[test]
    fn test_call() {
        let text = "-fn_gen(true).first(add(1, 2)).second as String;";
        let stmts = parse_and_resolve(text).unwrap();

        assert_eq!(
            stmts.first().unwrap().tp,
            Stmt::Expr(Expr::Cast(
                box Expr::Unary(
                    UnaryOp::Neg,
                    box Expr::Get(
                        box Expr::Call(
                            box Expr::Get(
                                box Expr::Call(
                                    box Expr::Variable("fn_gen".into()),
                                    vec![Expr::Literal(Value::Bool(true))],
                                ),
                                "first".into()
                            ),
                            vec![Expr::Call(
                                box Expr::Variable("add".into()),
                                vec![Expr::Literal(Value::Int(1)), Expr::Literal(Value::Int(2))]
                            )],
                        ),
                        "second".into()
                    )
                ),
                "String".into(),
            ))
        );
    }

    #[test]
    fn test_inline_closure_call() {
        let text = "|x|{x+1;}(3);";
        parse_and_resolve(text).unwrap_err();

        let text = "(|x|{x+1;})(3);";
        parse_and_resolve(text).unwrap();
    }

    #[test]
    fn test_tokens() {
        use rand::prelude::IteratorRandom;
        use rand::thread_rng;

        let mut rng = thread_rng();
        (0..100000)
            .map(|_| {
                (0..8)
                    .map(|_| ('\x00'..='\x7f').choose(&mut rng).unwrap())
                    .collect::<String>()
            })
            .for_each(|s| {
                let _ = tokens(&s);
            });
    }

    #[test]
    fn test_overflow() {
        parse_and_resolve("1234567890987654321234567890987654321234567890;").unwrap_err();
    }
}
