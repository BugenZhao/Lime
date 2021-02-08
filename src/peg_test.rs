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
}

peg::parser! {
    grammar my_parser() for str {
        rule _() = quiet!{ [' ' | '\t' | '\r' | '\n']* }

        rule integer() -> Expr
            = quiet!{ n:$(['0'..='9']+) { Expr::Literal(Value::Int(n.parse().unwrap())) } }
            / expected!("integer")

        rule number() -> Expr
            = integer()

        rule ident() -> Expr
            = quiet!{ i:$(['a'..='z' | 'A'..='Z'] ['a'..='z' | 'A'..='Z' | '0'..='9']*) { Expr::Variable(i.into()) } }
            / expected!("identifier")

        rule primary() -> Expr
            = ident()
            / number()
            / "(" _ e:expr() _ ")" { e }

        rule arithmetic() -> Expr = precedence!{
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

        rule expr() -> Expr
            = i:ident() _ "=" _ e:expr() { Expr::Assign(box i, box e) }
            / a:arithmetic() { a }

        rule stmt() -> Stmt
            = _ e:expr() _ ";" _ { Stmt::Expr(e) }

        pub rule stmts() -> Vec<Stmt>
            = _ ss:(stmt())* _ ![_] { ss }
    }
}

#[inline]
pub fn parse(text: &str) -> Vec<Stmt> {
    my_parser::stmts(text).unwrap()
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test() {
        let text = r#"
        b = 1 + (2+3) * 5 ^ 2 ^ 2 + 6 * a; 
        c = 6; 
        "#;
        let r: Result<Vec<Stmt>, _> = my_parser::stmts(text);
        println!("{:#?}", r);
        assert!(r.is_ok())
    }
}
