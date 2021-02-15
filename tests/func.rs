use lime::*;

#[macro_use]
mod common;

#[test]
fn test_func() {
    eval_file!("res/func.lm").unwrap();
}

#[test]
fn test_bad_arity() {
    let r = eval!("(|a, b| { a + b; })(3);").unwrap_err();
    assert!(matches!(r.tp, Error::WrongArguments { .. }));
}

#[test]
fn test_bad_return_1() {
    let r = eval!("return 10;").unwrap_err();
    assert!(matches!(r.tp, Error::Return(Value::Int(10))));
}

#[test]
fn test_bad_return_2() {
    let r = eval!("return;").unwrap_err();
    assert!(matches!(r.tp, Error::Return(Value::Nil)));
}

#[test]
fn test_static_resolving() {
    let text = r#"
    var a = "global";
    {
        var show_global = || { println(a); a; };
        assert show_global() == "global";

        var a = "block";
        var show_block = || { println(a); a; };
        assert show_global() == "global";
        assert  show_block() == "block";
    }
    "#;
    let stmts = lime::parse_and_resolve(text).unwrap();
    println!("{:#?}", stmts);
    Interpreter::new().eval_stmts(&stmts).unwrap();
}
