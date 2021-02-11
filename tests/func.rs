use lime::*;

#[macro_use]
mod common;

#[test]
fn test_func() {
    eval_file!("tests/res/func.lm").unwrap();
}

#[test]
fn test_bad_arity() {
    let r = eval!("(|a, b| { a + b; })(3);").unwrap_err();
    assert!(matches!(r, Error::WrongArguments { .. }));
}

#[test]
fn test_bad_return_1() {
    let r = eval!("return 10;").unwrap_err();
    assert!(matches!(r, Error::Return(Value::Int(10))));
}

#[test]
fn test_bad_return_2() {
    let r = eval!("return;").unwrap_err();
    assert!(matches!(r, Error::Return(Value::Nil)));
}

#[test]
fn test_static_resolving() {
    let text = r#"
    var a = "global";
    {
        var show_a = || { print(a); a; };
        assert show_a() == "global";
        var a = "block";
        assert show_a() == "global";
    }
    "#;
    eval!(text).unwrap();
}
