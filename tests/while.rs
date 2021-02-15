use lime::*;

#[macro_use]
mod common;

#[test]
fn test_while() {
    eval_file!("res/while.lm").unwrap();
}

#[test]
fn test_nil_1() {
    eval!("while false {}").unwrap();
}

#[test]
fn test_nil_2() {
    let r = eval!("var a = while false {};").unwrap_err();
    assert!(matches!(r.tp, ErrType::CannotHaveValue(_, Value::Nil(..))));
}

#[test]
fn test_nil_3() {
    let r = eval!("var a = 5; a = while false {};").unwrap_err();
    assert!(matches!(r.tp, ErrType::CannotHaveValue(_, Value::Nil(..))));
}

#[test]
fn test_default() {
    eval!(
        "
    var a = 0;
    var res = while (a = a - 1) >= 0 {
        _print \"looped\";
        42;
    } default 18;
    assert res == 18;
    "
    )
    .unwrap();
}

#[test]
fn test_default_looped_but_nil() {
    let r = eval!(
        "
    var a = 1;
    var res = while (a = a - 1) >= 0 {
        _print \"looped\";
        {}
    } default 42;
    "
    )
    .unwrap_err();
    assert!(matches!(r.tp, ErrType::CannotHaveValue(_, Value::Nil(..))));
}

#[test]
fn test_bad_break() {
    let r = eval!(
        "
    var a = if true {
        break 3;
    } else {
        5;
    };
        "
    )
    .unwrap_err();
    println!("{}", r);
    assert!(matches!(r.tp, ErrType::Break(..)));
}

#[test]
fn test_bad_continue() {
    let r = eval!(
        "
    var a = if true {
        continue 3;
    } else {
        5;
    };
        "
    )
    .unwrap_err();
    assert!(matches!(r.tp, ErrType::Continue(..)));
}

#[test]
fn test_break_without_value() {
    let r = eval!(
        "
    var a = while true {
        break;
    };
        "
    )
    .unwrap_err();
    assert!(matches!(r.tp, ErrType::CannotHaveValue(_, Value::Nil(..))));
}

#[test]
fn test_continue_without_value() {
    let r = eval!(
        "
    var flag = true;
    var a = while flag {
        flag = false;
        continue;
    };
        "
    )
    .unwrap_err();
    assert!(matches!(r.tp, ErrType::CannotHaveValue(_, Value::Nil(..))));
}
