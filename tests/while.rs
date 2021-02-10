use lime::*;

#[macro_use]
mod common;

#[test]
fn test_while() {
    eval_file!("tests/res/while.lm").unwrap();
}

#[test]
fn test_nil_1() {
    eval!("while false {}").unwrap();
}

#[test]
fn test_nil_2() {
    let r = eval!("var a = while false {};").unwrap_err();
    assert!(matches!(r, Error::CannotHaveValue(..)))
}

#[test]
fn test_nil_3() {
    let r = eval!("var a = 5; a = while false {};").unwrap_err();
    assert!(matches!(r, Error::CannotHaveValue(..)))
}

#[test]
fn test_default() {
    eval!(
        "
    var a = 0;
    var res = while (a = a - 1) >= 0 {
        print \"looped\";
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
        print \"looped\";
        {}
    } default 42;
    "
    )
    .unwrap_err();
    assert!(matches!(r, Error::CannotHaveValue(..)))
}
