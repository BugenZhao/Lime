use lime::*;

#[macro_use]
mod common;

#[test]
fn test_if() {
    eval_file!("tests/res/if.lm").unwrap();
}

#[test]
fn test_not_bool() {
    let r = eval!("if \"not a bool\" { print \"oh no\"; }").unwrap_err();
    assert!(matches!(r, Error::CannotBeCondition(..)));
}

#[test]
fn test_bad_syntax_1() {
    let r = eval!("if true print 233;").unwrap_err();
    assert!(matches!(r, Error::ParseError(..)));
}

#[test]
fn test_bad_syntax_2() {
    let r = eval!("if true { print 233; } else print 233;").unwrap_err();
    assert!(matches!(r, Error::ParseError(..)));
}

#[test]
fn test_bad_syntax_3() {
    let r = eval!("if false {} else {} else {}").unwrap_err();
    assert!(matches!(r, Error::ParseError(..)));
}

#[test]
fn test_bad_syntax_4() {
    let r = eval!("if true {} else if {}").unwrap_err();
    assert!(matches!(r, Error::ParseError(..)));
}

#[test]
fn test_nil_1() {
    let r = eval!("var a = if true and true {};").unwrap_err();
    assert!(matches!(r, Error::CannotHaveValue(..)));
}

#[test]
fn test_nil_2() {
    let r = eval!("var a = if false { 3; };").unwrap_err();
    assert!(matches!(r, Error::CannotHaveValue(..)));
}
