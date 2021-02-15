use lime::*;

#[macro_use]
mod common;

#[test]
fn test_assign() {
    eval!("var a = 10; a = 5;").unwrap();
}

#[test]
fn test_assign_before_decl() {
    let r = eval!("a = 5;").unwrap_err();
    assert!(matches!(r.tp, Error::CannotFindValue(..)));
}
