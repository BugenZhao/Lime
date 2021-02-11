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
