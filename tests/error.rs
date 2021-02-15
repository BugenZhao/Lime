use lime::*;

#[macro_use]
mod common;

#[test]
fn test_error() {
    eval_file!("res/error.lm").unwrap();
}

#[test]
fn test_top_level_error() {
    let text = "
    var bad_worker = || { nil; };
    bad_worker().expect(\"bad bad\");
    ";
    let r = eval!(text).unwrap_err();
    println!("{}", r);
    assert!(matches!(r.tp, ErrType::ErrorReturn(..)))
}

#[test]
fn test_bad_var_for_nil() {
    let text = "
    var bad_worker = || { nil; };
    var ans = bad_worker();
    ";
    let r = eval!(text).unwrap_err();
    println!("{}", r);
    assert!(matches!(r.tp, ErrType::CannotHaveValue(..)))
}

#[test]
fn test_good_var_for_nil() {
    let text = "
    var bad_worker = || { nil; };
    var ans? = bad_worker();
    assert ans?.is_nil();
    ";
    eval!(text).unwrap();
}
