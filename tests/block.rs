use lime::*;

#[macro_use]
mod common;

#[test]
fn test_block() {
    eval_file!("tests/res/block.lm").unwrap();
}

#[test]
fn test_empty_block_1() {
    eval!("{}").unwrap();
}

#[test]
fn test_empty_block_2() {
    let r = eval!("var a = {};").unwrap_err();
    assert!(matches!(r, Error::CannotHaveValue(_, Value::Nil)));
}
