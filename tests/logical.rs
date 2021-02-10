use lime::*;

#[macro_use]
mod common;

#[test]
fn test_logical() {
    eval_file!("tests/res/logical.lm").unwrap();
}
