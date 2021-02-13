use lime::*;

#[macro_use]
mod common;

#[test]
fn test_logical() {
    eval_file!("res/logical.lm").unwrap();
}
