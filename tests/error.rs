use lime::*;

#[macro_use]
mod common;

#[test]
fn test_error() {
    eval_file!("res/error.lm").unwrap();
}
