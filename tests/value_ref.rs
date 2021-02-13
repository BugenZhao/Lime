use lime::*;

#[macro_use]
mod common;

#[test]
fn test_by_ref() {
    eval_file!("res/value_ref.lm").unwrap();
}
