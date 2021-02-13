use lime::*;

#[macro_use]
mod common;

#[test]
fn test_by_ref() {
    eval_file!("res/by_ref.lm").unwrap();
}
