use lime::*;

#[macro_use]
mod common;

#[test]
fn test_object() {
    eval_file!("res/object.lm").unwrap();
}
