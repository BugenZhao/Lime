use lime::*;

#[macro_use]
mod common;

#[test]
fn test_assoc() {
    eval_file!("res/assoc.lm").unwrap();
}
