use lime::*;

#[macro_use]
mod common;

#[test]
fn test_assoc_magic_equals() {
    eval_file!("res/assoc_magic_equals.lm").unwrap();
}

#[test]
fn test_assoc_magic_finalize() {
    eval_file!("res/assoc_magic_finalize.lm").unwrap();
}
