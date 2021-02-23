use lime::*;

#[macro_use]
mod common;

#[test]
fn test_vec() {
    eval_file!("res/vec.lm").unwrap();
}

#[test]
fn test_ref_cell_borrow_in_vec_finalize() {
    eval!("println([1, [2, 3]]);").unwrap();
}
