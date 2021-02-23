use lime::*;

#[macro_use]
mod common;

#[test]
fn test_ident_ignored() {
    eval!("var _ = 5; println(_);").unwrap_err();
}
