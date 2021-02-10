use lime::*;

#[macro_use]
mod common;

#[test]
fn test_while() -> Result<()> {
    eval_file!("tests/res/while.lm")?;
    Ok(())
}
