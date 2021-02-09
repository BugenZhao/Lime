use peg_playground::*;

#[macro_use]
mod common;

#[test]
fn test_logical() -> Result<()> {
    eval_file!("tests/res/logical")?;
    Ok(())
}
