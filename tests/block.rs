use peg_playground::*;

#[macro_use]
mod common;

#[test]
fn test_block() -> Result<()> {
    eval_file!("tests/res/block")?;
    Ok(())
}
