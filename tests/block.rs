use lime::*;

#[macro_use]
mod common;

#[test]
fn test_block() -> Result<()> {
    eval_file!("tests/res/block.lm")?;
    Ok(())
}
