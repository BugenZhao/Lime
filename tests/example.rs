use peg_playground::*;

#[macro_use]
mod common;

#[test]
fn test_example() -> Result<()> {
    eval_file!("tests/res/example")?;
    Ok(())
}
