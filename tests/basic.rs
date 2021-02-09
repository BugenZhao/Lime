use peg_playground::*;

#[macro_use]
mod common;

#[test]
fn test_example() -> Result<()> {
    eval!("tests/res/example")?;
    Ok(())
}

#[test]
fn test_logical() -> Result<()> {
    eval!("tests/res/logical")?;
    Ok(())
}
