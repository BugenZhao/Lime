use peg_playground::*;

#[test]
fn test_example() -> Result<()> {
    Interpreter::new().eval_file("tests/res/example")?;
    Ok(())
}
