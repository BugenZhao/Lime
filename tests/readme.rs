use colored::*;
use lime::*;

peg::parser! {
    grammar readme_parser() for str {
        rule _() = (!"```" [_])
        rule lang() = "swift"
        rule code_block() -> &'input str
            = "```" lang() "\n" cb:$(_+) "```" { cb }
        pub rule code_blocks() -> Vec<&'input str>
            = _* cbs:(code_block() ** (_*)) _* ![_] { cbs }
    }
}

#[macro_use]
mod common;

#[test]
fn test_readme() {
    let readme = include_str!("../README.md");
    let code_blocks: Vec<&str> = readme_parser::code_blocks(readme).unwrap();
    code_blocks.into_iter().enumerate().for_each(|(i, t)| {
        println!("\n\n{}\n\n{}", format!(">>> {}:", i).green().bold(), t);
        eval!(t).unwrap();
    });
}
