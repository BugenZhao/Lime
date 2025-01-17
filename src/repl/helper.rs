use crate::parser;
use colored::Colorize;
use itertools::Itertools;
use rustyline::completion::Completer;
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::hint::Hinter;
use rustyline::validate::{self, MatchingBracketValidator, ValidationResult, Validator};
use rustyline::{CompletionType, Config, Context, Editor};
use rustyline_derive::Helper;
use std::borrow::Cow::{self, Owned};

#[derive(Helper)]
pub struct LimeHelper {
    highlighter: MatchingBracketHighlighter,
    validator: MatchingBracketValidator,
    pub hints: Vec<String>,
}

impl LimeHelper {
    pub fn new() -> Self {
        Self {
            highlighter: MatchingBracketHighlighter::new(),
            validator: MatchingBracketValidator::new(),
            hints: Vec::new(),
        }
    }
}

impl Completer for LimeHelper {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        if pos < line.len() {
            return Ok((0, vec![]));
        }

        let hints = &self.hints;
        let tokens = parser::tokens(line);
        let start = tokens.last().map(|p| p.0).unwrap_or(0);
        let cands = hints
            .iter()
            .map(|s| s.as_str())
            .chain(tokens.iter().rev().skip(1).map(|&p| p.1))
            .filter(|x| x.starts_with(&line[start..pos]))
            .map(|s| s.to_string())
            .unique()
            .sorted()
            .collect();

        Ok((start, cands))
    }
}

impl Hinter for LimeHelper {
    type Hint = String;

    fn hint(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Option<String> {
        if pos < line.len() || line.is_empty() {
            return None;
        }

        let (start, cands) = self.complete(line, pos, _ctx).unwrap();
        cands.first().map(|s| s[(pos - start)..].to_owned())
    }
}

impl Highlighter for LimeHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        let _ = default;
        Owned(prompt.bright_green().bold().to_string())
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned(hint.dimmed().to_string())
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }

    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str,
        completion: CompletionType,
    ) -> Cow<'c, str> {
        let _ = completion;
        Owned(candidate.underline().to_string())
    }
}

impl Validator for LimeHelper {
    fn validate(
        &self,
        ctx: &mut validate::ValidationContext,
    ) -> rustyline::Result<ValidationResult> {
        let mut r = self.validator.validate(ctx).unwrap();
        match r {
            ValidationResult::Valid(_) => {
                let mut text = ctx.input().to_owned();
                if text.starts_with(':') {
                    return Ok(r);
                }

                text.push(';');
                if let Err(e) = parser::parse(&text) {
                    return Ok(ValidationResult::Invalid(Some(format!(
                        "\n{}",
                        e.to_string()
                    ))));
                }
            }
            ValidationResult::Incomplete => {}
            ValidationResult::Invalid(Some(ref mut message)) => {
                message.insert(0, '\n');
            }
            _ => {}
        };

        Ok(r)
    }

    fn validate_while_typing(&self) -> bool {
        false
    }
}

pub fn editor() -> Editor<LimeHelper> {
    let config = Config::builder()
        .auto_add_history(true)
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .build();
    let h = LimeHelper::new();
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(h));

    rl
}
