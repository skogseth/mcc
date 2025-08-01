use std::path::{Path, PathBuf};

use anyhow::{Context, anyhow};
use clap::Args;
use thiserror::Error;

mod assembly;
mod identifier;
mod lexer;
mod parser;
mod tacky;
mod validate;

use crate::identifier::Identifier;
use crate::lexer::{CharElem, LexerError};
use crate::parser::ParseError;

#[derive(Debug, Clone, Args)]
#[group(required = false, multiple = false)]
pub struct Options {
    #[arg(long)]
    pub lex: bool,

    #[arg(long)]
    pub parse: bool,

    #[arg(long)]
    pub validate: bool,

    #[arg(long)]
    pub tacky: bool,

    #[arg(long)]
    pub codegen: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub line: usize,
    pub start_position: usize,
    pub end_position: usize,
}

impl Span {
    fn single(c: CharElem) -> Self {
        Self {
            line: c.line,
            start_position: c.position,
            end_position: c.position,
        }
    }

    fn wide(start: CharElem, end: CharElem) -> Self {
        assert_eq!(start.line, end.line);
        Self {
            line: start.line,
            start_position: start.position,
            end_position: end.position,
        }
    }

    #[cfg(test)]
    fn dummy() -> Self {
        Self {
            line: 0,
            start_position: 0,
            end_position: 0,
        }
    }
}

#[derive(Debug)]
pub struct Output<'lines> {
    filepath: PathBuf,
    lines: &'lines [&'lines str],
}

impl Output<'static> {
    #[cfg(test)]
    pub(crate) fn dummy() -> Self {
        Self {
            filepath: PathBuf::from("this/is/not/a/path"),
            lines: &["this is not a line"],
        }
    }
}

impl Output<'_> {
    pub fn warning(&self, span: Span, message: String) {
        let span_error = SpanError {
            message,
            span,
            lines: self.lines,
            style: console::Style::new().yellow(),
        };
        eprintln!(
            "{level}: {path}\n{error}",
            level = console::style("warning").yellow(),
            path = self.filepath.display(),
            error = span_error,
        );
    }

    pub fn error(&self, span: Span, message: String) {
        let span_error = SpanError {
            message,
            span,
            lines: self.lines,
            style: console::Style::new().red(),
        };
        eprintln!(
            "{level}: {path}\n{error}",
            level = console::style("error").red(),
            path = self.filepath.display(),
            error = span_error,
        );
    }
}

#[derive(Debug, Error)]
pub struct SpanError<'lines> {
    message: String,
    span: Span,
    lines: &'lines [&'lines str],
    style: console::Style,
}

impl std::fmt::Display for SpanError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lower_line_number = usize::saturating_sub(self.span.line, 2);
        let correct_line_number = usize::saturating_add(self.span.line, 1);
        let upper_line_number = std::cmp::min(
            usize::saturating_add(self.span.line, 2),
            self.lines.len() - 1,
        );

        let before = (lower_line_number..correct_line_number).map(|i| (i, self.lines[i]));
        for (line_number, line) in before {
            writeln!(f, "{} {}", console::style(line_number).blue(), line)?;
        }

        let marker_length = self.span.end_position - self.span.start_position + 1;
        writeln!(
            f,
            "{line} {fill}{marker} {message}",
            line = console::style("|").blue(),
            fill = " ".repeat(self.span.start_position),
            marker = self.style.apply_to("^".repeat(marker_length)),
            message = self.style.apply_to(&self.message),
        )?;

        let after = (correct_line_number..=upper_line_number).map(|i| (i, self.lines[i]));
        for (line_number, line) in after {
            writeln!(f, "{} {}", console::style(line_number).blue(), line)?;
        }

        Ok(())
    }
}

pub fn compiler(
    input_path: &Path,
    output_path: &Path,
    options: Options,
    filepath: PathBuf,
) -> Result<bool, Option<anyhow::Error>> {
    let content = std::fs::read_to_string(input_path).context("failed to read input file")?;
    let lines: Vec<&str> = content.lines().collect();

    let output = Output {
        lines: &lines[..],
        filepath,
    };

    let tokens: Vec<_> = lexer::make_lexer(&lines[..])
        .collect::<Result<_, LexerError>>()
        .map_err(|source| {
            let span = Span {
                line: source.char_elem.line,
                start_position: source.char_elem.position,
                end_position: source.char_elem.position,
            };
            output.error(span, source.message.to_owned());
            anyhow!("failed to lex file")
        })?;
    if options.lex {
        println!("{tokens:#?}");
        return Ok(false);
    }

    let mut program = parser::parse(tokens, &output).map_err(|source| match source {
        ParseError::BadTokens => None,
        ParseError::EarlyEnd(e) => Some(anyhow!("unexpected early end: {e}")),
    })?;
    if options.parse {
        println!("parsing succeeded, program:\n{program:#?}");
        return Ok(false);
    }

    validate::resolve_variables(&mut program, &output).map_err(|_| None)?;
    validate::resolve_loops(&mut program, &output).map_err(|_| None)?;
    if options.validate {
        println!("validation succeeded, new program:\n{program:#?}");
        return Ok(false);
    }

    let tacky = program.emit_tacky();
    if options.tacky {
        println!("{tacky}");
        return Ok(false);
    }

    let assembly = tacky.assembly();
    if options.codegen {
        println!("{assembly}");
        return Ok(false);
    }

    let buf = format!("{assembly}");
    std::fs::write(output_path, &buf[..]).context("failed to write to output file")?;

    Ok(true)
}

pub fn preprocessor(input: &Path, output: &Path) -> Result<(), anyhow::Error> {
    let input_content =
        std::fs::read_to_string(input).context("failed to read input file for preprocessor")?;
    let output_content = mcc_preprocessor::main(&input_content);
    std::fs::write(output, output_content.as_bytes())
        .context("failed to write preprocessor output to file")?;
    Ok(())
}
