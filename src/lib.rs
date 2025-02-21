use std::path::Path;

use anyhow::Context;
use clap::Args;
use lexer::Span;
use thiserror::Error;

mod assembly;
mod ast;
mod lexer;
mod tacky;

use self::ast::ParseError;
use self::lexer::{CharElem, LexerError, TokenElem};

#[derive(Debug, Clone, Args)]
#[group(required = false, multiple = false)]
pub struct Options {
    #[arg(long)]
    pub lex: bool,

    #[arg(long)]
    pub parse: bool,

    #[arg(long)]
    pub tacky: bool,

    #[arg(long)]
    pub codegen: bool,
}

#[derive(Debug, Clone)]
pub struct LineView {
    pub before: Vec<(usize, String)>,
    pub after: Vec<(usize, String)>,
}

impl LineView {
    fn from_line(line: usize, lines: &[&str]) -> LineView {
        let lower_line_number = usize::saturating_sub(line, 2);
        let correct_line_number = usize::saturating_add(line, 1);
        let upper_line_number = std::cmp::min(usize::saturating_add(line, 2), lines.len() - 1);

        let before: Vec<(usize, String)> = (lower_line_number..correct_line_number)
            .map(|i| (i, lines[i].to_owned()))
            .collect();

        let after: Vec<(usize, String)> = (correct_line_number..=upper_line_number)
            .map(|i| (i, lines[i].to_owned()))
            .collect();

        LineView { before, after }
    }
}

#[derive(Debug, Error)]
pub enum CompilerError {
    Lexer(LexerError, LineView),
    WrongToken {
        message: String,
        span: Span,
        view: LineView,
    },
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lexer(error, view) => {
                for (line_number, line) in &view.before {
                    writeln!(f, "{} {}", console::style(line_number).blue(), line)?;
                }

                writeln!(
                    f,
                    "| {fill}{marker} {message}",
                    fill = " ".repeat(error.char_elem.position),
                    marker = console::style("^").red(),
                    message = console::style(error.message).red(),
                )?;

                for (line_number, line) in &view.after {
                    writeln!(f, "{} {}", console::style(line_number).blue(), line)?;
                }

                Ok(())
            }
            Self::WrongToken {
                message,
                span,
                view,
            } => {
                for (line_number, line) in &view.before {
                    writeln!(f, "{} {}", console::style(line_number).blue(), line)?;
                }

                writeln!(
                    f,
                    "| {fill}{marker} {message}",
                    fill = " ".repeat(span.start_position),
                    marker =
                        console::style("^".repeat(span.end_position - span.start_position + 1))
                            .red(),
                    message = console::style(message).red(),
                )?;

                for (line_number, line) in &view.after {
                    writeln!(f, "{} {}", console::style(line_number).blue(), line)?;
                }

                Ok(())
            }
            Self::Other(e) => {
                writeln!(f, "{}", console::style(e).red())
            }
        }
    }
}

pub fn compiler(input: &Path, output: &Path, options: Options) -> Result<bool, CompilerError> {
    let content = std::fs::read_to_string(input).context("failed to read input file")?;
    let lines: Vec<&str> = content.lines().collect();

    let tokens = lexer::run(&lines[..]).map_err(|source| {
        let view = LineView::from_line(source.char_elem.line, &lines[..]);
        CompilerError::Lexer(source, view)
    })?;
    if options.lex {
        println!("{tokens:?}");
        return Ok(false);
    }

    let program = ast::parse(tokens).map_err(|source| match source {
        ParseError::WrongToken { message, span } => {
            let view = LineView::from_line(span.line, &lines[..]);
            CompilerError::WrongToken {
                message,
                span,
                view,
            }
        }
        ParseError::Other(e) => CompilerError::Other(e),
        ParseError::EarlyEnd(e) => {
            CompilerError::Other(anyhow::anyhow!("unexpected early end: {e}"))
        }
    })?;
    if options.parse {
        println!("{program:#?}");
        return Ok(false);
    }

    let tacky = program.emit_tacky();
    if options.tacky {
        println!("{tacky:#?}");
        return Ok(false);
    }

    let assembly = tacky.assembly();
    if options.codegen {
        println!("{assembly:#?}");
        return Ok(false);
    }

    let buf = format!("{assembly}");
    std::fs::write(output, &buf[..]).context("failed to write to output file")?;

    Ok(true)
}
