use std::path::{Path, PathBuf};

use anyhow::Context;
use clap::Args;
use lexer::LexerError;
use thiserror::Error;

mod assembly;
mod ast;
mod lexer;
mod tacky;

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

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error(transparent)]
    Lexer(#[from] LexerError),
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub fn compiler(input: &Path, output: &Path, options: Options) -> Result<bool, CompilerError> {
    let content = std::fs::read_to_string(input).context("failed to read input file")?;

    let tokens = lexer::run(content)?;
    if options.lex {
        println!("{tokens:?}");
        return Ok(false);
    }

    let program = ast::parse(tokens)?;
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
