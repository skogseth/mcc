use std::path::Path;

use anyhow::Context;
use clap::Args;

mod assembly;
mod ast;
mod lexer;

#[derive(Debug, Clone, Args)]
#[group(required = false, multiple = false)]
pub struct Options {
    #[arg(long)]
    pub lex: bool,

    #[arg(long)]
    pub parse: bool,

    #[arg(long)]
    pub codegen: bool,
}

pub fn compiler(input: &Path, _output: &Path, options: Options) -> Result<bool, anyhow::Error> {
    let content = std::fs::read_to_string(input).context("failed to read input file")?;

    let tokens = lexer::run(&content)?;
    println!("{tokens:#?}");

    if options.lex {
        return Ok(false);
    }

    let program = ast::parse(tokens)?;
    println!("{program:#?}");

    if options.parse {
        return Ok(false);
    }

    let assembly = program.assembly();
    println!("{assembly:#?}");

    if options.codegen {
        return Ok(false);
    }

    todo!("finish compiler!");

    Ok(true)
}
