use std::path::Path;

use anyhow::Context;
use clap::Args;

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

pub fn compiler(input: &Path, output: &Path, options: Options) -> Result<bool, anyhow::Error> {
    let content = std::fs::read_to_string(input).context("failed to read input file")?;

    let tokens = lexer::run(&content)?;
    if options.lex {
        return Ok(false);
    }

    let program = ast::parse(tokens)?;
    if options.parse {
        return Ok(false);
    }

    // println!("AST:\n{program:#?}\n");

    let tacky = program.emit_tacky();
    if options.tacky {
        return Ok(false);
    }

    // println!("Tacky:\n{tacky:#?}\n");

    let assembly = tacky.assembly();
    if options.codegen {
        return Ok(false);
    }

    // println!("Assembly (structured):\n{assembly:#?}\n");

    let buf = format!("{assembly}");
    std::fs::write(output, &buf[..])?;

    Ok(true)
}
