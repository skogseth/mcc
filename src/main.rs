use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::Context;
use clap::{Args, Parser};

#[derive(Debug, Clone, Parser)]
struct Cli {
    filepath: PathBuf,

    #[clap(flatten)]
    options: Options,
}

#[derive(Debug, Clone, Args)]
#[group(required = false, multiple = false)]
struct Options {
    #[arg(long)]
    lex: bool,

    #[arg(long)]
    parse: bool,

    #[arg(long)]
    codegen: bool,
}

fn main() -> Result<(), anyhow::Error> {
    let cli = Cli::parse();

    let filename = cli.filepath.file_name().with_context(|| {
        format!(
            "received filepath with no filename: {}",
            cli.filepath.display()
        )
    })?;

    let basedir = cli.filepath.parent().with_context(|| {
        format!(
            "received filepath with 'parent path': {}",
            cli.filepath.display()
        )
    })?;

    let tempdir = tempfile::tempdir().context("failed to create tempdir")?;

    let preprocessed_file = tempdir.path().join(filename).with_extension("i");
    preprocessor(&cli.filepath, &preprocessed_file)?;

    let assembly_file = tempdir.path().join(filename).with_extension("s");
    compiler(&preprocessed_file, &assembly_file)?;

    let executable_file = basedir.join(filename).with_extension("");
    produce_executable(&assembly_file, &executable_file)?;

    Ok(())
}

fn preprocessor(input: &Path, output: &Path) -> Result<(), anyhow::Error> {
    let output = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(input)
        .arg("-o")
        .arg(output)
        .output()
        .context("failed to run `gcc` for preprocessing")?;

    if !output.status.success() {
        return Err(anyhow::anyhow!(
            "Process failed:\n --- stdout ---\n{}\n\n --- stderr --- \n{}\n\n",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        ));
    }

    Ok(())
}

fn compiler(_input: &Path, output: &Path) -> Result<(), anyhow::Error> {
    let mut file = File::create(output).context("failed to open file")?;

    #[rustfmt::skip]
    let content = String::from(
r#"
        	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0	sdk_version 15, 0
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
## %bb.0:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$2, %eax
	popq	%rbp
	retq
                                        ## -- End function
.subsections_via_symbols
"#
    );

    file.write_all(content.as_bytes())
        .context("failed to write assembly file")?;

    Ok(())
}

fn produce_executable(input: &Path, output: &Path) -> Result<(), anyhow::Error> {
    let output = Command::new("gcc")
        .arg(input)
        .arg("-o")
        .arg(output)
        .output()
        .unwrap();

    if !output.status.success() {
        return Err(anyhow::anyhow!(
            "Process failed:\n --- stdout ---\n{}\n\n --- stderr --- \n{}\n\n",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        ));
    }

    Ok(())
}
