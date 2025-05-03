use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

use anyhow::Context;
use clap::Parser;

use mcc::Options;

#[derive(Debug, Clone, Parser)]
struct Cli {
    filepath: PathBuf,

    #[clap(flatten)]
    options: Options,

    #[clap(short, long, default_value_t = Preprocessor::Gcc)]
    preprocessor: Preprocessor,

    #[clap(long)]
    only_preprocess: bool,
}

#[derive(Debug, Clone, Copy)]
enum Preprocessor {
    Mcc,
    Gcc,
}

impl std::fmt::Display for Preprocessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mcc => f.write_str("mcc"),
            Self::Gcc => f.write_str("gcc"),
        }
    }
}

impl FromStr for Preprocessor {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "mcc" => Ok(Self::Mcc),
            "gcc" => Ok(Self::Gcc),
            _ => Err(format!("'{s}' is not a valid preprocessor")),
        }
    }
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
    match cli.preprocessor {
        Preprocessor::Mcc => mcc::preprocessor(&cli.filepath, &preprocessed_file)?,
        Preprocessor::Gcc => gcc::preprocessor(&cli.filepath, &preprocessed_file)?,
    }

    if cli.only_preprocess {
        let content = std::fs::read_to_string(&preprocessed_file).unwrap();
        println!("{content}");
        return Ok(());
    }

    let assembly_file = tempdir.path().join(filename).with_extension("s");

    let result = mcc::compiler(
        &preprocessed_file,
        &assembly_file,
        cli.options,
        cli.filepath.clone(),
    );

    match result {
        Ok(_keep_going @ false) => Ok(()),
        Ok(_keep_going @ true) => {
            let executable_file = basedir.join(filename).with_extension("");
            gcc::produce_executable(&assembly_file, &executable_file)?;
            Ok(())
        }
        Err(maybe_error_message) => {
            if let Some(error) = maybe_error_message {
                eprintln!("{error}");
            }
            std::process::exit(1);
        }
    }
}

mod gcc {
    use super::*;

    pub fn preprocessor(input: &Path, output: &Path) -> Result<(), anyhow::Error> {
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

    pub fn produce_executable(input: &Path, output: &Path) -> Result<(), anyhow::Error> {
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
}
