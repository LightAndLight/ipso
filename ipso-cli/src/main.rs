use clap::{AppSettings, Parser};
use ipso_cli::{
    run::{run_interpreter, InterpreterError},
    version::VERSION,
};
use ipso_diagnostic::{Diagnostic, Location, Message, Source};
use std::{io, path::PathBuf};

fn report_interpreter_error(filename: String, err: InterpreterError) -> io::Result<()> {
    let mut diagnostic = Diagnostic::new();
    match err {
        InterpreterError::ParseError(err) => err.report(&mut diagnostic),
        InterpreterError::TypeError(err) => err.report(&mut diagnostic),
        InterpreterError::ImportError(err) => err.report(&mut diagnostic),
        InterpreterError::MissingEntrypoint(name) => diagnostic.item(
            Some(Location {
                source: Source::File {
                    path: PathBuf::from(filename),
                },
                offset: None,
            }),
            Message {
                content: format!("missing entrypoint {:?}", name),
                addendum: None,
            },
        ),
        InterpreterError::FileDoesNotExist(path) => diagnostic.item(
            None,
            Message {
                content: format!("file {} does not exist", path.to_str().unwrap()),
                addendum: None,
            },
        ),
    }
    diagnostic.report_all()
}

#[derive(Parser)]
#[clap(name = "ipso", global_setting(AppSettings::NoAutoVersion))]
struct Cli {
    /// The file to run. Starts a REPL if omitted.
    filename: Option<String>,

    /// Run a specific IO action from the file. Defaults to "main".
    #[clap(long = "run")]
    entrypoint: Option<String>,

    /// Print the current version.
    #[clap(long = "version")]
    version: bool,
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    if cli.version {
        println!("ipso {}", VERSION);
        return Ok(());
    }

    match cli.filename {
        Some(filename) => {
            let config = ipso_cli::run::Config {
                filename: filename.clone(),
                entrypoint: cli.entrypoint,
                stdin: None,
                stdout: None,
            };

            match run_interpreter(config) {
                Ok(()) => Ok(()),
                Err(err) => {
                    let () = report_interpreter_error(filename, err)?;
                    std::process::exit(1)
                }
            }
        }
        None => {
            ipso_cli::repl::run()?;
            Ok(())
        }
    }
}
