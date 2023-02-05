#![deny(unused_crate_dependencies)]

use std::{io, path::PathBuf, rc::Rc};

use clap::Parser;
use ipso_core::CommonKinds;
use ipso_diagnostic::{Diagnostic, Location, Message, Source};
use run::InterpreterError;

use crate::version::VERSION;

pub mod repl;
pub mod run;
pub mod version;

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
#[command(name = "ipso", disable_version_flag = true)]
struct Cli {
    /// The file to run. Starts a REPL if omitted.
    filename: Option<String>,

    /// Parse and type check the file without running.
    #[arg(long)]
    check: bool,

    /// Run a specific IO action from the file. Defaults to "main".
    #[clap(long = "run")]
    entrypoint: Option<String>,

    /// Print the current version.
    #[clap(long = "version")]
    version: bool,

    /// Arguments to pass to the ipso program.
    #[arg(trailing_var_arg = true)]
    args: Vec<String>,
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert()
}

/// The entrypoint to the CLI.
///
/// It's defined in this library to avoid false positives associated with the
/// [`#![deny(unused_crate_dependencies)]` attribute](https://doc.rust-lang.org/rustc/lints/listing/allowed-by-default.html#unused-crate-dependencies).
pub fn main() -> io::Result<()> {
    let cli = Cli::parse();

    if cli.version {
        println!("ipso {}", VERSION);
        return Ok(());
    }

    let args: Vec<Rc<str>> = cli.args.into_iter().map(Rc::from).collect();

    match cli.filename {
        Some(filename) => {
            /*
            I only wrote this function because I couldn't get the `?` operator to play will
            in the present of the top-level `io::Result<()>` value.
            */
            fn run_file_with_args(
                filename: &str,
                args: &[Rc<str>],
                entrypoint: Option<String>,
                check: bool,
            ) -> Result<(), InterpreterError> {
                let common_kinds = CommonKinds::default();

                let (modules, module_id, entrypoint) =
                    run::check(&common_kinds, filename, entrypoint)?;

                if !check {
                    run::run(
                        &common_kinds,
                        modules,
                        module_id,
                        filename,
                        args,
                        &entrypoint,
                        run::IO::default(),
                    )
                } else {
                    println!("{}: success", filename);
                    Ok(())
                }
            }

            match run_file_with_args(&filename, &args, cli.entrypoint, cli.check) {
                Ok(()) => Ok(()),
                Err(err) => {
                    report_interpreter_error(filename, err)?;
                    std::process::exit(1)
                }
            }
        }
        None => {
            crate::repl::run(
                std::env::args()
                    .take(1)
                    .map(Rc::from)
                    .collect::<Vec<_>>()
                    .pop()
                    .unwrap_or_else(|| Rc::from("ipso")),
                &args,
            )?;
            Ok(())
        }
    }
}
