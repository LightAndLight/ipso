use clap::Parser;
use ipso_cli::{
    run::{run_interpreter, InterpreterError},
    version::VERSION,
};
use ipso_diagnostic::{Diagnostic, Location, Message, Source};
use std::{io, path::PathBuf, rc::Rc};

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

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    if cli.version {
        println!("ipso {}", VERSION);
        return Ok(());
    }

    let args: Vec<Rc<str>> = cli.args.into_iter().map(Rc::from).collect();

    match cli.filename {
        Some(filename) => {
            let config = ipso_cli::run::Config {
                filename: filename.clone(),
                entrypoint: cli.entrypoint,
                args,
                stdin: None,
                stdout: None,
            };

            match run_interpreter(config) {
                Ok(()) => Ok(()),
                Err(err) => {
                    report_interpreter_error(filename, err)?;
                    std::process::exit(1)
                }
            }
        }
        None => {
            ipso_cli::repl::run(
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
