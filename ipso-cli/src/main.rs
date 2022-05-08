mod run;

use ipso_diagnostic::{Diagnostic, Location, Message, Source};
use run::{run_interpreter, Config, InterpreterError};
use std::{env, io, path::PathBuf};

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

#[derive(Debug)]
enum ConfigError {
    MissingFilename,
    MissingEntrypoint,
}

fn get_filename(args: &[String]) -> Result<String, ConfigError> {
    if args.len() > 1 {
        Ok(args[1].clone())
    } else {
        Err(ConfigError::MissingFilename)
    }
}

fn get_entrypoint(args: &[String]) -> Result<Option<String>, ConfigError> {
    match args
        .iter()
        .enumerate()
        .find_map(|(ix, val)| if val == "--run" { Some(ix) } else { None })
    {
        None => Ok(None),
        Some(option_ix) => match args.get(option_ix + 1) {
            None => Err(ConfigError::MissingEntrypoint),
            Some(entrypoint) => Ok(Some(entrypoint.clone())),
        },
    }
}

fn get_config(args: &[String]) -> Result<Config, ConfigError> {
    let filename = get_filename(args)?;
    let entrypoint = get_entrypoint(args)?;
    Ok(Config {
        filename,
        entrypoint,
        stdin: None,
        stdout: None,
    })
}

fn parse_args() -> Result<Config, ConfigError> {
    let args: Vec<String> = env::args().collect();
    get_config(&args)
}

fn report_config_error(err: ConfigError) {
    println!("{:?}", err)
}

fn main() -> io::Result<()> {
    match parse_args() {
        Err(err) => {
            report_config_error(err);
            std::process::exit(1)
        }
        Ok(config) => {
            let filename = config.filename.clone();
            match run_interpreter(config) {
                Ok(()) => Ok(()),
                Err(err) => {
                    let () = report_interpreter_error(filename, err)?;
                    std::process::exit(1)
                }
            }
        }
    }
}
