use crate::diagnostic::{Diagnostic, Item};
use crate::typecheck::Typechecker;
use std::path::Path;
use std::{env, io};

mod builtins;
mod core;
mod diagnostic;
mod eval;
mod lex;
mod parse;
mod syntax;
mod typecheck;

#[derive(Debug)]
struct Config {
    filename: String,
    entrypoint: Option<String>,
}

#[derive(Debug)]
enum ConfigError {
    MissingFilename,
    MissingEntrypoint,
}

fn get_filename(args: &Vec<String>) -> Result<String, ConfigError> {
    if args.len() > 1 {
        return Ok(args[1].clone());
    } else {
        return Err(ConfigError::MissingFilename);
    }
}

fn get_entrypoint(args: &Vec<String>) -> Result<Option<String>, ConfigError> {
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

fn get_config(args: &Vec<String>) -> Result<Config, ConfigError> {
    let filename = get_filename(args)?;
    let entrypoint = get_entrypoint(args)?;
    Ok(Config {
        filename,
        entrypoint,
    })
}

fn parse_args() -> Result<Config, ConfigError> {
    let args = env::args().collect();
    get_config(&args)
}

fn report_config_error(err: ConfigError) {
    println!("{:?}", err)
}

#[derive(Debug)]
enum InterpreterError {
    ParseError(parse::ParseError),
    TypeError(typecheck::TypeError),
}

impl From<parse::ParseError> for InterpreterError {
    fn from(err: parse::ParseError) -> Self {
        InterpreterError::ParseError(err)
    }
}

impl From<typecheck::TypeError> for InterpreterError {
    fn from(err: typecheck::TypeError) -> Self {
        InterpreterError::TypeError(err)
    }
}

fn report_interpreter_error(config: &Config, err: InterpreterError) -> io::Result<()> {
    let mut diagnostic = Diagnostic::new();
    match err {
        InterpreterError::ParseError(err) => err.report(&mut diagnostic),
        InterpreterError::TypeError(err) => err.report(&mut diagnostic),
    }
    diagnostic.report_all(&Path::new(&config.filename))
}

fn run_interpreter(config: &Config) -> Result<(), InterpreterError> {
    let filename: &String = &config.filename;
    let main = String::from("main");
    let entrypoint: &String = match config.entrypoint {
        None => &main,
        Some(ref value) => value,
    };
    let module: syntax::Module = parse::parse_file(filename)?;
    let mut tc: Typechecker = Typechecker::new_with_builtins();
    let module: core::Module = tc.check_module(module)?;
    panic!("{:?} {:?} {:?}", filename, entrypoint, module)
}

fn main() -> io::Result<()> {
    match parse_args() {
        Err(err) => {
            report_config_error(err);
            std::process::exit(1)
        }
        Ok(config) => match run_interpreter(&config) {
            Ok(()) => Ok(()),
            Err(err) => {
                let () = report_interpreter_error(&config, err)?;
                std::process::exit(1)
            }
        },
    }
}
