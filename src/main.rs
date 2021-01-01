use eval::Interpreter;
use typed_arena::Arena;

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
mod rope;
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
    MissingEntrypoint(String),
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
        InterpreterError::MissingEntrypoint(name) => diagnostic.item(diagnostic::Item {
            pos: 0,
            message: format!("Missing entrypoint {:?}", name),
            addendum: None,
        }),
    }
    diagnostic.report_all(&Path::new(&config.filename))
}

fn find_entrypoint_body(
    entrypoint: &String,
    module: &core::Module,
) -> Result<(core::Expr, core::TypeSig), InterpreterError> {
    match module.decls.iter().find_map(|decl| match decl {
        core::Declaration::Definition {
            name, sig, body, ..
        } if name == entrypoint => Some((body.clone(), sig.clone())),
        _ => None,
    }) {
        None => Err(InterpreterError::MissingEntrypoint(entrypoint.clone())),
        Some(body) => Ok(body),
    }
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

    let (target, target_sig) = find_entrypoint_body(entrypoint, &module)?;
    {
        let var = tc.fresh_typevar(syntax::Kind::Type);
        let expected = syntax::Type::mk_app(syntax::Type::IO, var);
        let actual = target_sig.body;
        let context = typecheck::UnifyTypeContext {
            expected: expected.clone(),
            actual: actual.clone(),
        };
        let _ = tc.unify_type(&context, expected, actual)?;
    }

    let heap = Arena::new();
    let env = Vec::new();
    let _result = {
        let mut stdout = io::stdout();
        let mut interpreter = Interpreter::new_with_builtins(&mut stdout, &heap);
        let action = interpreter.eval(&env, target);
        action.perform_io(&mut interpreter)
    };
    Ok(())
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
