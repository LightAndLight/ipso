use diagnostic::InputLocation;
use ipso::{
    core,
    diagnostic::{self, Diagnostic},
    eval::{self, Interpreter},
    import::{self, ModulePath},
    parse, syntax,
    typecheck::{self, Typechecker},
};
use std::{
    env, io,
    path::{Path, PathBuf},
};
use typed_arena::Arena;

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
    ModuleError(import::ModuleError),
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

impl From<import::ModuleError> for InterpreterError {
    fn from(err: import::ModuleError) -> Self {
        InterpreterError::ModuleError(err)
    }
}

fn report_interpreter_error(config: &Config, err: InterpreterError) -> io::Result<()> {
    let mut diagnostic = Diagnostic::new();
    match err {
        InterpreterError::ParseError(err) => err.report(&mut diagnostic),
        InterpreterError::TypeError(err) => err.report(&mut diagnostic),
        InterpreterError::ModuleError(err) => err.report(&mut diagnostic),
        InterpreterError::MissingEntrypoint(name) => diagnostic.item(diagnostic::Item {
            location: InputLocation::File {
                path: PathBuf::from(&config.filename),
            },
            offset: 0,
            message: format!("missing entrypoint {:?}", name),
            addendum: None,
        }),
    }
    diagnostic.report_all()
}

fn find_entrypoint_signature(
    entrypoint: &String,
    module: &core::Module,
) -> Result<core::TypeSig, InterpreterError> {
    match module.decls.iter().find_map(|decl| match decl {
        core::Declaration::Definition {
            name, sig, body, ..
        } if name == entrypoint => Some((body.clone(), sig.clone())),
        _ => None,
    }) {
        None => Err(InterpreterError::MissingEntrypoint(entrypoint.clone())),
        Some((_, sig)) => Ok(sig),
    }
}

fn run_interpreter(config: &Config) -> Result<(), InterpreterError> {
    let working_dir = std::env::current_dir().unwrap();
    let target_path: ModulePath = ModulePath::from_file(&PathBuf::from(config.filename.as_str()));
    let main = String::from("main");
    let entrypoint: &String = match config.entrypoint {
        None => &main,
        Some(ref value) => value,
    };
    let modules_data = Arena::new();
    let mut modules = import::Modules::new(&modules_data);
    let input_location = InputLocation::Interactive {
        label: String::from("main"),
    };
    let module = modules.import(&input_location, 0, &target_path)?;

    let target_sig = find_entrypoint_signature(entrypoint, module)?;
    {
        let mut tc =
            Typechecker::new_with_builtins(working_dir.as_path(), input_location, &modules);
        let expected = syntax::Type::mk_app(syntax::Type::IO, tc.fresh_typevar(syntax::Kind::Type));
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
        let context = module
            .decls
            .iter()
            .flat_map(|decl| decl.get_bindings().into_iter())
            .collect();
        let eval_modules = modules
            .iter()
            .map(|(module_path, module)| {
                (
                    module_path.clone(),
                    eval::Module {
                        module_mapping: module.module_mapping.clone(),
                        bindings: module.get_bindings(),
                    },
                )
            })
            .collect();
        let mut interpreter =
            Interpreter::new_with_builtins(&mut stdout, context, eval_modules, &heap);
        let action = interpreter.eval_from_module(&env, &target_path, entrypoint);
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
