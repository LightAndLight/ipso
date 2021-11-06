use eval::Env;
use ipso_builtins as builtins;
use ipso_core::{self as core, ModulePath};
use ipso_diagnostic::Source;
use ipso_eval::{self as eval, Interpreter};
use ipso_import as import;
use ipso_parse as parse;
use ipso_syntax::{self as syntax, Kind};
use ipso_typecheck::{self as typecheck, Typechecker};
use std::{
    io::{self, BufRead, BufReader, Write},
    path::PathBuf,
};
use typed_arena::Arena;

pub struct Config {
    pub filename: String,
    pub entrypoint: Option<String>,
    pub stdin: Option<Box<dyn BufRead>>,
    pub stdout: Option<Box<dyn Write>>,
}

#[derive(Debug)]
pub enum InterpreterError {
    ParseError(parse::ParseError),
    TypeError(typecheck::TypeError),
    ModuleError(import::ModuleError),
    MissingEntrypoint(String),
    FileDoesNotExist(PathBuf),
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

fn find_entrypoint_signature(
    entrypoint: &str,
    module: &core::Module,
) -> Result<core::TypeSig, InterpreterError> {
    match module.decls.iter().find_map(|decl| match decl {
        core::Declaration::Definition {
            name, sig, body, ..
        } if name == entrypoint => Some((body.clone(), sig.clone())),
        _ => None,
    }) {
        None => Err(InterpreterError::MissingEntrypoint(entrypoint.to_string())),
        Some((_, sig)) => Ok(sig),
    }
}

pub fn run_interpreter(config: Config) -> Result<(), InterpreterError> {
    let working_dir = std::env::current_dir().unwrap();

    let main = String::from("main");

    let modules_data = Arena::new();
    let mut modules = import::Modules::new(&modules_data);
    let source = Source::Interactive {
        label: main.clone(),
    };
    let target_path = PathBuf::from(config.filename.as_str());
    if !target_path.exists() {
        return Err(InterpreterError::FileDoesNotExist(target_path));
    }
    let target_module_path: ModulePath = ModulePath::from_file(&target_path);
    let builtins = builtins::builtins();
    let module = modules.import(&source, 0, &target_module_path, &builtins)?;

    let entrypoint: &String = match &config.entrypoint {
        None => &main,
        Some(value) => value,
    };
    let target_sig = find_entrypoint_signature(entrypoint, module)?;
    {
        let mut tc = {
            let mut tc = Typechecker::new(working_dir.as_path(), source, &modules.index);
            tc.register_from_import(&builtins, &syntax::Names::All);
            tc
        };
        let expected = syntax::Type::mk_app(syntax::Type::IO, tc.fresh_typevar(Kind::Type));
        let actual = target_sig.body;
        let context = typecheck::UnifyTypeContext {
            expected: expected.clone(),
            actual: actual.clone(),
        };
        let _ = tc.unify_type(&context, &expected, &actual)?;
    }

    let bytes = Arena::new();
    let values = Arena::new();
    let objects = Arena::new();
    let mut env = Env::new();
    let _result = {
        let mut stdout = config.stdout.unwrap_or_else(|| Box::new(io::stdout()));
        let mut stdin = config
            .stdin
            .unwrap_or_else(|| Box::new(BufReader::new(io::stdin())));
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
        let context = builtins
            .decls
            .iter()
            .flat_map(|decl| decl.get_bindings().into_iter())
            .chain(
                module
                    .decls
                    .iter()
                    .flat_map(|decl| decl.get_bindings().into_iter()),
            )
            .collect();
        let mut interpreter = Interpreter::new(
            &mut stdin,
            &mut stdout,
            &context,
            eval_modules,
            &bytes,
            &values,
            &objects,
        );
        let action = interpreter.eval_from_module(&mut env, &target_module_path, entrypoint);
        action.perform_io(&mut interpreter)
    };
    Ok(())
}
