use eval::Env;
use ipso_builtins as builtins;
use ipso_core::{self as core, Binding, CommonKinds, Name};
use ipso_diagnostic::Source;
use ipso_eval::{self as eval, Interpreter};
use ipso_import as import;
use ipso_parse as parse;
use ipso_syntax::{kind::Kind, ModuleKey, ModuleRef, Modules};
use ipso_typecheck::{self as typecheck, kind_inference, type_inference, BoundVars};
use std::{
    collections::HashMap,
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

impl From<type_inference::InferenceError> for InterpreterError {
    fn from(err: type_inference::InferenceError) -> Self {
        InterpreterError::TypeError(typecheck::TypeError::from(err))
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
    let main = String::from("main");

    let mut modules = Modules::new();
    let source = Source::Interactive {
        label: main.clone(),
    };
    let target_path = PathBuf::from(config.filename.as_str());
    if !target_path.exists() {
        return Err(InterpreterError::FileDoesNotExist(target_path));
    }
    let common_kinds = CommonKinds::default();
    let builtins_module_id = {
        let builtins = builtins::builtins(&common_kinds);
        modules.insert(ModuleKey::from("builtins"), builtins)
    };
    let module_id = import::import(
        &mut modules,
        builtins_module_id,
        &source,
        0,
        &target_path,
        &common_kinds,
    )?;
    let module = modules.lookup(module_id);

    let entrypoint: &String = match &config.entrypoint {
        None => &main,
        Some(value) => value,
    };
    let target_sig = find_entrypoint_signature(entrypoint, module)?;
    {
        let mut kind_solutions = kind_inference::Solutions::new();
        let mut type_solutions = type_inference::unification::Solutions::new();

        let expected = core::Type::app(
            core::Type::mk_io(&common_kinds),
            core::Type::Meta(Kind::Type, type_solutions.fresh_meta()),
        );
        let actual = target_sig.body;

        let _ = type_inference::unify(
            &common_kinds,
            &HashMap::new(),
            &BoundVars::new(),
            &mut kind_solutions,
            &mut type_solutions,
            &source,
            None,
            &expected,
            &actual,
        )?;
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
        let context: HashMap<Name, Binding> = module
            .decls
            .iter()
            .flat_map(|decl| decl.get_bindings(&common_kinds).into_iter())
            .collect();
        let mut interpreter = Interpreter::new(
            &mut stdin,
            &mut stdout,
            &common_kinds,
            &modules,
            &context,
            &bytes,
            &values,
            &objects,
        );
        let action = interpreter.eval_from_module(
            &mut env,
            &ModuleRef::from(module_id),
            &[],
            &Name::definition(entrypoint),
        );
        action.perform_io(&mut interpreter)
    };
    Ok(())
}
