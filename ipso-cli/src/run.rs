use eval::Env;
use ipso_builtins as builtins;
use ipso_core::{self as core, Binding, CommonKinds, Module, Name};
use ipso_diagnostic::Source;
use ipso_eval::{self as eval, Interpreter};
use ipso_import as import;
use ipso_parse as parse;
use ipso_syntax::{kind::Kind, ModuleId, ModuleKey, ModuleRef, Modules};
use ipso_typecheck::{self as typecheck, kind_inference, type_inference, BoundVars};
use std::{
    collections::HashMap,
    io::{self, BufRead, BufReader, Write},
    path::PathBuf,
    rc::Rc,
};

pub struct Config {
    pub filename: String,
    pub entrypoint: Option<String>,
    pub args: Vec<Rc<str>>,
    pub stdin: Option<Box<dyn BufRead>>,
    pub stdout: Option<Box<dyn Write>>,
}

#[derive(Debug)]
pub enum InterpreterError {
    ParseError(parse::Error),
    TypeError(Box<typecheck::Error>),
    ImportError(import::Error),
    MissingEntrypoint(String),
    FileDoesNotExist(PathBuf),
}

impl From<parse::Error> for InterpreterError {
    fn from(err: parse::Error) -> Self {
        InterpreterError::ParseError(err)
    }
}

impl From<typecheck::Error> for InterpreterError {
    fn from(err: typecheck::Error) -> Self {
        InterpreterError::TypeError(Box::new(err))
    }
}

impl From<type_inference::Error> for InterpreterError {
    fn from(err: type_inference::Error) -> Self {
        Self::from(typecheck::Error::from(err))
    }
}

impl From<import::Error> for InterpreterError {
    fn from(err: import::Error) -> Self {
        InterpreterError::ImportError(err)
    }
}

fn find_entrypoint_signature(
    entrypoint: &str,
    module: &core::Module,
) -> Result<core::TypeSig, InterpreterError> {
    match module.decls.iter().find_map(|decl| match decl {
        core::Declaration::Definition {
            name, sig, body, ..
        } if name.as_ref() == entrypoint => Some((body.clone(), sig.clone())),
        _ => None,
    }) {
        None => Err(InterpreterError::MissingEntrypoint(entrypoint.to_string())),
        Some((_, sig)) => Ok(sig),
    }
}

pub fn check(
    common_kinds: &CommonKinds,
    filename: &str,
    entrypoint: Option<String>,
) -> Result<(Modules<Module>, ModuleId, String), InterpreterError> {
    let mut modules = Modules::new();
    let builtins_module_id = {
        let builtins = builtins::builtins(common_kinds);
        modules.insert(ModuleKey::from("builtins"), builtins)
    };

    let target_path = PathBuf::from(filename);
    if !target_path.exists() {
        return Err(InterpreterError::FileDoesNotExist(target_path));
    }
    let source = Source::File {
        path: target_path.clone(),
    };
    let module_id = import::import(
        &mut modules,
        builtins_module_id,
        &source,
        0,
        &target_path,
        common_kinds,
    )?;
    let module = modules.lookup(module_id);

    let entrypoint = match entrypoint {
        None => String::from("main"),
        Some(value) => value,
    };
    let target_sig = find_entrypoint_signature(&entrypoint, module)?;
    {
        let mut kind_inference_state = kind_inference::State::new();
        let mut type_solutions = type_inference::unification::Solutions::new();

        let expected = core::Type::app(
            core::Type::mk_io(common_kinds),
            core::Type::Meta(Kind::Type, type_solutions.fresh_meta()),
        );
        let actual = target_sig.body;

        type_inference::unification::unify(
            type_inference::unification::Env {
                common_kinds,
                types: &HashMap::new(),
                type_variables: &BoundVars::new(),
            },
            &mut kind_inference_state,
            &mut type_solutions,
            0,
            &expected,
            &actual,
        )
        .map_err(|error| {
            type_inference::Error::unification_error(
                // TODO: figure out how to give a useful position?
                &source, 0, error,
            )
        })?;
    }

    Ok((modules, module_id, entrypoint))
}

#[derive(Default)]
pub struct IO {
    stdin: Option<Box<dyn BufRead>>,
    stdout: Option<Box<dyn Write>>,
    stderr: Option<Box<dyn Write>>,
}

pub fn run(
    common_kinds: &CommonKinds,
    modules: Modules<Module>,
    module_id: ModuleId,
    filename: &str,
    args: &[Rc<str>],
    entrypoint: &str,
    io: IO,
) -> Result<(), InterpreterError> {
    let module = modules.lookup(module_id);

    let mut env = Env::new();
    let _result = {
        let mut stdout = io.stdout.unwrap_or_else(|| Box::new(io::stdout()));
        let mut stderr = io.stderr.unwrap_or_else(|| Box::new(io::stderr()));
        let mut stdin = io
            .stdin
            .unwrap_or_else(|| Box::new(BufReader::new(io::stdin())));
        let context: HashMap<Name, Binding> = module
            .decls
            .iter()
            .flat_map(|decl| decl.get_bindings(common_kinds).into_iter())
            .collect();
        let mut interpreter = Interpreter::new(
            Rc::from(filename),
            args,
            ipso_eval::IO {
                stdin: &mut stdin,
                stdout: &mut stdout,
                stderr: &mut stderr,
            },
            common_kinds,
            &modules,
            &context,
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
