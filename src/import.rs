use typed_arena::Arena;

use crate::{
    core,
    diagnostic::Diagnostic,
    lex::Lexer,
    parse::{self, Parser},
    syntax,
    typecheck::{self, Typechecker},
};
use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
pub enum ModuleError {
    IO(io::Error),
    Parse(parse::ParseError),
    Check(typecheck::TypeError),
}

impl ModuleError {
    pub fn report(&self, diagnostic: &mut Diagnostic) {
        match self {
            ModuleError::IO(err) => panic!("ioerror: {}", err),
            ModuleError::Parse(err) => err.report(diagnostic),
            ModuleError::Check(err) => err.report(diagnostic),
        }
    }
}

impl From<io::Error> for ModuleError {
    fn from(err: io::Error) -> Self {
        ModuleError::IO(err)
    }
}

impl From<parse::ParseError> for ModuleError {
    fn from(err: parse::ParseError) -> Self {
        ModuleError::Parse(err)
    }
}

impl From<typecheck::TypeError> for ModuleError {
    fn from(err: typecheck::TypeError) -> Self {
        ModuleError::Check(err)
    }
}

pub struct Modules<'a> {
    data: &'a Arena<core::Module>,
    index: HashMap<String, &'a core::Module>,
}

fn calculate_imports<'a>(module: &'a syntax::Module) -> Vec<&'a String> {
    todo!()
}

impl<'a> Modules<'a> {
    pub fn new(data: &'a Arena<core::Module>) -> Self {
        Modules {
            data,
            index: HashMap::new(),
        }
    }

    pub fn import(&mut self, path: &String) -> Result<&'a core::Module, ModuleError> {
        match self.index.get(path) {
            None => {
                let mut file = File::open(path)?;
                let mut content = String::new();
                file.read_to_string(&mut content)?;
                let tokens = Lexer::new(&content).tokenize();
                let mut parser = Parser::new(tokens);
                let module = parser
                    .module()
                    .and_then(|module| parser.eof().map(|_| module))
                    .result?;
                let imports = calculate_imports(&module);
                for import in imports.into_iter() {
                    match self.import(import) {
                        Err(err) => {
                            return Err(err);
                        }
                        Ok(_) => {}
                    }
                }
                let mut tc = Typechecker::new_with_builtins(self);
                let module = tc.check_module(&module)?;
                let module_ref: &core::Module = self.data.alloc(module);
                self.index.insert(path.clone(), module_ref);
                Ok(module_ref)
            }
            Some(module_ref) => Ok(*module_ref),
        }
    }
}
