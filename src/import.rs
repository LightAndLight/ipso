use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
};

use crate::{
    core,
    diagnostic::Diagnostic,
    lex::Lexer,
    parse::{self, Parser},
    syntax,
    typecheck::{self, Typechecker},
};

#[derive(PartialEq, Eq, Debug, Hash)]
struct ModuleName(Vec<String>);

enum ModuleStage {
    Unparsed(String),
    Parsed(syntax::Module),
    Checking,
    Checked(core::Module),
}

pub struct ModuleInfo {
    stage: ModuleStage,
}

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

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct ModuleRef(usize);

pub struct Modules {
    data: Vec<ModuleInfo>,
    index: HashMap<String, usize>,
}

impl Modules {
    pub fn new() -> Self {
        Modules {
            data: Vec::new(),
            index: HashMap::new(),
        }
    }

    fn lookup_mut<'a>(&'a mut self, module_ref: ModuleRef) -> &'a mut ModuleInfo {
        &mut self.data[module_ref.0]
    }

    pub fn lookup<'a>(&'a self, module_ref: ModuleRef) -> &'a ModuleInfo {
        &self.data[module_ref.0]
    }

    pub fn load(&mut self, filepath: &String) -> Result<ModuleRef, ModuleError> {
        match self.index.get(filepath) {
            None => {
                let mut contents = String::new();
                let _ = {
                    let mut file = File::open(filepath)?;
                    file.read_to_string(&mut contents)
                }?;

                let ix = self.data.len();
                self.index.insert(filepath.clone(), ix);
                self.data.push(ModuleInfo {
                    stage: ModuleStage::Unparsed(contents),
                });
                Ok(ModuleRef(ix))
            }
            Some(ix) => Ok(ModuleRef(*ix)),
        }
    }

    pub fn parse(&mut self, filepath: &String) -> Result<ModuleRef, ModuleError> {
        let module_ref = self.load(filepath)?;
        let info = self.lookup_mut(module_ref);
        match &info.stage {
            ModuleStage::Unparsed(content) => {
                let lexer = Lexer::new(content);
                let tokens = lexer.tokenize();
                let mut parser = Parser::new(tokens);
                let module = parser
                    .module()
                    .and_then(|module| parser.eof().map(|_| module))
                    .result?;
                info.stage = ModuleStage::Parsed(module);
                Ok(module_ref)
            }
            _ => Ok(module_ref),
        }
    }

    pub fn check(&mut self, filepath: &String) -> Result<ModuleRef, ModuleError> {
        let module_ref = self.parse(filepath)?;
        let info = self.lookup_mut(module_ref);
        let stage = std::mem::replace(&mut info.stage, ModuleStage::Checking);
        match stage {
            ModuleStage::Parsed(module) => {
                let mut tc = Typechecker::new_with_builtins(self);
                let module = tc.check_module(&module)?;
                self.lookup_mut(module_ref).stage = ModuleStage::Checked(module);
                Ok(module_ref)
            }
            ModuleStage::Checking => {
                panic!("loop when checking {}", filepath)
            }
            stage => {
                let _ = std::mem::replace(&mut info.stage, stage);
                Ok(module_ref)
            }
        }
    }
}
