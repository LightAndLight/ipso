use typed_arena::Arena;

use crate::{
    core,
    diagnostic::{self, Diagnostic},
    lex::Lexer,
    parse::{self, Parser},
    rope::Rope,
    syntax,
    typecheck::{self, Typechecker},
};
use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
    path::{Path, PathBuf},
};

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct ModulePath(PathBuf);

impl ModulePath {
    pub fn new(dir: &Path, module_name: &String) -> Self {
        ModulePath(dir.join(format!("{}.ipso", module_name)))
    }

    pub fn from_path(path: &Path) -> Self {
        ModulePath(PathBuf::from(path))
    }

    pub fn as_path<'a>(&'a self) -> &'a Path {
        self.0.as_path()
    }

    pub fn to_str<'a>(&'a self) -> &'a str {
        self.0.to_str().unwrap()
    }
}

#[derive(Debug)]
pub enum ModuleError {
    NotFound { pos: usize, path: ModulePath },
    IO(io::Error),
    Parse(parse::ParseError),
    Check(typecheck::TypeError),
}

impl ModuleError {
    pub fn report(&self, diagnostic: &mut Diagnostic) {
        match self {
            ModuleError::NotFound { pos, path } => diagnostic.item(diagnostic::Item {
                pos: *pos,
                message: String::from("module not found"),
                addendum: Some(format!("file {} does not exist", path.to_str())),
            }),
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
    index: HashMap<ModulePath, &'a core::Module>,
}

fn desugar_module_accessors_expr(module_names: Rope<String>, expr: &mut syntax::Expr) {
    match expr {
        syntax::Expr::Var(_) => {}
        syntax::Expr::Module { .. } => {}
        syntax::Expr::App(a, b) => {
            desugar_module_accessors_expr(module_names.clone(), &mut (*a).item);
            desugar_module_accessors_expr(module_names, &mut (*b).item);
        }
        syntax::Expr::Lam { args, body } => {
            let module_names = {
                let mut module_names = module_names;
                for name in args.iter().flat_map(|pattern| pattern.iter_names()) {
                    module_names = module_names
                        .delete_first(|x| x == &name.item)
                        .unwrap_or_else(|x| x);
                }
                module_names
            };
            desugar_module_accessors_expr(module_names, &mut (*body).item);
        }
        syntax::Expr::True => {}
        syntax::Expr::False => {}
        syntax::Expr::IfThenElse(a, b, c) => {
            desugar_module_accessors_expr(module_names.clone(), &mut (*a).item);
            desugar_module_accessors_expr(module_names.clone(), &mut (*b).item);
            desugar_module_accessors_expr(module_names, &mut (*c).item);
        }
        syntax::Expr::Int(_) => {}
        syntax::Expr::Binop(_, a, b) => {
            desugar_module_accessors_expr(module_names.clone(), &mut (*a).item);
            desugar_module_accessors_expr(module_names, &mut (*b).item);
        }
        syntax::Expr::Char(_) => {}
        syntax::Expr::String(parts) => {
            for part in parts {
                match part {
                    syntax::StringPart::String(_) => {}
                    syntax::StringPart::Expr(e) => {
                        desugar_module_accessors_expr(module_names.clone(), &mut e.item);
                    }
                }
            }
        }
        syntax::Expr::Array(xs) => {
            for x in xs {
                desugar_module_accessors_expr(module_names.clone(), &mut x.item);
            }
        }
        syntax::Expr::Record { fields, rest } => {
            for (_, e) in fields {
                desugar_module_accessors_expr(module_names.clone(), &mut e.item);
            }
            for e in rest {
                desugar_module_accessors_expr(module_names.clone(), &mut (*e).item);
            }
        }
        syntax::Expr::Project(value, field) => {
            let (head, tail) = (*value).item.unwrap_projects();
            match head {
                syntax::Expr::Var(head) => match module_names.iter().find(|x| *x == head) {
                    Some(_) => {
                        let mut path: Vec<String> = Vec::with_capacity(tail.len() + 1);
                        path.push(head.clone());
                        path.extend(tail.into_iter().map(|x| x.clone()));
                        *expr = syntax::Expr::Module {
                            name: syntax::ModuleName(path),
                            item: field.clone(),
                        }
                    }
                    None => {
                        todo!()
                    }
                },
                _ => {
                    todo!()
                }
            }
        }
        syntax::Expr::Variant(_) => {}
        syntax::Expr::Embed(_, a) => {
            desugar_module_accessors_expr(module_names, &mut (*a).item);
        }
        syntax::Expr::Case(a, branches) => {
            desugar_module_accessors_expr(module_names.clone(), &mut (*a).item);
            for branch in branches {
                let module_names = {
                    let mut module_names = module_names.clone();
                    for name in branch.pattern.item.iter_names() {
                        module_names = module_names
                            .delete_first(|x| x == &name.item)
                            .unwrap_or_else(|x| x);
                    }
                    module_names
                };
                desugar_module_accessors_expr(module_names, &mut branch.body.item);
            }
        }
        syntax::Expr::Unit => {}
    }
}

fn desugar_module_accessors_decl(module_names: Rope<String>, decl: &mut syntax::Declaration) {
    match decl {
        syntax::Declaration::Definition {
            name,
            ty: _,
            args,
            body,
        } => {
            let module_names = {
                let mut module_names = module_names
                    .delete_first(|x| x == name)
                    .unwrap_or_else(|x| x);
                for name in args.iter().flat_map(|pattern| pattern.iter_names()) {
                    module_names = module_names
                        .delete_first(|x| x == &name.item)
                        .unwrap_or_else(|x| x);
                }
                module_names
            };
            desugar_module_accessors_expr(module_names, &mut body.item)
        }
        syntax::Declaration::Class { .. } => {}
        syntax::Declaration::Instance {
            assumes: _,
            name: _,
            args: _,
            members,
        } => {
            for (name, args, body) in members {
                let module_names = {
                    let mut module_names = module_names
                        .clone()
                        .delete_first(|x| x == &name.item)
                        .unwrap_or_else(|x| x);
                    for name in args.iter().flat_map(|pattern| pattern.iter_names()) {
                        module_names = module_names
                            .delete_first(|x| x == &name.item)
                            .unwrap_or_else(|x| x);
                    }
                    module_names
                };
                desugar_module_accessors_expr(module_names, &mut body.item)
            }
        }
        syntax::Declaration::TypeAlias { .. } => {}
        syntax::Declaration::Import { .. } => {}
        syntax::Declaration::FromImport { .. } => {}
    }
}

struct ImportInfo {
    pos: usize,
    path: ModulePath,
    module: Option<String>,
}

fn calculate_imports(working_dir: &Path, module: &mut syntax::Module) -> Vec<ImportInfo> {
    let mut paths: Vec<ImportInfo> = Vec::new();
    for decl in &mut module.decls {
        match &decl.item {
            syntax::Declaration::Definition { .. }
            | syntax::Declaration::Class { .. }
            | syntax::Declaration::Instance { .. }
            | syntax::Declaration::TypeAlias { .. } => {}
            syntax::Declaration::Import { module, .. } => {
                paths.push(ImportInfo {
                    pos: module.pos,
                    path: ModulePath::new(working_dir, &module.item),
                    module: Some(module.item.clone()),
                });
            }
            syntax::Declaration::FromImport { module, .. } => {
                paths.push(ImportInfo {
                    pos: module.pos,
                    path: ModulePath::new(working_dir, &module.item),
                    module: None,
                });
            }
        }

        let module_names: Vec<String> = paths.iter().filter_map(|x| x.module.clone()).collect();
        desugar_module_accessors_decl(Rope::from_vec(&module_names), &mut decl.item)
    }
    paths
}

impl<'a> Modules<'a> {
    pub fn new(data: &'a Arena<core::Module>) -> Self {
        Modules {
            data,
            index: HashMap::new(),
        }
    }

    pub fn lookup(&self, path: &ModulePath) -> Option<&core::Module> {
        self.index.get(path).map(|x| *x)
    }

    /// Import a module.
    ///
    /// Module imports are cached, so importing the same module repeatedly is cheap.
    ///
    /// * `pos` - source file offset for error reporting
    pub fn import(
        &mut self,
        pos: usize,
        module_path: &ModulePath,
    ) -> Result<&'a core::Module, ModuleError> {
        match self.index.get(module_path) {
            None => {
                let path = module_path.as_path();
                if path.exists() {
                    let mut module = {
                        let content = {
                            let mut file = File::open(path)?;
                            let mut content = String::new();
                            file.read_to_string(&mut content)?;
                            content
                        };

                        let tokens = Lexer::new(&content).tokenize();
                        let mut parser = Parser::new(tokens);

                        parser
                            .module()
                            .and_then(|module| parser.eof().map(|_| module))
                            .result
                    }?;

                    let working_dir = path.parent().unwrap();
                    for import_info in calculate_imports(working_dir, &mut module).into_iter() {
                        match self.import(import_info.pos, &import_info.path) {
                            Err(err) => {
                                return Err(err);
                            }
                            Ok(_) => {}
                        }
                    }
                    let module = {
                        let mut tc = Typechecker::new_with_builtins(working_dir, self);
                        tc.check_module(&module)
                    }?;
                    let module_ref: &core::Module = self.data.alloc(module);
                    self.index.insert(module_path.clone(), module_ref);
                    Ok(module_ref)
                } else {
                    Err(ModuleError::NotFound {
                        pos,
                        path: module_path.clone(),
                    })
                }
            }
            Some(module_ref) => Ok(*module_ref),
        }
    }
}
