use diagnostic::{self, Diagnostic, InputLocation};
use typed_arena::Arena;

use crate::typecheck::{self, Typechecker};
use core::{self, Module, ModulePath};
use parse::{self};
use rope::Rope;
use std::{
    collections::HashMap,
    io::{self},
    path::{Path, PathBuf},
    rc::Rc,
};
use syntax::{self, ModuleName};

#[derive(Debug)]
pub enum ModuleError {
    NotFound {
        location: InputLocation,
        pos: usize,
        module_path: ModulePath,
    },
    IO(io::Error),
    Parse(parse::ParseError),
    Check(typecheck::TypeError),
}

impl ModuleError {
    pub fn report(&self, diagnostic: &mut Diagnostic) {
        match self {
            ModuleError::NotFound {
                location,
                pos,
                module_path,
            } => diagnostic.item(diagnostic::Item {
                location: location.clone(),
                offset: *pos,
                message: String::from("module not found"),
                addendum: Some(format!("file {} does not exist", module_path.to_str())),
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

fn desugar_module_accessors_expr(module_names: &Rope<String>, expr: &mut syntax::Expr) {
    match expr {
        syntax::Expr::Var(_) => {}
        syntax::Expr::Module { .. } => {}
        syntax::Expr::App(a, b) => {
            desugar_module_accessors_expr(module_names, &mut Rc::make_mut(a).item);
            desugar_module_accessors_expr(module_names, &mut Rc::make_mut(b).item);
        }
        syntax::Expr::Lam { args, body } => {
            let module_names = {
                let mut module_names = module_names.clone();
                for name in args.iter().flat_map(|pattern| pattern.iter_names()) {
                    module_names = module_names
                        .delete_first(|x| x == &name.item)
                        .unwrap_or_else(|x| x);
                }
                module_names
            };
            desugar_module_accessors_expr(&module_names, &mut Rc::make_mut(body).item)
        }
        syntax::Expr::True => {}
        syntax::Expr::False => {}
        syntax::Expr::IfThenElse(a, b, c) => {
            desugar_module_accessors_expr(module_names, &mut Rc::make_mut(a).item);
            desugar_module_accessors_expr(module_names, &mut Rc::make_mut(b).item);
            desugar_module_accessors_expr(module_names, &mut Rc::make_mut(c).item);
        }
        syntax::Expr::Int(_) => {}
        syntax::Expr::Binop(_, a, b) => {
            desugar_module_accessors_expr(module_names, &mut Rc::make_mut(a).item);
            desugar_module_accessors_expr(module_names, &mut Rc::make_mut(b).item);
        }
        syntax::Expr::Char(_) => {}
        syntax::Expr::String(parts) => {
            for part in parts {
                match part {
                    syntax::StringPart::String(_) => {}
                    syntax::StringPart::Expr(e) => {
                        desugar_module_accessors_expr(module_names, &mut e.item);
                    }
                }
            }
        }
        syntax::Expr::Array(xs) => {
            for x in xs {
                desugar_module_accessors_expr(module_names, &mut x.item);
            }
        }
        syntax::Expr::Record { fields, rest } => {
            for (_, e) in fields {
                desugar_module_accessors_expr(module_names, &mut e.item);
            }

            for e in rest {
                desugar_module_accessors_expr(module_names, &mut Rc::make_mut(e).item);
            }
        }
        syntax::Expr::Project(value, field) => {
            let (head, tail) = (*value).item.unwrap_projects();
            match head {
                syntax::Expr::Var(head) => match module_names.iter().find(|x| *x == head) {
                    Some(_) => {
                        let mut path: Vec<String> = Vec::with_capacity(tail.len() + 1);
                        path.push(head.clone());
                        path.extend(tail.into_iter().cloned());
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
            desugar_module_accessors_expr(module_names, &mut Rc::make_mut(a).item);
        }
        syntax::Expr::Case(a, branches) => {
            desugar_module_accessors_expr(module_names, &mut Rc::make_mut(a).item);

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
                desugar_module_accessors_expr(&module_names, &mut branch.body.item);
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
            desugar_module_accessors_expr(&module_names, &mut body.item);
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
                desugar_module_accessors_expr(&module_names, &mut body.item)
            }
        }
        syntax::Declaration::TypeAlias { .. } => {}
        syntax::Declaration::Import { .. } => {}
        syntax::Declaration::FromImport { .. } => {}
    }
}

struct ImportInfo {
    pos: usize,
    module_path: ModulePath,
    module: Option<String>,
}

fn calculate_imports(file: &Path, module: &mut syntax::Module) -> Vec<ImportInfo> {
    let working_dir = file.parent().unwrap();
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
                    module_path: ModulePath::from_module(
                        working_dir,
                        &ModuleName(vec![module.item.clone()]),
                    ),
                    module: Some(module.item.clone()),
                });
            }
            syntax::Declaration::FromImport { module, .. } => {
                paths.push(ImportInfo {
                    pos: module.pos,
                    module_path: ModulePath::from_module(
                        working_dir,
                        &ModuleName(vec![module.item.clone()]),
                    ),
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

    pub fn iter(&self) -> std::collections::hash_map::Iter<ModulePath, &core::Module> {
        self.index.iter()
    }

    pub fn lookup(&self, path: &ModulePath) -> Option<&core::Module> {
        self.index.get(path).copied()
    }

    /// Import a module.
    ///
    /// Module imports are cached, so importing the same module repeatedly is cheap.
    ///
    /// * `location` - source file location for error reporting
    /// * `pos` - source file offset for error reporting
    pub fn import(
        &mut self,
        location: &InputLocation,
        pos: usize,
        module_path: &ModulePath,
        builtins: &Module,
    ) -> Result<&'a core::Module, ModuleError> {
        match self.index.get(module_path) {
            None => {
                let path = module_path.as_path();
                if path.exists() {
                    let input_location = InputLocation::File {
                        path: PathBuf::from(path),
                    };
                    let mut module = parse::parse_file(path)?;

                    for import_info in calculate_imports(path, &mut module).into_iter() {
                        if let Err(err) = self.import(
                            &InputLocation::File {
                                path: PathBuf::from(path),
                            },
                            import_info.pos,
                            &import_info.module_path,
                            builtins,
                        ) {
                            return Err(err);
                        }
                    }
                    let module = {
                        let working_dir = path.parent().unwrap();
                        let mut tc = {
                            let mut tc = Typechecker::new(working_dir, input_location, self);
                            tc.register_from_import(builtins, &syntax::Names::All);
                            tc
                        };
                        tc.check_module(&module)
                    }?;
                    let module_ref: &core::Module = self.data.alloc(module);
                    self.index.insert(module_path.clone(), module_ref);
                    Ok(module_ref)
                } else {
                    Err(ModuleError::NotFound {
                        location: location.clone(),
                        pos,
                        module_path: module_path.clone(),
                    })
                }
            }
            Some(module_ref) => Ok(*module_ref),
        }
    }
}
