use diagnostic::{Location, Message};
use ipso_core::{self as core, CommonKinds, Module, ModulePath};
use ipso_diagnostic::{self as diagnostic, Diagnostic, Source};
use ipso_parse as parse;
use ipso_rope::Rope;
use ipso_syntax::{self as syntax, ModuleName};
use ipso_typecheck::{self as typecheck, Typechecker};
use std::{
    collections::HashMap,
    io::{self},
    path::{Path, PathBuf},
    rc::Rc,
};
use typed_arena::Arena;

#[derive(Debug)]
pub enum ModuleError {
    NotFound {
        source: Source,
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
                source,
                pos,
                module_path,
            } => diagnostic.item(
                Some(Location {
                    source: source.clone(),
                    offset: Some(*pos),
                }),
                Message {
                    content: String::from("module not found"),
                    addendum: Some(format!("file {} does not exist", module_path.to_str())),
                },
            ),
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
    pub index: HashMap<ModulePath, &'a core::Module>,
}

fn desugar_module_accessors_comp_line(module_names: &Rope<String>, line: &mut syntax::CompLine) {
    match line {
        syntax::CompLine::Expr(value) => {
            desugar_module_accessors_expr(module_names, &mut value.item);
        }
        syntax::CompLine::Bind(_, value) => {
            desugar_module_accessors_expr(module_names, &mut value.item);
        }
        syntax::CompLine::Let(_, value) => {
            desugar_module_accessors_expr(module_names, &mut value.item);
        }
    }
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
                for name in args.iter().flat_map(|pattern| pattern.item.iter_names()) {
                    module_names = module_names
                        .delete_first(|x| x == &name.item)
                        .unwrap_or_else(|x| x);
                }
                module_names
            };
            desugar_module_accessors_expr(&module_names, &mut Rc::make_mut(body).item)
        }
        syntax::Expr::Let { value, rest, .. } => {
            desugar_module_accessors_expr(module_names, &mut Rc::make_mut(value).item);
            desugar_module_accessors_expr(module_names, &mut Rc::make_mut(rest).item);
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
            desugar_module_accessors_expr(module_names, &mut Rc::make_mut(value).item);

            let (head, tail) = (*value).item.unwrap_projects();

            if let syntax::Expr::Var(head) = head {
                if module_names.iter().any(|x| x == head) {
                    let mut path: Vec<String> = Vec::with_capacity(tail.len() + 1);
                    path.push(head.clone());
                    path.extend(tail.into_iter().cloned());
                    *expr = syntax::Expr::Module {
                        name: syntax::ModuleName(path),
                        item: field.clone(),
                    }
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
        syntax::Expr::Comp(lines) => lines
            .iter_mut()
            .for_each(|line| desugar_module_accessors_comp_line(module_names, line)),
        syntax::Expr::Cmd(_) => {}
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
                for name in args.iter().flat_map(|pattern| pattern.item.iter_names()) {
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
                    for name in args.iter().flat_map(|pattern| pattern.item.iter_names()) {
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
        source: &Source,
        pos: usize,
        module_path: &ModulePath,
        common_kinds: &CommonKinds,
        builtins: &Module,
    ) -> Result<&'a core::Module, ModuleError> {
        match self.index.get(module_path) {
            None => {
                let path = module_path.as_path();
                if path.exists() {
                    let input_location = Source::File {
                        path: PathBuf::from(path),
                    };
                    let mut module = parse::parse_file(path)?;

                    for import_info in calculate_imports(path, &mut module).into_iter() {
                        if let Err(err) = self.import(
                            &Source::File {
                                path: PathBuf::from(path),
                            },
                            import_info.pos,
                            &import_info.module_path,
                            common_kinds,
                            builtins,
                        ) {
                            return Err(err);
                        }
                    }
                    let module = {
                        let working_dir = path.parent().unwrap();
                        let mut tc = {
                            let mut tc = Typechecker::new(
                                working_dir,
                                input_location,
                                common_kinds,
                                &self.index,
                            );
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
                        source: source.clone(),
                        pos,
                        module_path: module_path.clone(),
                    })
                }
            }
            Some(module_ref) => Ok(*module_ref),
        }
    }
}
