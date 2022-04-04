use diagnostic::{Location, Message};
use ipso_core::{self as core, CommonKinds, Module};
use ipso_diagnostic::{self as diagnostic, Diagnostic, Source};
use ipso_parse as parse;
use ipso_syntax::{self as syntax, ModuleName, ModulePath};
use ipso_typecheck::{self as typecheck, Typechecker};
use std::{
    collections::HashMap,
    io::{self},
    path::{Path, PathBuf},
    rc::Rc,
};

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
                    addendum: Some(format!(
                        "file {} does not exist",
                        module_path.path().display()
                    )),
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

#[derive(Debug, Clone)]
enum ImportedItemInfo {
    /**
    A definition was imported from a specific module.

    e.g. `from x import y`
    */
    DefinitionImportedFrom {
        /// The module's file path.
        path: PathBuf,

        /**
        Any submodule references.

        e.g. in `from x.y import z`, `.y` is a submodule reference.
        */
        submodules: Vec<String>,
    },

    /**
    A module was imported.

    e.g. `import x`, `import x as y`
    */
    ModuleImportedAs {
        /// The module's file path.
        path: PathBuf,
    },
}

fn desugar_module_accessors_comp_line(
    imported_items: &HashMap<String, ImportedItemInfo>,
    line: &mut syntax::CompLine,
) {
    match line {
        syntax::CompLine::Expr(value) => {
            desugar_module_accessors_expr(imported_items, &mut value.item);
        }
        syntax::CompLine::Bind(_, value) => {
            desugar_module_accessors_expr(imported_items, &mut value.item);
        }
        syntax::CompLine::Let(_, value) => {
            desugar_module_accessors_expr(imported_items, &mut value.item);
        }
    }
}

fn desugar_module_accessors_expr(
    imported_items: &HashMap<String, ImportedItemInfo>,
    expr: &mut syntax::Expr,
) {
    match expr {
        syntax::Expr::Var(name) => {
            if let Some(imported_item_info) = imported_items.get(name) {
                match imported_item_info {
                    ImportedItemInfo::DefinitionImportedFrom {
                        path, submodules, ..
                    } => {
                        /*
                        ```
                        from x import y

                        ...

                        y
                        ```

                        is desugared to

                        ```
                        import x

                        ...

                        <x's path>.y
                        ```
                        */
                        *expr = syntax::Expr::Module {
                            path: ModulePath {
                                file: path.clone(),
                                submodules: submodules.clone(),
                            },
                            item: name.clone(),
                        };
                    }
                    ImportedItemInfo::ModuleImportedAs { .. } => {
                        /*
                        A module can only be used as an expression when an item is projected from it.

                        i.e.

                        ```
                        import x

                        ...

                        x
                        ```

                        is not allowed, and

                        ```
                        import x

                        ...


                        x.y
                        ```

                        is allowed.

                        Module item access is desugared in the `Expr::Project` branch, rather than here.
                        */
                    }
                }
            }
        }
        syntax::Expr::Module { .. } => {}
        syntax::Expr::App(a, b) => {
            desugar_module_accessors_expr(imported_items, &mut Rc::make_mut(a).item);
            desugar_module_accessors_expr(imported_items, &mut Rc::make_mut(b).item);
        }
        syntax::Expr::Lam { args, body } => {
            let imported_items = {
                let mut imported_items: HashMap<String, ImportedItemInfo> = imported_items.clone();
                for name in args.iter().flat_map(|pattern| pattern.item.iter_names()) {
                    imported_items.remove(&name.item);
                }
                imported_items
            };
            desugar_module_accessors_expr(&imported_items, &mut Rc::make_mut(body).item)
        }
        syntax::Expr::Let { value, rest, .. } => {
            desugar_module_accessors_expr(imported_items, &mut Rc::make_mut(value).item);

            /*
            TODO: fix variable shadowing

            ```
            import x

            ...


            let x = {} in x.a
            ```

            In the above example, `x.a` should be a record projection rather than a module access.
            */
            desugar_module_accessors_expr(imported_items, &mut Rc::make_mut(rest).item);
        }
        syntax::Expr::True => {}
        syntax::Expr::False => {}
        syntax::Expr::IfThenElse(a, b, c) => {
            desugar_module_accessors_expr(imported_items, &mut Rc::make_mut(a).item);
            desugar_module_accessors_expr(imported_items, &mut Rc::make_mut(b).item);
            desugar_module_accessors_expr(imported_items, &mut Rc::make_mut(c).item);
        }
        syntax::Expr::Int(_) => {}
        syntax::Expr::Binop(_, a, b) => {
            desugar_module_accessors_expr(imported_items, &mut Rc::make_mut(a).item);
            desugar_module_accessors_expr(imported_items, &mut Rc::make_mut(b).item);
        }
        syntax::Expr::Char(_) => {}
        syntax::Expr::String(parts) => {
            for part in parts {
                match part {
                    syntax::StringPart::String(_) => {}
                    syntax::StringPart::Expr(e) => {
                        desugar_module_accessors_expr(imported_items, &mut e.item);
                    }
                }
            }
        }
        syntax::Expr::Array(xs) => {
            for x in xs {
                desugar_module_accessors_expr(imported_items, &mut x.item);
            }
        }
        syntax::Expr::Record { fields, rest } => {
            for (_, e) in fields {
                desugar_module_accessors_expr(imported_items, &mut e.item);
            }

            for e in rest {
                desugar_module_accessors_expr(imported_items, &mut Rc::make_mut(e).item);
            }
        }
        syntax::Expr::Project(value, field) => {
            let value = Rc::make_mut(value);
            desugar_module_accessors_expr(imported_items, &mut value.item);

            match &value.item {
                syntax::Expr::Module { path, item } => {
                    let mut path = path.clone();
                    path.submodules.push(item.clone());

                    *expr = syntax::Expr::Module {
                        path,
                        item: field.clone(),
                    };
                }

                syntax::Expr::Var(name) => {
                    if let Some(ImportedItemInfo::ModuleImportedAs { path }) =
                        imported_items.get(name)
                    {
                        *expr = syntax::Expr::Module {
                            path: ModulePath {
                                file: path.clone(),
                                submodules: vec![],
                            },
                            item: field.clone(),
                        }
                    }
                }
                _ => {}
            }
        }
        syntax::Expr::Variant(_) => {}
        syntax::Expr::Embed(_, a) => {
            desugar_module_accessors_expr(imported_items, &mut Rc::make_mut(a).item);
        }
        syntax::Expr::Case(a, branches) => {
            desugar_module_accessors_expr(imported_items, &mut Rc::make_mut(a).item);

            for branch in branches {
                let module_names = {
                    // TODO: replace this loop with filter_map/collect
                    let mut module_names = imported_items.clone();
                    for name in branch.pattern.item.iter_names() {
                        module_names.remove(&name.item);
                    }
                    module_names
                };
                desugar_module_accessors_expr(&module_names, &mut branch.body.item);
            }
        }
        syntax::Expr::Unit => {}
        syntax::Expr::Comp(lines) => lines
            .iter_mut()
            .for_each(|line| desugar_module_accessors_comp_line(imported_items, line)),
        syntax::Expr::Cmd(parts) => parts.iter_mut().for_each(|part| match part {
            syntax::CmdPart::Literal(_) => {}
            syntax::CmdPart::Expr(expr) => {
                desugar_module_accessors_expr(imported_items, &mut expr.item)
            }
        }),
    }
}

fn desugar_module_accessors_decl(
    imported_items: &HashMap<String, ImportedItemInfo>,
    decl: &mut syntax::Declaration,
) {
    match decl {
        syntax::Declaration::Definition {
            name,
            ty: _,
            args,
            body,
        } => {
            let imported_items = {
                let mut imported_items = imported_items.clone();
                imported_items.remove(name);
                for name in args.iter().flat_map(|pattern| pattern.item.iter_names()) {
                    imported_items.remove(&name.item);
                }
                imported_items
            };
            desugar_module_accessors_expr(&imported_items, &mut body.item);
        }
        syntax::Declaration::Class { .. } => {}
        syntax::Declaration::Instance {
            assumes: _,
            name: _,
            args: _,
            members,
        } => {
            for (name, args, body) in members {
                let imported_items = {
                    let mut imported_items = imported_items.clone();
                    imported_items.remove(&name.item);
                    for name in args.iter().flat_map(|pattern| pattern.item.iter_names()) {
                        imported_items.remove(&name.item);
                    }
                    imported_items
                };
                desugar_module_accessors_expr(&imported_items, &mut body.item)
            }
        }
        syntax::Declaration::TypeAlias { .. } => {}
        syntax::Declaration::Import { .. } => {}
        syntax::Declaration::FromImport { .. } => {}
    }
}

fn desugar_module_accessors(
    common_kinds: &CommonKinds,
    modules: &core::Modules,
    module: &mut syntax::Module,
    working_dir: &Path,
) {
    let mut imported_items: HashMap<String, ImportedItemInfo> = HashMap::new();
    for decl in &mut module.decls {
        match &decl.item {
            syntax::Declaration::Definition { .. }
            | syntax::Declaration::Class { .. }
            | syntax::Declaration::Instance { .. }
            | syntax::Declaration::TypeAlias { .. } => {}
            syntax::Declaration::Import { module, .. } => {
                let module_path =
                    ModulePath::from_module(working_dir, &ModuleName(vec![module.item.clone()]));
                imported_items.insert(
                    module.item.clone(),
                    ImportedItemInfo::ModuleImportedAs {
                        path: PathBuf::from(module_path.path()),
                    },
                );
            }
            syntax::Declaration::FromImport { module, names } => {
                let module_path =
                    ModulePath::from_module(working_dir, &ModuleName(vec![module.item.clone()]));
                let path = PathBuf::from(module_path.path());

                let module = modules.lookup(&module_path).unwrap_or_else(|| {
                    panic!(
                        "impossible: module {} ({}) is missing",
                        module.item,
                        path.display()
                    )
                });
                match names {
                    syntax::Names::All => {
                        module
                            .get_bindings(common_kinds)
                            .into_keys()
                            .for_each(|name| {
                                imported_items.insert(
                                    name,
                                    ImportedItemInfo::DefinitionImportedFrom {
                                        path: path.clone(),
                                        submodules: vec![],
                                    },
                                );
                            })
                    }
                    syntax::Names::Names(names) => {
                        for name in names {
                            imported_items.insert(
                                name.clone(),
                                ImportedItemInfo::DefinitionImportedFrom {
                                    path: path.clone(),
                                    submodules: vec![],
                                },
                            );
                        }
                    }
                }
            }
        }
    }

    for decl in &mut module.decls {
        desugar_module_accessors_decl(&imported_items, &mut decl.item);
    }
}

/// Import a module.
///
/// Module imports are cached, so importing the same module repeatedly is cheap.
///
/// * `location` - source file location for error reporting
/// * `pos` - source file offset for error reporting
pub fn import<'a>(
    modules: &mut core::Modules<'a>,
    source: &Source,
    pos: usize,
    module_path: &ModulePath,
    common_kinds: &CommonKinds,
    builtins: &Module,
) -> Result<&'a core::Module, ModuleError> {
    match modules.index.get(module_path) {
        None => {
            let path = module_path.path();
            if path.exists() {
                let input_location = Source::File {
                    path: PathBuf::from(path),
                };
                let mut module = parse::parse_file(path)?;

                let working_dir = path.parent().unwrap();

                enum CheckImport<'a> {
                    Import {
                        module: &'a syntax::Spanned<String>,
                    },
                    FromImport {
                        module: &'a syntax::Spanned<String>,
                        names: &'a syntax::Names,
                    },
                }
                module
                    .decls
                    .iter()
                    .filter_map(|decl| match &decl.item {
                        syntax::Declaration::Definition { .. }
                        | syntax::Declaration::Class { .. }
                        | syntax::Declaration::Instance { .. }
                        | syntax::Declaration::TypeAlias { .. } => None,
                        syntax::Declaration::Import { module, .. } => {
                            Some(CheckImport::Import { module })
                        }
                        syntax::Declaration::FromImport { module, names } => {
                            Some(CheckImport::FromImport { module, names })
                        }
                    })
                    .try_for_each(|filtered_item| match filtered_item {
                        CheckImport::Import { module } => {
                            let module_path = ModulePath::from_module(
                                working_dir,
                                &ModuleName(vec![module.item.clone()]),
                            );
                            let _ = import(
                                modules,
                                &Source::File {
                                    path: PathBuf::from(path),
                                },
                                module.pos,
                                &module_path,
                                common_kinds,
                                builtins,
                            )?;

                            Ok::<(), ModuleError>(())
                        }
                        CheckImport::FromImport { module, names } => {
                            let module_path = ModulePath::from_module(
                                working_dir,
                                &ModuleName(vec![module.item.clone()]),
                            );

                            let _ = import(
                                modules,
                                &Source::File {
                                    path: PathBuf::from(path),
                                },
                                module.pos,
                                &module_path,
                                common_kinds,
                                builtins,
                            )?;

                            match names {
                                syntax::Names::All => Ok::<(), ModuleError>(()),
                                syntax::Names::Names(names) => names.iter().try_for_each(|name| {
                                    todo!("check that each name exists");
                                }),
                            }?;

                            Ok(())
                        }
                    })?;

                desugar_module_accessors(common_kinds, modules, &mut module, working_dir);

                let module = {
                    let working_dir = path.parent().unwrap();
                    let mut tc = {
                        let mut tc = Typechecker::new(
                            working_dir,
                            input_location,
                            common_kinds,
                            &modules.index,
                        );
                        tc.register_from_import(builtins, &syntax::Names::All);
                        tc
                    };
                    tc.check_module(&module)
                }?;
                let module_ref: &core::Module = modules.insert(module_path, module);
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
