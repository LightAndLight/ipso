use diagnostic::{Location, Message};
use ipso_core::{self as core, CommonKinds, Module};
use ipso_diagnostic::{self as diagnostic, Diagnostic, Source};
use ipso_parse as parse;
use ipso_syntax::{self as syntax};
use ipso_typecheck::{self as typecheck, Typechecker};
use std::{
    collections::{HashMap, HashSet},
    io::{self},
    path::{Path, PathBuf},
    rc::Rc,
};
use syntax::{ModuleId, Modules};

#[derive(Debug)]
pub enum ModuleError {
    NotFound {
        source: Source,
        pos: usize,
        module_path: PathBuf,
    },
    DoesNotDefine {
        source: Source,
        pos: usize,
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
                    addendum: Some(format!("file {} does not exist", module_path.display())),
                },
            ),
            ModuleError::DoesNotDefine { source, pos } => diagnostic.item(
                Some(Location {
                    source: source.clone(),
                    offset: Some(*pos),
                }),
                Message {
                    content: String::from("not defined in module"),
                    addendum: None,
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
        id: ModuleId,

        /**
        Any submodule references.

        e.g. in `from x.y import z`, `.y` is a submodule reference.
        */
        path: Vec<String>,
    },

    /**
    A module was imported.

    e.g. `import x`, `import x as y`
    */
    ModuleImportedAs { id: ModuleId },
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
                    ImportedItemInfo::DefinitionImportedFrom { id, path, .. } => {
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
                            id: *id,
                            path: path.clone(),
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
                syntax::Expr::Module { id, path, item } => {
                    let mut path = path.clone();
                    path.push(item.clone());

                    *expr = syntax::Expr::Module {
                        id: *id,
                        path,
                        item: field.clone(),
                    };
                }

                syntax::Expr::Var(name) => {
                    if let Some(ImportedItemInfo::ModuleImportedAs { id }) =
                        imported_items.get(name)
                    {
                        *expr = syntax::Expr::Module {
                            id: *id,
                            path: vec![],
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

        syntax::Declaration::Import { .. } => panic!("unresolved Import"),
        syntax::Declaration::FromImport { .. } => panic!("unresolved FromImport"),

        syntax::Declaration::ResolvedImport { .. }
        | syntax::Declaration::ResolvedFromImport { .. } => {}
    }
}

fn desugar_module_accessors(
    common_kinds: &CommonKinds,
    modules: &Modules<core::Module>,
    module: &mut syntax::Module,
) {
    let mut imported_items: HashMap<String, ImportedItemInfo> = HashMap::new();
    for decl in &mut module.decls {
        match &decl.item {
            syntax::Declaration::Definition { .. }
            | syntax::Declaration::Class { .. }
            | syntax::Declaration::Instance { .. }
            | syntax::Declaration::TypeAlias { .. } => {}
            syntax::Declaration::Import { .. } => panic!("unresolved Import"),
            syntax::Declaration::FromImport { .. } => panic!("unresolved FromImport"),
            syntax::Declaration::ResolvedImport { id, module, .. } => {
                imported_items.insert(
                    module.item.clone(),
                    ImportedItemInfo::ModuleImportedAs { id: *id },
                );
            }
            syntax::Declaration::ResolvedFromImport { id, names, .. } => {
                let module = modules.lookup(*id);

                match names {
                    syntax::Names::All => {
                        module
                            .get_bindings(common_kinds)
                            .into_keys()
                            .for_each(|name| {
                                imported_items.insert(
                                    name,
                                    ImportedItemInfo::DefinitionImportedFrom {
                                        id: *id,
                                        path: vec![],
                                    },
                                );
                            })
                    }
                    syntax::Names::Names(names) => {
                        for name in names {
                            imported_items.insert(
                                name.item.clone(),
                                ImportedItemInfo::DefinitionImportedFrom {
                                    id: *id,
                                    path: vec![],
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

fn resolve_imports(
    common_kinds: &CommonKinds,
    builtins: &Module,
    modules: &mut Modules<core::Module>,
    working_dir: &Path,
    path: &Path,
    module: &mut syntax::Module,
) -> Result<(), ModuleError> {
    let decls: Vec<syntax::Spanned<syntax::Declaration>> = module
        .decls
        .iter()
        .map(|decl| -> Result<_, ModuleError> {
            match &decl.item {
                syntax::Declaration::Import { module, as_name } => {
                    let source = &Source::File {
                        path: PathBuf::from(path),
                    };
                    let module_path = working_dir.join(&module.item).with_extension("ipso");
                    let id = import(
                        modules,
                        source,
                        module.pos,
                        &module_path,
                        common_kinds,
                        builtins,
                    )?;

                    Ok(syntax::Spanned {
                        pos: decl.pos,
                        item: syntax::Declaration::ResolvedImport {
                            id,
                            module: module.clone(),
                            as_name: as_name.clone(),
                        },
                    })
                }
                syntax::Declaration::FromImport { module, names } => {
                    let source = &Source::File {
                        path: PathBuf::from(path),
                    };
                    let importing_module_path =
                        working_dir.join(&module.item).with_extension("ipso");

                    let imported_module_id = import(
                        modules,
                        source,
                        module.pos,
                        &importing_module_path,
                        common_kinds,
                        builtins,
                    )?;
                    let imported_module = modules.lookup(imported_module_id);

                    match names {
                        syntax::Names::All => Ok::<(), ModuleError>(()),
                        syntax::Names::Names(names) => {
                            let available_names: HashSet<String> = imported_module
                                .get_bindings(common_kinds)
                                .into_keys()
                                .collect();

                            names.iter().try_for_each(|name| {
                                if available_names.contains(&name.item) {
                                    Ok(())
                                } else {
                                    Err(ModuleError::DoesNotDefine {
                                        source: source.clone(),
                                        pos: name.pos,
                                    })
                                }
                            })
                        }
                    }?;

                    Ok(syntax::Spanned {
                        pos: decl.pos,
                        item: syntax::Declaration::ResolvedFromImport {
                            id: imported_module_id,
                            module: module.clone(),
                            names: names.clone(),
                        },
                    })
                }
                _ => Ok(decl.clone()),
            }
        })
        .collect::<Result<_, _>>()?;

    module.decls = decls;
    Ok(())
}

/// Import a module.
///
/// Module imports are cached, so importing the same module repeatedly is cheap.
///
/// * `source` - source file location for error reporting
/// * `pos` - source file offset for error reporting
/// * `path` - file path to import
pub fn import(
    modules: &mut Modules<core::Module>,
    source: &Source,
    pos: usize,
    path: &Path,
    common_kinds: &CommonKinds,
    builtins: &Module,
) -> Result<ModuleId, ModuleError> {
    match modules.lookup_id(path) {
        None => {
            if path.exists() {
                let input_location = Source::File {
                    path: PathBuf::from(path),
                };
                let mut module = parse::parse_file(path)?;

                let working_dir = path.parent().unwrap();

                resolve_imports(
                    common_kinds,
                    builtins,
                    modules,
                    working_dir,
                    path,
                    &mut module,
                )?;
                desugar_module_accessors(common_kinds, modules, &mut module);

                let module = {
                    let working_dir = path.parent().unwrap();
                    let mut tc = {
                        let mut tc =
                            Typechecker::new(working_dir, input_location, common_kinds, modules);
                        tc.register_from_import(builtins, &syntax::Names::All);
                        tc
                    };
                    tc.check_module(&module)
                }?;
                let module_id: ModuleId = modules.insert(path.to_path_buf(), module);
                Ok(module_id)
            } else {
                Err(ModuleError::NotFound {
                    source: source.clone(),
                    pos,
                    module_path: path.to_path_buf(),
                })
            }
        }
        Some(module_id) => Ok(module_id),
    }
}
