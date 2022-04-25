use diagnostic::{Location, Message};
use ipso_core::{self as core, CommonKinds};
use ipso_diagnostic::{self as diagnostic, Diagnostic, Source};
use ipso_parse as parse;
use ipso_syntax::{self as syntax};
use ipso_typecheck::{self as typecheck, Typechecker};
use ipso_util::hash_multi_set::HashMultiset;
use std::{
    collections::{HashMap, HashSet},
    io,
    path::{Path, PathBuf},
    rc::Rc,
};
use syntax::{desugar, ModuleId, ModuleKey, Modules};

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
    Desugar(desugar::Error),
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
            ModuleError::Desugar(err) => err.report(diagnostic),
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

impl From<desugar::Error> for ModuleError {
    fn from(err: desugar::Error) -> Self {
        ModuleError::Desugar(err)
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

fn rewrite_module_accessors_expr(
    exclude: &mut HashMultiset<Rc<str>>,
    imported_items: &HashMap<String, ImportedItemInfo>,
    expr: &mut syntax::Expr,
) {
    match expr {
        syntax::Expr::Var(name) => {
            if !exclude.contains(name.as_str()) {
                if let Some(imported_item_info) = imported_items.get(name) {
                    match imported_item_info {
                        ImportedItemInfo::DefinitionImportedFrom { id, path, .. } => {
                            /*
                            ```
                            from x import y

                            ...

                            y
                            ```

                            is rewritten to

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

                            Module item access is rewritten in the `Expr::Project` branch, rather than here.
                            */
                        }
                    }
                }
            }
        }
        syntax::Expr::Module { .. } => {}
        syntax::Expr::App(a, b) => {
            rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(a).item);
            rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(b).item);
        }
        syntax::Expr::Lam { args, body } => {
            let arg_names: Vec<Rc<str>> = args
                .iter()
                .flat_map(|pattern| pattern.item.iter_names())
                .map(|name| name.item.clone())
                .collect();

            exclude.insert_all(arg_names.iter().cloned());
            rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(body).item);
            exclude.remove_all(arg_names.into_iter());
        }
        syntax::Expr::Let { name, value, rest } => {
            rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(value).item);

            exclude.insert(name.clone());
            rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(rest).item);
            exclude.remove(name);
        }
        syntax::Expr::IfThenElse(a, b, c) => {
            rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(a).item);
            rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(b).item);
            rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(c).item);
        }
        syntax::Expr::Binop(_, a, b) => {
            rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(a).item);
            rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(b).item);
        }
        syntax::Expr::String(parts) => {
            for part in parts {
                match part {
                    syntax::StringPart::String(_) => {}
                    syntax::StringPart::Expr(e) => {
                        rewrite_module_accessors_expr(exclude, imported_items, &mut e.item);
                    }
                }
            }
        }
        syntax::Expr::Array(xs) => {
            for x in xs {
                rewrite_module_accessors_expr(exclude, imported_items, &mut x.item);
            }
        }
        syntax::Expr::Record { fields, rest } => {
            for (_, e) in fields {
                rewrite_module_accessors_expr(exclude, imported_items, &mut e.item);
            }

            for e in rest {
                rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(e).item);
            }
        }
        syntax::Expr::Project(value, field) => {
            let value = Rc::make_mut(value);
            rewrite_module_accessors_expr(exclude, imported_items, &mut value.item);

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

                syntax::Expr::Var(name) if !exclude.contains(name.as_str()) => {
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
        syntax::Expr::Embed(_, a) => {
            rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(a).item);
        }
        syntax::Expr::Case(a, branches) => {
            rewrite_module_accessors_expr(exclude, imported_items, &mut Rc::make_mut(a).item);

            for branch in branches {
                let arg_names: Vec<Rc<str>> = branch
                    .pattern
                    .item
                    .iter_names()
                    .map(|arg_name| arg_name.item.clone())
                    .collect();

                exclude.insert_all(arg_names.iter().cloned());
                rewrite_module_accessors_expr(exclude, imported_items, &mut branch.body.item);
                exclude.remove_all(arg_names.into_iter());
            }
        }
        syntax::Expr::Cmd(parts) => parts.iter_mut().for_each(|part| match part {
            syntax::CmdPart::Literal(_) => {}
            syntax::CmdPart::Expr(expr) => {
                rewrite_module_accessors_expr(exclude, imported_items, &mut expr.item)
            }
        }),
        syntax::Expr::Comp(_) => {
            /*
            Assuming computation expressions have been desugared means that
            we don't have to re-implement the shadowing rules. `Expr::Lam`
            does it for us.
            */
            panic!("computation expression not desugared")
        }
        syntax::Expr::True
        | syntax::Expr::False
        | syntax::Expr::Int(_)
        | syntax::Expr::Char(_)
        | syntax::Expr::Variant(_)
        | syntax::Expr::Unit => {}
    }
}

fn resolve_imports(
    common_kinds: &CommonKinds,
    modules: &mut Modules<core::Module>,
    builtins_module_id: ModuleId,
    working_dir: &Path,
    path: &Path,
    module: &mut syntax::Module,
) -> Result<(), ModuleError> {
    fn resolve_from_import_all(
        common_kinds: &CommonKinds,
        imported_items: &mut HashMap<String, ImportedItemInfo>,
        imported_module_id: ModuleId,
        imported_module: &core::Module,
    ) {
        imported_items.extend(
            imported_module
                .get_bindings(common_kinds)
                .into_keys()
                .filter_map(|name| match name {
                    core::Name::Definition(name) => Some((
                        name,
                        ImportedItemInfo::DefinitionImportedFrom {
                            id: imported_module_id,
                            path: vec![],
                        },
                    )),
                    /*
                    Only definitions are brought into scope. Evidence values
                    are internal to their respective modules.
                    */
                    core::Name::Evidence(_) => None,
                }),
        );
    }

    fn resolve_from_import(
        common_kinds: &CommonKinds,
        modules: &mut Modules<core::Module>,
        source: Source,
        imported_items: &mut HashMap<String, ImportedItemInfo>,
        imported_module_id: ModuleId,
        names: &syntax::Names,
    ) -> Result<(), ModuleError> {
        let imported_module = modules.lookup(imported_module_id);

        match names {
            syntax::Names::All => {
                resolve_from_import_all(
                    common_kinds,
                    imported_items,
                    imported_module_id,
                    imported_module,
                );

                Ok(())
            }
            syntax::Names::Names(names) => {
                imported_items.extend(names.iter().map(|name| {
                    (
                        name.item.clone(),
                        ImportedItemInfo::DefinitionImportedFrom {
                            id: imported_module_id,
                            path: vec![],
                        },
                    )
                }));

                let available_names: HashSet<String> = imported_module
                    .get_bindings(common_kinds)
                    .into_keys()
                    .filter_map(|name| match name {
                        core::Name::Evidence(_) => None,
                        core::Name::Definition(name) => Some(name),
                    })
                    .collect();

                names
                    .iter()
                    .try_for_each(|name| {
                        if available_names.contains(&name.item) {
                            Ok(())
                        } else {
                            Err(name)
                        }
                    })
                    .map_err(|name| ModuleError::DoesNotDefine {
                        source,
                        pos: name.pos,
                    })
            }
        }
    }

    let mut imported_items: HashMap<String, ImportedItemInfo> = HashMap::new();
    let mut exclude: HashMultiset<Rc<str>> = HashMultiset::new();

    // Resolve the builtins imports, as if the first declaration was `from builtins import *`.
    resolve_from_import_all(
        common_kinds,
        &mut imported_items,
        builtins_module_id,
        modules.lookup(builtins_module_id),
    );

    module
        .decls
        .iter_mut()
        .try_for_each(|decl| -> Result<_, ModuleError> {
            match &mut decl.item {
                syntax::Declaration::Import {
                    resolved,
                    module,
                    as_name,
                } => {
                    let id = import(
                        modules,
                        builtins_module_id,
                        &Source::File {
                            path: PathBuf::from(path),
                        },
                        module.pos,
                        &working_dir.join(&module.item).with_extension("ipso"),
                        common_kinds,
                    )?;

                    imported_items.insert(
                        as_name.as_ref().unwrap_or(module).item.clone(),
                        ImportedItemInfo::ModuleImportedAs { id },
                    );

                    *resolved = Some(id);

                    Ok(())
                }
                syntax::Declaration::FromImport {
                    resolved,
                    module,
                    names,
                } => {
                    let source = Source::File {
                        path: PathBuf::from(path),
                    };

                    let imported_module_id = import(
                        modules,
                        builtins_module_id,
                        &source,
                        module.pos,
                        &working_dir.join(&module.item).with_extension("ipso"),
                        common_kinds,
                    )?;

                    resolve_from_import(
                        common_kinds,
                        modules,
                        source,
                        &mut imported_items,
                        imported_module_id,
                        names,
                    )?;

                    *resolved = Some(imported_module_id);

                    Ok(())
                }

                syntax::Declaration::Definition {
                    name,
                    ty: _,
                    args,
                    body,
                } => {
                    exclude.insert(Rc::from(name.as_str()));

                    let arg_names: Vec<Rc<str>> = args
                        .iter()
                        .flat_map(|pattern| pattern.item.iter_names().map(|name| name.item.clone()))
                        .collect();

                    exclude.insert_all(arg_names.iter().cloned());
                    rewrite_module_accessors_expr(&mut exclude, &imported_items, &mut body.item);
                    exclude.remove_all(arg_names.into_iter());

                    Ok(())
                }
                syntax::Declaration::Instance { members, .. } => {
                    for (name, args, body) in members {
                        let to_exclude: Vec<Rc<str>> =
                            std::iter::once(Rc::from(name.item.as_str()))
                                .chain(args.iter().flat_map(|pattern| {
                                    pattern.item.iter_names().map(|name| name.item.clone())
                                }))
                                .collect();

                        exclude.insert_all(to_exclude.iter().cloned());
                        rewrite_module_accessors_expr(
                            &mut exclude,
                            &imported_items,
                            &mut body.item,
                        );
                        exclude.remove_all(to_exclude.into_iter());
                    }
                    Ok(())
                }

                syntax::Declaration::Class { name, members, .. } => {
                    exclude.insert(name.clone());
                    exclude.insert_all(members.iter().map(|(name, _)| Rc::from(name.as_str())));
                    Ok(())
                }
                syntax::Declaration::TypeAlias { name, .. } => {
                    exclude.insert(Rc::from(name.as_str()));
                    Ok(())
                }
            }
        })?;

    /*
    We insert the resolved `from builtins import *` because the type checker uses the imports in `decls`
    to decide which names will be in scope with which types.
    */
    module.decls.insert(
        0,
        syntax::Spanned {
            pos: 0,
            item: syntax::Declaration::FromImport {
                resolved: Some(builtins_module_id),
                module: syntax::Spanned {
                    pos: 0,
                    item: String::from("builtins"),
                },
                names: syntax::Names::All,
            },
        },
    );

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
    builtins_module_id: ModuleId,
    source: &Source,
    pos: usize,
    path: &Path,
    common_kinds: &CommonKinds,
) -> Result<ModuleId, ModuleError> {
    match modules.lookup_id(&ModuleKey::from(path)) {
        None => {
            if path.exists() {
                let input_location = Source::File {
                    path: PathBuf::from(path),
                };

                let module = parse::parse_file(path)?;
                let mut module = desugar::desugar_module(&input_location, module)?;

                let working_dir = path.parent().unwrap();

                resolve_imports(
                    common_kinds,
                    modules,
                    builtins_module_id,
                    working_dir,
                    path,
                    &mut module,
                )?;

                let module = Typechecker::new(input_location, common_kinds, modules)
                    .check_module(&module)?;
                let module_id: ModuleId = modules.insert(ModuleKey::from(path), module);

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
