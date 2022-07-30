//! Module checking.

use crate::constraint_solving::Sort;
use crate::{constraint_solving::Implication, declaration, Error};
use ipso_core::{self as core, CommonKinds};
use ipso_diagnostic::Source;
use ipso_syntax::{self as syntax, kind::Kind, ModuleId, ModuleRef, Modules};
use std::collections::HashMap;
use std::rc::Rc;

/// Module checking state.
pub struct State {
    pub implications: Vec<Implication>,
    pub type_context: HashMap<Rc<str>, Kind>,
    pub context: HashMap<String, core::Signature>,
    pub class_context: HashMap<Rc<str>, core::ClassDeclaration>,
    pub module_context: HashMap<ModuleId, HashMap<String, core::Signature>>,
    pub decls: Vec<core::Declaration>,
}

impl State {
    pub fn new() -> Self {
        State {
            implications: Vec::new(),
            type_context: HashMap::new(),
            context: HashMap::new(),
            class_context: HashMap::new(),
            module_context: HashMap::new(),
            decls: Vec::new(),
        }
    }

    /// Finish a module checking session, returning all the checked declarations.
    pub fn finish(self) -> Vec<core::Declaration> {
        self.decls
    }

    pub fn add_declaration(&mut self, common_kinds: &CommonKinds, decl: declaration::Checked) {
        match decl {
            declaration::Checked::Definition { name, sig, body } => {
                register_definition(&mut self.context, &name, &sig);
                self.decls
                    .push(core::Declaration::Definition { name, sig, body })
            }

            declaration::Checked::ResolvedImport { module_id, module } => {
                self.module_context
                    .insert(module_id, module.get_signatures(common_kinds));
                module.decls.iter().for_each(|decl| {
                    if let core::Declaration::Instance {
                        ty_vars,
                        assumes,
                        head,
                        evidence,
                        ..
                    } = decl
                    {
                        self.import_instance(module_id, ty_vars, assumes, head, evidence.clone())
                    }
                });
            }

            declaration::Checked::Class(class_decl) => {
                register_class(
                    common_kinds,
                    &mut self.type_context,
                    &mut self.implications,
                    &mut self.context,
                    &mut self.class_context,
                    &class_decl,
                );
                self.decls.push(core::Declaration::Class(class_decl))
            }

            declaration::Checked::Instance {
                evidence_name,
                evidence_body,
                instance_ty_vars,
                instance_assumes,
                instance_head,
                instance_evidence,
            } => {
                register_instance(
                    &mut self.implications,
                    None,
                    &instance_ty_vars,
                    &instance_assumes,
                    &instance_head,
                    instance_evidence.clone(),
                );

                self.decls.push(core::Declaration::Evidence {
                    name: evidence_name,
                    body: evidence_body,
                });
                self.decls.push(core::Declaration::Instance {
                    ty_vars: instance_ty_vars,
                    assumes: instance_assumes,
                    head: instance_head,
                    evidence: instance_evidence,
                });
            }
        }
    }

    /**
    Import a declaration from another module.

    ## Arguments

    * `module_id` - Module that contains the declaration.

    */
    pub fn import_declaration(
        &mut self,
        common_kinds: &CommonKinds,
        module_id: ModuleId,
        decl: &core::Declaration,
    ) {
        match decl {
            core::Declaration::BuiltinType { name, kind } => {
                self.import_builtin_type(name, kind);
            }
            core::Declaration::Definition { name, sig, .. } => self.import_definition(name, sig),
            core::Declaration::Module { name, decls, .. } => {
                self.import_module(common_kinds, name, decls)
            }
            core::Declaration::TypeAlias { name, args, body } => {
                self.import_type_alias(name, args, body)
            }
            core::Declaration::Class(decl) => self.import_class(common_kinds, decl),
            core::Declaration::Evidence { .. } => {}
            core::Declaration::Instance {
                ty_vars,
                assumes,
                head,
                evidence,
                ..
            } => self.import_instance(module_id, ty_vars, assumes, head, evidence.clone()),
        }
    }

    pub fn import_class(&mut self, common_kinds: &CommonKinds, decl: &core::ClassDeclaration) {
        register_class(
            common_kinds,
            &mut self.type_context,
            &mut self.implications,
            &mut self.context,
            &mut self.class_context,
            decl,
        )
    }

    pub fn import_instance(
        &mut self,
        module_id: ModuleId,
        ty_vars: &[(Rc<str>, Kind)],
        assumes: &[core::Type],
        head: &core::Type,
        evidence_name: Rc<str>,
    ) {
        register_instance(
            &mut self.implications,
            Some(module_id),
            ty_vars,
            assumes,
            head,
            evidence_name,
        )
    }

    pub fn import_builtin_type(&mut self, name: &str, kind: &Kind) {
        register_builtin_type(&mut self.type_context, name, kind)
    }

    pub fn import_definition(&mut self, name: &str, sig: &core::TypeSig) {
        register_definition(&mut self.context, name, sig)
    }

    pub fn import_module(
        &mut self,
        common_kinds: &CommonKinds,
        name: &str,
        decls: &[Rc<core::Declaration>],
    ) {
        register_module(common_kinds, &mut self.context, name, decls)
    }

    pub fn import_type_alias(&mut self, name: &str, args: &[Kind], body: &core::Type) {
        todo!("import TypeAlias {:?}", (name, args, body))
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

pub fn check(
    common_kinds: &CommonKinds,
    modules: &Modules<core::Module>,
    source: &Source,
    module: &syntax::Module,
) -> Result<core::Module, Error> {
    let mut state = State::new();

    module.decls.iter().try_for_each(|decl| {
        declaration::check(
            declaration::Env {
                common_kinds,
                modules,
                module_context: &state.module_context,
                type_context: &state.type_context,
                class_context: &state.class_context,
                context: &state.context,
                implications: &state.implications,
                source,
            },
            decl,
        )
        .map(|checked| state.add_declaration(common_kinds, checked))
    })?;

    let decls = state.finish();

    Ok(core::Module { decls })
}

#[allow(clippy::too_many_arguments)]
pub fn register_from_import(
    common_kinds: &CommonKinds,
    implications: &mut Vec<Implication>,
    type_context: &mut HashMap<Rc<str>, Kind>,
    context: &mut HashMap<String, core::Signature>,
    class_context: &mut HashMap<Rc<str>, core::ClassDeclaration>,
    modules: &Modules<core::Module>,
    module_id: ModuleId,
    names: &syntax::Names,
) {
    let module = modules.lookup(module_id);

    let should_import = |expected_name: &str| -> bool {
        match names {
            syntax::Names::All => true,
            syntax::Names::Names(names) => names.iter().any(|name| name.item == expected_name),
        }
    };

    for decl in &module.decls {
        match decl {
            core::Declaration::BuiltinType { name, kind } => {
                if should_import(name) {
                    register_builtin_type(type_context, name, kind);
                }
            }
            core::Declaration::Definition { name, sig, .. } => {
                if should_import(name) {
                    register_definition(context, name, sig);
                }
            }
            core::Declaration::Module { name, decls, .. } => {
                if should_import(name) {
                    register_module(common_kinds, context, name, decls);
                }
            }
            core::Declaration::TypeAlias { name, args, body } => {
                if should_import(name) {
                    register_type_alias(name, args, body);
                }
            }
            core::Declaration::Class(class_decl) => {
                if should_import(class_decl.name.as_ref()) {
                    register_class(
                        common_kinds,
                        type_context,
                        implications,
                        context,
                        class_context,
                        class_decl,
                    );
                }
            }
            core::Declaration::Evidence { .. } => {}
            core::Declaration::Instance {
                ty_vars,
                assumes,
                head,
                evidence,
                ..
            } => {
                register_instance(
                    implications,
                    Some(module_id),
                    ty_vars,
                    assumes,
                    head,
                    evidence.clone(),
                );
            }
        }
    }
}

pub fn register_class(
    common_kinds: &CommonKinds,
    type_context: &mut HashMap<Rc<str>, Kind>,
    implications: &mut Vec<Implication>,
    context: &mut HashMap<String, core::Signature>,
    class_context: &mut HashMap<Rc<str>, core::ClassDeclaration>,
    decl: &core::ClassDeclaration,
) {
    let decl_name: Rc<str> = Rc::from(decl.name.as_ref());
    let decl_name_kind = decl
        .args
        .iter()
        .rev()
        .fold(Kind::Constraint, |acc, (_, arg_kind)| {
            Kind::mk_arrow(arg_kind, &acc)
        });
    let decl_ty = core::Type::unsafe_mk_name(decl_name.clone(), decl_name_kind);

    // generate constraint's kind
    let mut constraint_kind = Kind::Constraint;
    for (_, kind) in decl.args.iter().rev() {
        constraint_kind = Kind::mk_arrow(kind, &Kind::Constraint);
    }
    type_context.insert(decl_name.clone(), constraint_kind);

    // generate superclass accessors
    let applied_type =
        decl.args
            .iter()
            .enumerate()
            .fold(decl_ty, |acc, (arg_index, (_, arg_kind))| {
                core::Type::app(acc, core::Type::unsafe_mk_var(arg_index, arg_kind.clone()))
            });
    implications.extend(
        decl.supers
            .iter()
            .enumerate()
            .map(|(pos, superclass)| Implication {
                sort: Sort::Class,
                ty_vars: decl.args.iter().map(|(_, kind)| kind.clone()).collect(),
                antecedents: vec![applied_type.clone()],
                consequent: superclass.clone(),
                evidence: Rc::new(core::Expr::mk_lam(
                    true,
                    core::Expr::mk_project(core::Expr::Var(0), core::Expr::Int(pos as i32)),
                )),
            }),
    );

    // generate class members
    let decl_bindings = decl.get_bindings(common_kinds);
    context.extend(
        decl_bindings
            .iter()
            .map(|(name, (sig, _))| (name.clone(), core::Signature::TypeSig(sig.clone()))),
    );

    // update class context
    class_context.insert(decl_name, decl.clone());
}

pub fn register_instance(
    implications: &mut Vec<Implication>,
    module_id: Option<ModuleId>,
    ty_vars: &[(Rc<str>, Kind)],
    assumes: &[core::Type],
    head: &core::Type,
    evidence_name: Rc<str>,
) {
    implications.push(Implication {
        sort: Sort::Instance,
        ty_vars: ty_vars.iter().map(|(_, a)| a.clone()).collect(),
        antecedents: Vec::from(assumes),
        consequent: head.clone(),
        evidence: Rc::new(match module_id {
            Some(module_id) => core::Expr::Module {
                id: ModuleRef::from(module_id),
                path: vec![],
                item: core::Name::Evidence(evidence_name),
            },
            None => core::Expr::Name(core::Name::Evidence(evidence_name)),
        }),
    });
}

pub fn register_builtin_type(type_context: &mut HashMap<Rc<str>, Kind>, name: &str, kind: &Kind) {
    type_context.insert(Rc::from(name), kind.clone());
}

pub fn register_definition(
    context: &mut HashMap<String, core::Signature>,
    name: &str,
    sig: &core::TypeSig,
) {
    context.insert(String::from(name), core::Signature::TypeSig(sig.clone()));
}

pub fn register_module(
    common_kinds: &CommonKinds,
    context: &mut HashMap<String, core::Signature>,
    name: &str,
    decls: &[Rc<core::Declaration>],
) {
    context.insert(
        String::from(name),
        core::Signature::Module(
            decls
                .iter()
                .flat_map(|decl| decl.get_signatures(common_kinds))
                .collect(),
        ),
    );
}

pub fn register_type_alias(name: &str, args: &[Kind], body: &core::Type) {
    todo!("register TypeAlias {:?}", (name, args, body))
}
