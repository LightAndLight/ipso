use crate::{check_declaration, Declarations};
use crate::{Implication, TypeError};
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
}

impl State {
    pub fn new() -> Self {
        State {
            implications: Vec::new(),
            type_context: HashMap::new(),
            context: HashMap::new(),
            class_context: HashMap::new(),
        }
    }

    pub fn register_declaration(
        &mut self,
        common_kinds: &CommonKinds,
        module_id: Option<ModuleId>,
        decl: &core::Declaration,
    ) {
        match decl {
            core::Declaration::BuiltinType { name, kind } => {
                register_builtin_type(&mut self.type_context, name, kind);
            }
            core::Declaration::Definition { name, sig, .. } => {
                register_definition(&mut self.context, name, sig)
            }
            core::Declaration::Module { name, decls, .. } => {
                register_module(common_kinds, &mut self.context, name, decls)
            }
            core::Declaration::TypeAlias { name, args, body } => {
                register_type_alias(name, args, body)
            }
            core::Declaration::Class(decl) => register_class(
                common_kinds,
                &mut self.type_context,
                &mut self.implications,
                &mut self.context,
                &mut self.class_context,
                decl,
            ),
            core::Declaration::Evidence { .. } => {}
            core::Declaration::Instance {
                ty_vars,
                assumes,
                head,
                evidence,
                ..
            } => register_instance(
                &mut self.implications,
                module_id,
                ty_vars,
                assumes,
                head,
                evidence.clone(),
            ),
        }
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
) -> Result<core::Module, TypeError> {
    let mut module_context = HashMap::new();
    let mut state = State::new();

    let decls = module.decls.iter().fold(Ok(vec![]), |acc, decl| {
        acc.and_then(|mut decls| {
            check_declaration(
                common_kinds,
                &mut state.implications,
                &mut state.type_context,
                &mut state.context,
                &state.class_context,
                modules,
                &mut module_context,
                source,
                decl,
            )
            .map(|checked_decls| match checked_decls {
                Declarations::Zero => decls,
                Declarations::One(decl) => {
                    state.register_declaration(common_kinds, None, &decl);
                    decls.push(decl);
                    decls
                }
                Declarations::Two(decl1, decl2) => {
                    state.register_declaration(common_kinds, None, &decl1);
                    decls.push(decl1);
                    state.register_declaration(common_kinds, None, &decl2);
                    decls.push(decl2);
                    decls
                }
            })
        })
    })?;
    Ok(core::Module { decls })
}

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
                ty_vars: decl.args.iter().map(|(_, kind)| kind.clone()).collect(),
                antecedents: vec![applied_type.clone()],
                consequent: superclass.clone(),
                evidence: Rc::new(core::Expr::mk_lam(
                    true,
                    core::Expr::mk_project(core::Expr::Var(0), core::Expr::Int(pos as u32)),
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