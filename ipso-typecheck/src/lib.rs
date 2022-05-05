#[cfg(test)]
mod test;

pub mod evidence;
pub mod kind_inference;
pub mod metavariables;
pub mod type_inference;

use diagnostic::{Location, Message};
use evidence::{
    solver::{self, solve_placeholder},
    Constraint, Evidence,
};
use ipso_core::{self as core, CommonKinds};
use ipso_diagnostic::{self as diagnostic, Source};
use ipso_syntax::{self as syntax, kind::Kind, ModuleRef, Spanned};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    rc::Rc,
    todo,
};
use syntax::{ModuleId, Modules};
use type_inference::infer_pattern;

#[derive(Debug, PartialEq, Eq)]
enum Declarations {
    Zero,
    One(core::Declaration),
    Two(core::Declaration, core::Declaration),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BoundVars<A> {
    indices: HashMap<Rc<str>, Vec<usize>>,
    info: Vec<(Rc<str>, A)>,
}

impl<A> BoundVars<A> {
    pub fn new() -> Self {
        BoundVars {
            indices: HashMap::new(),
            info: Vec::new(),
        }
    }

    fn lookup_name(&self, name: &str) -> Option<(usize, &A)> {
        self.indices
            .get(name)
            .and_then(|entries| entries.last())
            .and_then(|&ix| self.lookup_index(ix).map(|(_, item)| (ix, item)))
    }

    fn lookup_index(&self, ix: usize) -> Option<&(Rc<str>, A)> {
        self.info.get(self.info.len() - 1 - ix)
    }

    fn insert(&mut self, vars: &[(Rc<str>, A)])
    where
        A: Debug + Clone,
    {
        debug_assert!(
            {
                let mut seen: HashSet<&Rc<str>> = HashSet::new();
                vars.iter().fold(true, |acc, el: &(Rc<str>, A)| {
                    let acc = acc && !seen.contains(&el.0);
                    seen.insert(&el.0);
                    acc
                })
            },
            "duplicate name in {:?}",
            vars
        );
        let num_vars = vars.len();
        for entries in self.indices.values_mut() {
            for entry in entries {
                *entry += num_vars;
            }
        }
        for (index, (var, _)) in vars.iter().rev().enumerate() {
            match self.indices.get_mut(var) {
                None => {
                    self.indices.insert((*var).clone(), vec![index]);
                }
                Some(entries) => {
                    entries.push(index);
                }
            };
        }
        self.info.extend(
            vars.iter()
                .map(|(name, item)| ((*name).clone(), item.clone())),
        );
    }

    fn delete(&mut self, count: usize) {
        for _ in 0..count {
            match self.info.pop() {
                None => panic!("unexpected empty context"),
                Some((name, _)) => {
                    let should_delete = match self.indices.get_mut(&name) {
                        None => panic!("context missing entry {:?}", name),
                        Some(ixs) => match ixs.pop() {
                            None => panic!("context ran out of indices in {:?}", name),
                            Some(_) => ixs.is_empty(),
                        },
                    };
                    if should_delete {
                        self.indices.remove(&name);
                    }
                }
            }
        }
        for item in &mut self.indices {
            for entry in item.1 {
                *entry -= count;
            }
        }
    }
}

impl<A> Default for BoundVars<A> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct Implication {
    pub ty_vars: Vec<Kind>,
    pub antecedents: Vec<core::Type>,
    pub consequent: core::Type,
    pub evidence: Rc<core::Expr>,
}

impl Implication {
    pub fn instantiate_many(&self, tys: &[core::Type]) -> Self {
        let mut ty_vars = self.ty_vars.clone();
        for _ in tys.iter().rev() {
            let _ = ty_vars.pop();
        }
        let antecedents = self
            .antecedents
            .iter()
            .map(|ty| ty.instantiate_many(tys))
            .collect();
        let consequent = self.consequent.instantiate_many(tys);
        Implication {
            ty_vars,
            antecedents,
            consequent,
            evidence: self.evidence.clone(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SolveConstraintContext {
    pub constraint: syntax::Type<Rc<str>>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum TypeError {
    TypeError {
        error: type_inference::Error,
    },
    DuplicateClassArgument {
        source: Source,
        pos: usize,
    },
    NotInScope {
        source: Source,
        pos: usize,
        name: String,
    },
    NotInModule {
        source: Source,
        pos: usize,
        item: String,
    },
    KindError {
        source: Source,
        pos: usize,
        error: kind_inference::InferenceError,
    },
    NoSuchClass {
        source: Source,
        pos: usize,
    },
    NotAMember {
        source: Source,
        pos: usize,
        cls: Rc<str>,
    },
    CannotDeduce {
        source: Source,
        pos: usize,
        context: Option<SolveConstraintContext>,
    },
}

impl TypeError {
    pub fn source(&self) -> Source {
        match self {
            TypeError::TypeError { error } => error.source.clone(),
            TypeError::KindError { source, .. } => source.clone(),
            TypeError::NotInScope { source, .. } => source.clone(),
            TypeError::NotInModule { source, .. } => source.clone(),
            TypeError::DuplicateClassArgument { source, .. } => source.clone(),
            TypeError::NoSuchClass { source, .. } => source.clone(),
            TypeError::NotAMember { source, .. } => source.clone(),
            TypeError::CannotDeduce { source, .. } => source.clone(),
        }
    }

    pub fn position(&self) -> usize {
        match self {
            TypeError::TypeError { error } => error.position.unwrap_or(0),
            TypeError::KindError { pos, .. } => *pos,
            TypeError::NotInScope { pos, .. } => *pos,
            TypeError::NotInModule { pos, .. } => *pos,
            TypeError::DuplicateClassArgument { pos, .. } => *pos,
            TypeError::NoSuchClass { pos, .. } => *pos,
            TypeError::NotAMember { pos, .. } => *pos,
            TypeError::CannotDeduce { pos, .. } => *pos,
        }
    }

    pub fn message(&self) -> String {
        match self {
            TypeError::TypeError { error, .. } => match &error.info {
                type_inference::ErrorInfo::UnificationError { error } => match error {
                    type_inference::unification::Error::Mismatch { expected, actual } => format!(
                        "expected type \"{}\", got type \"{}\"",
                        expected.render(),
                        actual.render()
                    ),
                    type_inference::unification::Error::Occurs { meta, ty } => format!(
                        "infinite type from equating ?{} with \"{}\"",
                        meta,
                        ty.render()
                    ),

                    type_inference::unification::Error::KindError { error } => {
                        render_kind_inference_error(error)
                    }
                },

                type_inference::ErrorInfo::NotInScope { .. } => {
                    String::from("variable not in scope")
                }
                type_inference::ErrorInfo::DuplicateArgument { .. } => {
                    String::from("duplicate argument")
                }
                type_inference::ErrorInfo::RedundantPattern => String::from("redundant pattern"),
                type_inference::ErrorInfo::NotAValue { .. } => String::from("not a value"),
                type_inference::ErrorInfo::NotAModule => String::from("not a module"),
            },
            TypeError::KindError { error, .. } => render_kind_inference_error(error),
            TypeError::NotInScope { .. } => String::from("not in scope"),
            TypeError::NotInModule { .. } => String::from("not in scope"),
            TypeError::DuplicateClassArgument { .. } => {
                String::from("duplicate type class argument")
            }
            TypeError::NoSuchClass { .. } => String::from("type class not in scope"),
            TypeError::NotAMember { cls, .. } => {
                format!("not a member of the {:?} type class", cls)
            }
            TypeError::CannotDeduce { context, .. } => match context {
                None => String::from("cannot deduce"),
                Some(context) => format!("cannot deduce \"{:}\"", context.constraint.render()),
            },
        }
    }

    pub fn addendum(&self) -> Option<String> {
        match self {
            TypeError::TypeError { error, .. } => match &error.info {
                type_inference::ErrorInfo::UnificationError { .. } => None,
                type_inference::ErrorInfo::NotInScope { .. } => None,
                type_inference::ErrorInfo::DuplicateArgument { .. } => None,
                type_inference::ErrorInfo::RedundantPattern { .. } => None,
                type_inference::ErrorInfo::NotAValue { .. } => None,
                type_inference::ErrorInfo::NotAModule => None,
            },
            TypeError::KindError { error, .. } => match error.info {
                kind_inference::InferenceErrorInfo::NotInScope { .. } => None,
                kind_inference::InferenceErrorInfo::UnificationError { .. } => {
                    error.hint.as_ref().map(|hint| match hint {
                        kind_inference::InferenceErrorHint::WhileChecking { ty, has_kind } => {
                            format!(
                                "While checking that \"{}\" has kind \"{}\"",
                                ty.render(),
                                has_kind.render()
                            )
                        }
                        kind_inference::InferenceErrorHint::WhileInferring { ty } => {
                            format!("While inferring the kind of \"{}\"", ty.render())
                        }
                    })
                }
            },
            TypeError::DuplicateClassArgument { .. } => None,
            TypeError::NotInScope { .. } => None,
            TypeError::NotInModule { .. } => None,
            TypeError::NoSuchClass { .. } => None,
            TypeError::NotAMember { .. } => None,
            TypeError::CannotDeduce { .. } => None,
        }
    }

    pub fn report(&self, diagnostic: &mut diagnostic::Diagnostic) {
        diagnostic.item(
            Some(Location {
                source: self.source(),
                offset: Some(self.position()),
            }),
            Message {
                content: self.message(),
                addendum: self.addendum(),
            },
        )
    }
}

impl From<type_inference::Error> for TypeError {
    fn from(error: type_inference::Error) -> Self {
        TypeError::TypeError { error }
    }
}

fn render_kind_inference_error(error: &kind_inference::InferenceError) -> String {
    match &error.info {
        kind_inference::InferenceErrorInfo::NotInScope { .. } => String::from("type not in scope"),
        kind_inference::InferenceErrorInfo::UnificationError {
            error: unification_error,
        } => match unification_error {
            kind_inference::UnificationError::Mismatch { expected, actual } => {
                let mut message = String::from("expected kind ");
                message.push('"');
                message.push_str(expected.render().as_str());
                message.push('"');
                message.push_str(", got kind ");
                message.push('"');
                message.push_str(actual.render().as_str());
                message.push('"');
                message
            }
            kind_inference::UnificationError::Occurs { meta, kind } => {
                format!(
                    "infinite kind from equating ?{} with \"{}\"",
                    meta,
                    kind.render()
                )
            }
        },
    }
}

/// The results of typechecking a pattern
#[derive(Debug, PartialEq, Eq)]
pub struct CheckedPattern {
    /// The elaborated pattern
    pub pattern: core::Pattern,
    /// The variables bound by the pattern, and their types
    pub bindings: Vec<(Rc<str>, core::Type)>,
}

/// The results of inferring a type for a pattern
#[derive(Debug, PartialEq, Eq)]
pub struct InferredPattern {
    /// The elaborated pattern
    pub pattern: core::Pattern,
    /// The pattern's inferred type
    pub r#type: core::Type,
    /// The variables bound by the pattern, and their types
    pub bindings: Vec<(Rc<str>, core::Type)>,
}

pub fn register_declaration(
    common_kinds: &CommonKinds,
    implications: &mut Vec<Implication>,
    type_context: &mut HashMap<Rc<str>, Kind>,
    context: &mut HashMap<String, core::Signature>,
    class_context: &mut HashMap<Rc<str>, core::ClassDeclaration>,
    module_id: Option<ModuleId>,
    decl: &core::Declaration,
) {
    match decl {
        core::Declaration::BuiltinType { name, kind } => {
            register_builtin_type(type_context, name, kind);
        }
        core::Declaration::Definition { name, sig, .. } => register_definition(context, name, sig),
        core::Declaration::Module { name, decls, .. } => {
            register_module(common_kinds, context, name, decls)
        }
        core::Declaration::TypeAlias { name, args, body } => register_type_alias(name, args, body),
        core::Declaration::Class(decl) => register_class(
            common_kinds,
            type_context,
            implications,
            context,
            class_context,
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
            implications,
            module_id,
            ty_vars,
            assumes,
            head,
            evidence.clone(),
        ),
    }
}

pub fn check_module_with(
    common_kinds: &CommonKinds,
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut type_inference::unification::Solutions,
    implications: &mut Vec<Implication>,
    evidence: &mut Evidence,
    type_context: &mut HashMap<Rc<str>, Kind>,
    context: &mut HashMap<String, core::Signature>,
    class_context: &mut HashMap<Rc<str>, core::ClassDeclaration>,
    bound_vars: &mut BoundVars<core::Type>,
    bound_tyvars: &mut BoundVars<Kind>,
    modules: &Modules<core::Module>,
    module_context: &mut HashMap<ModuleId, HashMap<String, core::Signature>>,
    source: &Source,
    module: &syntax::Module,
) -> Result<core::Module, TypeError> {
    let decls = module.decls.iter().fold(Ok(vec![]), |acc, decl| {
        acc.and_then(|mut decls| {
            check_declaration(
                common_kinds,
                kind_solutions,
                type_solutions,
                implications,
                evidence,
                type_context,
                context,
                class_context,
                bound_vars,
                bound_tyvars,
                modules,
                module_context,
                source,
                decl,
            )
            .map(|checked_decls| match checked_decls {
                Declarations::Zero => decls,
                Declarations::One(decl) => {
                    register_declaration(
                        common_kinds,
                        implications,
                        type_context,
                        context,
                        class_context,
                        None,
                        &decl,
                    );
                    decls.push(decl);
                    decls
                }
                Declarations::Two(decl1, decl2) => {
                    register_declaration(
                        common_kinds,
                        implications,
                        type_context,
                        context,
                        class_context,
                        None,
                        &decl1,
                    );
                    decls.push(decl1);
                    register_declaration(
                        common_kinds,
                        implications,
                        type_context,
                        context,
                        class_context,
                        None,
                        &decl2,
                    );
                    decls.push(decl2);
                    decls
                }
            })
        })
    })?;
    Ok(core::Module { decls })
}

pub fn check_module(
    common_kinds: &CommonKinds,
    modules: &Modules<core::Module>,
    source: &Source,
    module: &syntax::Module,
) -> Result<core::Module, TypeError> {
    let mut kind_solutions = Default::default();
    let mut type_solutions = Default::default();
    let mut implications = Default::default();
    let mut evidence = Default::default();
    let mut types = Default::default();
    let mut context = Default::default();
    let mut class_context = Default::default();
    let mut variables = Default::default();
    let mut type_variables = Default::default();
    let mut module_context = Default::default();
    check_module_with(
        common_kinds,
        &mut kind_solutions,
        &mut type_solutions,
        &mut implications,
        &mut evidence,
        &mut types,
        &mut context,
        &mut class_context,
        &mut variables,
        &mut type_variables,
        modules,
        &mut module_context,
        source,
        module,
    )
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

fn check_definition(
    common_kinds: &CommonKinds,
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut type_inference::unification::Solutions,
    implications: &[Implication],
    evidence: &mut Evidence,
    type_context: &mut HashMap<Rc<str>, Kind>,
    context: &mut HashMap<String, core::Signature>,
    bound_vars: &mut BoundVars<core::Type>,
    bound_tyvars: &mut BoundVars<Kind>,
    module_context: &HashMap<ModuleId, HashMap<String, core::Signature>>,
    source: &Source,
    name: &str,
    ty: &syntax::Type<Rc<str>>,
    args: &[Spanned<syntax::Pattern>],
    body: &Spanned<syntax::Expr>,
) -> Result<core::Declaration, TypeError> {
    let ty_var_kinds: Vec<(Rc<str>, Kind)> = {
        let mut seen_names: HashSet<&str> = HashSet::new();
        ty.iter_vars()
            .filter_map(|name| {
                if !seen_names.contains(name.as_ref()) {
                    seen_names.insert(name);
                    Some((name.clone(), Kind::Meta(kind_solutions.fresh_meta())))
                } else {
                    None
                }
            })
            .collect()
    };

    bound_tyvars.insert(&ty_var_kinds);

    let ty = check_kind(
        common_kinds,
        type_context,
        bound_tyvars,
        kind_solutions,
        source,
        // TODO: make `ty` `Spanned` and use its position here.
        None,
        ty,
        &Kind::Type,
    )?;

    let _ = context.insert(
        name.to_string(),
        core::Signature::TypeSig(core::TypeSig::new(ty_var_kinds.clone(), ty.clone())),
    );

    let (constraints, ty) = ty.unwrap_constraints();
    for constraint in constraints {
        evidence.assume(
            /*
            TODO: use the constraint's textual position. unwrap constraints before
            type checking to get those positions.
            */
            0,
            evidence::Constraint::from_type(constraint),
        );
    }

    let arg_tys: Vec<type_inference::InferredPattern> = args
        .iter()
        .map(|arg| infer_pattern(common_kinds, type_solutions, evidence, arg))
        .collect();
    let out_ty = core::Type::Meta(Kind::Type, type_solutions.fresh_meta());

    type_inference::unify(
        common_kinds,
        type_context,
        bound_tyvars,
        kind_solutions,
        type_solutions,
        source,
        None,
        ty,
        &arg_tys.iter().rev().fold(out_ty.clone(), |acc, el| {
            core::Type::mk_arrow(common_kinds, &el.ty(common_kinds), &acc)
        }),
    )?;

    let arg_bound_vars = arg_tys
        .iter()
        .flat_map(|arg_ty| arg_ty.names().into_iter())
        .collect::<Vec<_>>();
    bound_vars.insert(&arg_bound_vars);
    let body = type_inference::check(
        &mut type_inference::InferenceContext {
            common_kinds,
            modules: module_context,
            types: type_context,
            type_variables: bound_tyvars,
            kind_solutions,
            type_solutions,
            type_signatures: context,
            variables: bound_vars,
            evidence,
            source,
        },
        body,
        &out_ty,
    )?;
    bound_vars.delete(arg_bound_vars.len());

    let body = arg_tys.into_iter().rev().fold(body, |body, arg_ty| {
        let pattern = arg_ty.pattern();
        match &pattern {
            core::Pattern::Char(_)
            | core::Pattern::Int(_)
            | core::Pattern::String(_)
            | core::Pattern::Record { .. }
            | core::Pattern::Variant { .. } => core::Expr::mk_lam(
                true,
                core::Expr::mk_case(core::Expr::Var(0), vec![core::Branch { pattern, body }]),
            ),
            core::Pattern::Name => core::Expr::mk_lam(true, body),
            core::Pattern::Wildcard => core::Expr::mk_lam(false, body),
        }
    });

    let (body, sig) = generalise(
        common_kinds,
        type_context,
        kind_solutions,
        type_solutions,
        implications,
        bound_tyvars,
        evidence,
        source,
        body,
        ty.clone(),
    )?;
    *evidence = Evidence::new();

    bound_tyvars.delete(ty_var_kinds.len());

    context.remove(name);

    Ok(core::Declaration::Definition {
        name: name.to_string(),
        sig,
        body: Rc::new(body),
    })
}

fn check_class_member(
    common_kinds: &CommonKinds,
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut type_inference::unification::Solutions,
    type_context: &mut HashMap<Rc<str>, Kind>,
    bound_tyvars: &mut BoundVars<Kind>,
    source: &Source,
    class_args_kinds: &[(Rc<str>, Kind)],
    name: &str,
    ty: &syntax::Type<Rc<str>>,
) -> Result<core::ClassMember, TypeError> {
    let ty_var_kinds: Vec<(Rc<str>, Kind)> = {
        let mut seen_names: HashSet<&str> = class_args_kinds
            .iter()
            .map(|(name, _)| name.as_ref())
            .collect();
        ty.iter_vars()
            .filter_map(|name| {
                if !seen_names.contains(name.as_ref()) {
                    seen_names.insert(name);
                    Some((name.clone(), Kind::Meta(kind_solutions.fresh_meta())))
                } else {
                    None
                }
            })
            .collect()
    };

    bound_tyvars.insert(&ty_var_kinds);
    let ty = check_kind(
        common_kinds,
        type_context,
        bound_tyvars,
        kind_solutions,
        source,
        // TODO: make `ty` `Spanned` and use its position here.
        None,
        ty,
        &Kind::Type,
    )?;
    bound_tyvars.delete(ty_var_kinds.len());

    let ty_vars: Vec<(Rc<str>, Kind)> = ty_var_kinds
        .into_iter()
        .map(|(name, kind)| (name, kind_solutions.zonk(true, kind)))
        .collect();
    let sig = core::TypeSig::new(ty_vars, type_solutions.zonk(kind_solutions, ty));
    Ok(core::ClassMember {
        name: name.to_string(),
        sig,
    })
}

fn check_class(
    common_kinds: &CommonKinds,
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut type_inference::unification::Solutions,
    type_context: &mut HashMap<Rc<str>, Kind>,
    bound_tyvars: &mut BoundVars<Kind>,
    source: &Source,
    supers: &[Spanned<syntax::Type<Rc<str>>>],
    name: &Rc<str>,
    args: &[Spanned<Rc<str>>],
    members: &[(String, syntax::Type<Rc<str>>)],
) -> Result<core::Declaration, TypeError> {
    let args_kinds: Vec<(Rc<str>, Kind)> = {
        let mut seen_names: HashSet<&str> = HashSet::new();
        args.iter()
            .map(|arg| {
                if !seen_names.contains(arg.item.as_ref()) {
                    seen_names.insert(arg.item.as_ref());
                    Ok((arg.item.clone(), Kind::Meta(kind_solutions.fresh_meta())))
                } else {
                    Err(TypeError::DuplicateClassArgument {
                        source: source.clone(),
                        pos: arg.pos,
                    })
                }
            })
            .collect::<Result<_, _>>()
    }?;

    bound_tyvars.insert(&args_kinds);

    let supers = supers
        .iter()
        .map(|superclass| {
            check_kind(
                common_kinds,
                type_context,
                bound_tyvars,
                kind_solutions,
                source,
                Some(superclass.pos),
                &superclass.item,
                &Kind::Constraint,
            )
        })
        .collect::<Result<_, _>>()?;

    let members = members
        .iter()
        .map(|(member_name, member_type)| {
            check_class_member(
                common_kinds,
                kind_solutions,
                type_solutions,
                type_context,
                bound_tyvars,
                source,
                &args_kinds,
                member_name,
                member_type,
            )
        })
        .collect::<Result<_, _>>()?;

    bound_tyvars.delete(args_kinds.len());

    Ok(core::Declaration::Class(core::ClassDeclaration {
        supers,
        name: name.clone(),
        args: args_kinds
            .into_iter()
            .map(|(name, kind)| (name, kind_solutions.zonk(true, kind)))
            .collect::<Vec<(Rc<str>, Kind)>>(),
        members,
    }))
}

fn check_instance(
    common_kinds: &CommonKinds,
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut type_inference::unification::Solutions,
    implications: &[Implication],
    evidence: &mut Evidence,
    type_context: &mut HashMap<Rc<str>, Kind>,
    context: &HashMap<String, core::Signature>,
    class_context: &HashMap<Rc<str>, core::ClassDeclaration>,
    bound_vars: &mut BoundVars<core::Type>,
    bound_tyvars: &mut BoundVars<Kind>,
    module_context: &HashMap<ModuleId, HashMap<String, core::Signature>>,
    source: &Source,
    assumes: &[Spanned<syntax::Type<Rc<str>>>],
    name: &Spanned<Rc<str>>,
    args: &[Spanned<syntax::Type<Rc<str>>>],
    members: &[syntax::InstanceMember],
) -> Result<(core::Declaration, core::Declaration), TypeError> {
    let evidence_name: Rc<str> = {
        let mut buffer = String::new();

        if !assumes.is_empty() {
            let mut assumes = assumes.iter();
            if let Some(assume) = assumes.next() {
                buffer.push_str(&assume.item.render());
            }
            for assume in assumes {
                buffer.push(',');
                buffer.push_str(&assume.item.render());
            }
            buffer.push_str("=>");
        }
        {
            buffer.push_str(
                args.iter()
                    .fold(syntax::Type::Name(name.item.clone()), |acc, arg| {
                        syntax::Type::mk_app(acc, arg.item.clone())
                    })
                    .render()
                    .as_str(),
            );
        }

        Rc::from(buffer)
    };

    let class_decl: core::ClassDeclaration = match class_context.get(&name.item) {
        None => Err(TypeError::NoSuchClass {
            source: source.clone(),
            pos: name.pos,
        }),
        Some(class_decl) => Ok(class_decl.clone()),
    }?;

    let head = args
        .iter()
        .fold(syntax::Type::Name(name.item.clone()), |acc, el| {
            syntax::Type::mk_app(acc, el.item.clone())
        });

    let ty_var_kinds: Vec<(Rc<str>, Kind)> = {
        let mut seen_names: HashSet<&str> = HashSet::new();
        args.iter()
            .flat_map(|arg| arg.item.iter_vars())
            .filter_map(|name| {
                if !seen_names.contains(name.as_ref()) {
                    seen_names.insert(name);
                    Some((name.clone(), Kind::Meta(kind_solutions.fresh_meta())))
                } else {
                    None
                }
            })
            .collect()
    };

    bound_tyvars.insert(&ty_var_kinds);

    let args: Vec<core::Type> = args
        .iter()
        .map(|arg| {
            let res = infer_kind(
                common_kinds,
                type_context,
                bound_tyvars,
                kind_solutions,
                source,
                arg.pos,
                &arg.item,
            )?;
            Ok(res.0)
        })
        .collect::<Result<_, TypeError>>()?;

    let assumes: Vec<core::Type> = assumes
        .iter()
        .map(|assume| {
            let constraint = check_kind(
                common_kinds,
                type_context,
                bound_tyvars,
                kind_solutions,
                source,
                Some(assume.pos),
                &assume.item,
                &Kind::Constraint,
            )?;
            let _ = evidence.assume(assume.pos, evidence::Constraint::from_type(&constraint));
            Ok(constraint)
        })
        .collect::<Result<_, TypeError>>()?;

    // locate evidence for superclasses
    let superclass_constructors: Vec<core::Expr> = {
        let mut superclass_constructors = Vec::new();

        for superclass in &class_decl.supers {
            let superclass = superclass.instantiate_many(&args);

            match evidence::solver::solve_constraint(
                &mut solver::Context {
                    common_kinds,
                    types: type_context,
                    kind_solutions,
                    type_solutions,
                    implications,
                    type_variables: bound_tyvars,
                    evidence,
                    source,
                },
                name.pos,
                &Some(SolveConstraintContext {
                    constraint: fill_ty_names(bound_tyvars, superclass.to_syntax()),
                }),
                &evidence::Constraint::from_type(&superclass),
            )
            .and_then(|evidence_expr| {
                abstract_evidence(
                    common_kinds,
                    type_context,
                    kind_solutions,
                    type_solutions,
                    implications,
                    bound_tyvars,
                    evidence,
                    source,
                    evidence_expr.as_ref().clone(),
                )
            }) {
                Err(err) => {
                    return Err(err);
                }
                Ok((evidence, _)) => {
                    superclass_constructors.push(evidence);
                }
            }
        }
        superclass_constructors
    };

    let instantiated_class_members: Vec<core::ClassMember> = class_decl
        .members
        .iter()
        .map(|class_member| core::ClassMember {
            name: class_member.name.clone(),
            sig: class_member.sig.clone().instantiate_many(&args),
        })
        .collect();

    let head = check_kind(
        common_kinds,
        type_context,
        bound_tyvars,
        kind_solutions,
        source,
        Some(name.pos),
        &head,
        &Kind::Constraint,
    )?;

    // type check members
    let mut checked_members = Vec::with_capacity(members.len());
    for member in members {
        match instantiated_class_members
            .iter()
            .find(|class_member| class_member.name == member.name.item)
        {
            None => {
                return Err(TypeError::NotAMember {
                    source: source.clone(),
                    pos: member.name.pos,
                    cls: name.item.clone(),
                })
            }
            Some(member_type) => {
                bound_tyvars.insert(&member_type.sig.ty_vars);

                match {
                    let member_body = type_inference::check(
                        &mut type_inference::InferenceContext {
                            common_kinds,
                            modules: module_context,
                            types: type_context,
                            type_variables: bound_tyvars,
                            kind_solutions,
                            type_solutions,
                            type_signatures: context,
                            variables: bound_vars,
                            evidence,
                            source,
                        },
                        &Spanned {
                            pos: member.name.pos,
                            item: syntax::Expr::mk_lam(member.args.clone(), member.body.clone()),
                        },
                        &member_type.sig.body,
                    )?;
                    generalise(
                        common_kinds,
                        type_context,
                        kind_solutions,
                        type_solutions,
                        implications,
                        bound_tyvars,
                        evidence,
                        source,
                        member_body,
                        member_type.sig.body.clone(),
                    )
                } {
                    Err(err) => return Err(err),
                    Ok((member_body, _)) => {
                        bound_tyvars.delete(member_type.sig.ty_vars.len());
                        checked_members.push(member_body);
                    }
                };
            }
        }
    }
    *evidence = Evidence::new();

    bound_tyvars.delete(ty_var_kinds.len());

    let ty_vars = ty_var_kinds
        .into_iter()
        .map(|(name, kind)| (name, kind_solutions.zonk(true, kind)))
        .collect();

    let evidence = {
        let mut dictionary: Vec<core::Expr> = superclass_constructors;
        dictionary.extend(checked_members.into_iter());

        for (ix, _assume) in assumes.iter().enumerate().rev() {
            for item in &mut dictionary {
                *item = core::Expr::mk_app((*item).clone(), core::Expr::Var(ix));
            }
        }

        let mut evidence = core::Expr::mk_record(
            dictionary
                .into_iter()
                .enumerate()
                .map(|(ix, val)| (core::Expr::Int(ix as u32), val))
                .collect(),
            None,
        );

        for _assume in assumes.iter() {
            evidence = core::Expr::mk_lam(true, evidence);
        }

        Rc::new(evidence)
    };

    let evidence_decl = core::Declaration::Evidence {
        name: evidence_name.clone(),
        body: evidence,
    };
    let instance_decl = core::Declaration::Instance {
        ty_vars,
        assumes: assumes
            .into_iter()
            .map(|assume| type_solutions.zonk(kind_solutions, assume))
            .collect(),
        head: type_solutions.zonk(kind_solutions, head),
        evidence: evidence_name,
    };

    Ok((evidence_decl, instance_decl))
}

fn check_declaration(
    common_kinds: &CommonKinds,
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut type_inference::unification::Solutions,
    implications: &mut Vec<Implication>,
    evidence: &mut Evidence,
    type_context: &mut HashMap<Rc<str>, Kind>,
    context: &mut HashMap<String, core::Signature>,
    class_context: &HashMap<Rc<str>, core::ClassDeclaration>,
    bound_vars: &mut BoundVars<core::Type>,
    bound_tyvars: &mut BoundVars<Kind>,
    modules: &Modules<core::Module>,
    module_context: &mut HashMap<ModuleId, HashMap<String, core::Signature>>,
    source: &Source,
    decl: &syntax::Spanned<syntax::Declaration>,
) -> Result<Declarations, TypeError> {
    match &decl.item {
        syntax::Declaration::Definition {
            name,
            ty,
            args,
            body,
        } => check_definition(
            common_kinds,
            kind_solutions,
            type_solutions,
            implications,
            evidence,
            type_context,
            context,
            bound_vars,
            bound_tyvars,
            module_context,
            source,
            name,
            ty,
            args,
            body,
        )
        .map(Declarations::One),
        syntax::Declaration::TypeAlias { name, args, body } => {
            todo!("check type alias {:?}", (name, args, body))
        }

        syntax::Declaration::Import { resolved, .. }
        | syntax::Declaration::FromImport { resolved, .. } => {
            let module_id = resolved.unwrap_or_else(|| panic!("unresolved import"));

            let module = modules.lookup(module_id);
            {
                let signatures = module.get_signatures(common_kinds);
                module_context.insert(module_id, signatures);
            }
            module.decls.iter().for_each(|decl| {
                if let core::Declaration::Instance {
                    ty_vars,
                    assumes,
                    head,
                    evidence,
                    ..
                } = decl
                {
                    register_instance(
                        implications,
                        Some(module_id),
                        ty_vars,
                        assumes,
                        head,
                        evidence.clone(),
                    )
                }
            });

            Ok(Declarations::Zero)
        }

        syntax::Declaration::Class {
            supers,
            name,
            args,
            members,
        } => check_class(
            common_kinds,
            kind_solutions,
            type_solutions,
            type_context,
            bound_tyvars,
            source,
            supers,
            name,
            args,
            members,
        )
        .map(Declarations::One),
        syntax::Declaration::Instance {
            assumes,
            name,
            args,
            members,
        } => check_instance(
            common_kinds,
            kind_solutions,
            type_solutions,
            implications,
            evidence,
            type_context,
            context,
            class_context,
            bound_vars,
            bound_tyvars,
            module_context,
            source,
            assumes,
            name,
            args,
            members,
        )
        .map(|(a, b)| Declarations::Two(a, b)),
    }
}

fn abstract_evidence(
    common_kinds: &CommonKinds,
    types: &HashMap<Rc<str>, Kind>,
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut type_inference::unification::Solutions,
    implications: &[Implication],
    type_variables: &BoundVars<Kind>,
    evidence: &mut Evidence,
    source: &Source,
    mut expr: core::Expr,
) -> Result<(core::Expr, Vec<core::Type>), TypeError> {
    expr.subst_placeholder(&mut |p| -> Result<_, TypeError> {
        let (expr, _solved_constraint) = solve_placeholder(
            &mut solver::Context {
                common_kinds,
                types,
                kind_solutions,
                type_solutions,
                implications,
                type_variables,
                evidence,
                source,
            },
            *p,
        )?;
        Ok(expr.as_ref().clone())
    })?;

    let mut unsolved_constraints: Vec<(core::EVar, core::Type)> = Vec::new();
    let mut seen_evars: HashSet<core::EVar> = HashSet::new();
    for ev in expr.iter_evars() {
        if !seen_evars.contains(ev) {
            seen_evars.insert(*ev);
            let constraint = evidence.lookup_evar(ev).unwrap();
            unsolved_constraints.push((
                *ev,
                type_solutions.zonk(kind_solutions, constraint.to_type()),
            ));
        }
    }

    let mut expr = expr;
    let mut new_unsolved_constraints = Vec::new();
    for (ev, constraint) in unsolved_constraints.into_iter().rev() {
        expr = expr.abstract_evar(ev);
        match constraint.iter_metas().next() {
            None => {}
            Some(_) => {
                todo!("handle ambiguous constraints")
            }
        }
        new_unsolved_constraints.push(constraint);
    }
    new_unsolved_constraints.reverse();

    Ok((expr, new_unsolved_constraints))
}

fn generalise(
    common_kinds: &CommonKinds,
    types: &HashMap<Rc<str>, Kind>,
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut type_inference::unification::Solutions,
    implications: &[Implication],
    type_variables: &BoundVars<Kind>,
    evidence: &mut Evidence,
    source: &Source,
    expr: core::Expr,
    ty: core::Type,
) -> Result<(core::Expr, core::TypeSig), TypeError> {
    let (expr, unsolved_constraints) = abstract_evidence(
        common_kinds,
        types,
        kind_solutions,
        type_solutions,
        implications,
        type_variables,
        evidence,
        source,
        expr,
    )?;

    let mut ty = type_solutions.zonk(kind_solutions, ty);
    for constraint in unsolved_constraints.into_iter().rev() {
        ty = core::Type::mk_fatarrow(common_kinds, constraint, ty);
    }

    let ty_vars: Vec<(Rc<str>, Kind)> = type_variables
        .info
        .iter()
        .map(|(name, kind)| (name.clone(), kind_solutions.zonk(true, kind.clone())))
        .collect();
    let sig = core::TypeSig::new(ty_vars, ty);

    Ok((expr, sig))
}

pub fn zonk_constraint(
    kind_solutions: &kind_inference::Solutions,
    type_solutions: &type_inference::unification::Solutions,
    constraint: &Constraint,
) -> Constraint {
    match constraint {
        Constraint::HasField { field, rest } => Constraint::HasField {
            field: field.clone(),
            rest: type_solutions.zonk(kind_solutions, rest.clone()),
        },
        Constraint::Type(ty) => Constraint::Type(type_solutions.zonk(kind_solutions, ty.clone())),
    }
}

pub fn fill_ty_names(
    bound_tyvars: &BoundVars<Kind>,
    ty: syntax::Type<usize>,
) -> syntax::Type<Rc<str>> {
    ty.map(&mut |&ix| bound_tyvars.lookup_index(ix).unwrap().0.clone())
}

fn infer_kind(
    common_kinds: &CommonKinds,
    types: &HashMap<Rc<str>, Kind>,
    type_variables: &BoundVars<Kind>,
    kind_solutions: &mut kind_inference::Solutions,
    source: &Source,
    pos: usize,
    ty: &syntax::Type<Rc<str>>,
) -> Result<(core::Type, Kind), TypeError> {
    let mut ctx =
        kind_inference::InferenceContext::new(common_kinds, types, type_variables, kind_solutions);
    kind_inference::infer(&mut ctx, ty).map_err(|error| TypeError::KindError {
        source: source.clone(),
        pos,
        error,
    })
}

fn check_kind(
    common_kinds: &CommonKinds,
    types: &HashMap<Rc<str>, Kind>,
    type_variables: &BoundVars<Kind>,
    kind_solutions: &mut kind_inference::Solutions,
    source: &Source,
    pos: Option<usize>,
    ty: &syntax::Type<Rc<str>>,
    kind: &Kind,
) -> Result<core::Type, TypeError> {
    let mut ctx =
        kind_inference::InferenceContext::new(common_kinds, types, type_variables, kind_solutions);
    kind_inference::check(&mut ctx, ty, kind).map_err(|error| TypeError::KindError {
        source: source.clone(),
        pos: pos.unwrap_or(0),
        error,
    })
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

fn eq_zonked_type(
    type_solutions: &type_inference::unification::Solutions,
    t1: &core::Type,
    t2: &core::Type,
) -> bool {
    fn zonk_just_enough<'a>(
        type_solutions: &'a type_inference::unification::Solutions,
        t: &'a core::Type,
    ) -> &'a core::Type {
        match t {
            core::Type::Meta(_, n) => match type_solutions.get(*n) {
                metavariables::Solution::Unsolved => t,
                metavariables::Solution::Solved(sol) => zonk_just_enough(type_solutions, sol),
            },
            t => t,
        }
    }
    let t2: &core::Type = zonk_just_enough(type_solutions, t2);
    match t1 {
        core::Type::Name(_, n) => match t2 {
            core::Type::Name(_, n2) => n == n2,
            _ => false,
        },
        core::Type::Var(_, v) => match t2 {
            core::Type::Var(_, v2) => v == v2,
            _ => false,
        },
        core::Type::Bool => matches!(t2, core::Type::Bool),
        core::Type::Int => matches!(t2, core::Type::Int),
        core::Type::Char => matches!(t2, core::Type::Char),
        core::Type::String => matches!(t2, core::Type::String),
        core::Type::Bytes => matches!(t2, core::Type::Bytes),
        core::Type::Cmd => matches!(t2, core::Type::Cmd),
        core::Type::Arrow(_) => matches!(t2, core::Type::Arrow(_)),
        core::Type::FatArrow(_) => matches!(t2, core::Type::FatArrow(_)),
        core::Type::Array(_) => matches!(t2, core::Type::Arrow(_)),
        core::Type::Record(_) => matches!(t2, core::Type::Record(_)),
        core::Type::Variant(_) => matches!(t2, core::Type::Variant(_)),
        core::Type::IO(_) => matches!(t2, core::Type::IO(_)),
        core::Type::RowNil => matches!(t2, core::Type::RowNil),
        core::Type::Unit => matches!(t2, core::Type::Unit),
        core::Type::Constraints(cs) => match t2 {
            core::Type::Constraints(cs2) => {
                cs.len() == cs2.len()
                    && cs
                        .iter()
                        .zip(cs2.iter())
                        .all(|(a, b)| eq_zonked_type(type_solutions, a, b))
            }
            _ => false,
        },
        core::Type::App(_, a, b) => match t2 {
            core::Type::App(_, a2, b2) => {
                eq_zonked_type(type_solutions, a, a2) && eq_zonked_type(type_solutions, b, b2)
            }
            _ => false,
        },
        core::Type::RowCons(a, b, c) => match t2 {
            core::Type::RowCons(a2, b2, c2) => {
                a == a2
                    && eq_zonked_type(type_solutions, b, b2)
                    && eq_zonked_type(type_solutions, c, c2)
            }
            _ => false,
        },
        core::Type::HasField(a, b) => match t2 {
            core::Type::HasField(a2, b2) => a == a2 && eq_zonked_type(type_solutions, b, b2),
            _ => false,
        },
        core::Type::Meta(_, n) => match type_solutions.get(*n) {
            metavariables::Solution::Unsolved => match t2 {
                core::Type::Meta(_, n2) => n == n2,
                _ => false,
            },
            metavariables::Solution::Solved(sol) => eq_zonked_type(type_solutions, sol, t2),
        },
    }
}

pub fn eq_zonked_constraint(
    type_solutions: &type_inference::unification::Solutions,
    c1: &evidence::Constraint,
    c2: &evidence::Constraint,
) -> bool {
    match c1 {
        evidence::Constraint::HasField { field, rest } => match c2 {
            evidence::Constraint::HasField {
                field: field2,
                rest: rest2,
            } => field == field2 && eq_zonked_type(type_solutions, rest, rest2),
            _ => false,
        },
        evidence::Constraint::Type(ty) => match c2 {
            evidence::Constraint::Type(ty2) => eq_zonked_type(type_solutions, ty, ty2),
            _ => false,
        },
    }
}
