//! Declaration checking.

use crate::{
    check_kind,
    constraint_solving::{self, solve_constraint, solve_placeholder},
    evidence, fill_ty_names, generalise, infer_kind, kind_inference,
    type_inference::{self, infer_pattern},
    BoundVars, Error, Implication,
};
use ipso_core::{self as core, CommonKinds, EVar, TypeSig};
use ipso_diagnostic::Source;
use ipso_syntax::{self as syntax, kind::Kind, ModuleId, Modules, Spanned};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    todo,
};

/// Checked declarations.
#[derive(Debug, PartialEq, Eq)]
pub enum Checked {
    Definition {
        name: String,
        sig: TypeSig,
        body: Rc<core::Expr>,
    },
    ResolvedImport {
        module_id: ModuleId,
        module: core::Module,
    },
    Class(core::ClassDeclaration),
    Instance {
        evidence_name: Rc<str>,
        evidence_body: Rc<core::Expr>,
        instance_ty_vars: Vec<(Rc<str>, Kind)>,
        instance_assumes: Vec<core::Type>,
        instance_head: core::Type,
        instance_evidence: Rc<str>,
    },
}

#[derive(Clone, Copy)]
pub struct Env<'a> {
    pub common_kinds: &'a CommonKinds,
    pub modules: &'a Modules<core::Module>,
    pub module_context: &'a HashMap<ModuleId, HashMap<String, core::Signature>>,
    pub type_context: &'a HashMap<Rc<str>, Kind>,
    pub class_context: &'a HashMap<Rc<str>, core::ClassDeclaration>,
    pub context: &'a HashMap<String, core::Signature>,
    pub implications: &'a [Implication],
    pub source: &'a Source,
}

pub fn check(env: Env, decl: &syntax::Spanned<syntax::Declaration>) -> Result<Checked, Error> {
    match &decl.item {
        syntax::Declaration::Definition {
            name,
            ty,
            args,
            body,
        } => check_definition(env, name, ty, args, body),
        syntax::Declaration::TypeAlias { name, args, body } => {
            todo!("check type alias {:?}", (name, args, body))
        }

        syntax::Declaration::Import { resolved, .. }
        | syntax::Declaration::FromImport { resolved, .. } => {
            let module_id = resolved.unwrap_or_else(|| panic!("unresolved import"));

            let module = env.modules.lookup(module_id);

            Ok(Checked::ResolvedImport {
                module_id,
                module: module.clone(),
            })
        }

        syntax::Declaration::Class {
            supers,
            name,
            args,
            members,
        } => check_class(env, supers, name, args, members),
        syntax::Declaration::Instance {
            assumes,
            name,
            args,
            members,
        } => check_instance(env, assumes, name, args, members),
    }
}

pub fn check_definition(
    env: Env,
    name: &str,
    ty: &Spanned<syntax::Type<Rc<str>>>,
    args: &[Spanned<syntax::Pattern>],
    body: &Spanned<syntax::Expr>,
) -> Result<Checked, Error> {
    let position = ty.pos;

    let mut type_signatures = env.context.clone();
    let mut type_variables = BoundVars::new();
    let mut type_inference_state = type_inference::State::new();

    let ty_var_kinds: Vec<(Rc<str>, Kind)> = {
        let mut seen_names: HashSet<&str> = HashSet::new();
        ty.item
            .iter_vars()
            .filter_map(|name| {
                if !seen_names.contains(name.as_ref()) {
                    seen_names.insert(name);
                    Some((
                        name.clone(),
                        type_inference_state.kind_inference_state.fresh_meta(),
                    ))
                } else {
                    None
                }
            })
            .collect()
    };

    type_variables.insert(&ty_var_kinds);

    let ty = check_kind(
        env.common_kinds,
        env.type_context,
        &type_variables,
        &mut type_inference_state.kind_inference_state,
        env.source,
        Some(position),
        &ty.item,
        &Kind::Type,
    )?;

    let _ = type_signatures.insert(
        name.to_string(),
        core::Signature::TypeSig(core::TypeSig::new(ty_var_kinds.clone(), ty.clone())),
    );

    let (constraints, ty) = ty.unwrap_constraints();
    for constraint in constraints {
        type_inference_state.evidence.assume(
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
        .map(|arg| {
            infer_pattern(
                type_inference::Env {
                    common_kinds: env.common_kinds,
                    modules: env.module_context,
                    types: env.type_context,
                    type_variables: &type_variables,
                    type_signatures: &type_signatures,
                    source: env.source,
                },
                &mut type_inference_state,
                arg,
            )
        })
        .collect();
    let out_ty = type_inference_state.fresh_type_meta(Kind::Type);

    type_inference::unification::unify(
        type_inference::unification::Env {
            common_kinds: env.common_kinds,
            types: env.type_context,
            type_variables: &type_variables,
        },
        &mut type_inference_state.kind_inference_state,
        &mut type_inference_state.type_solutions,
        ty,
        &arg_tys.iter().rev().fold(out_ty.clone(), |acc, el| {
            core::Type::mk_arrow(env.common_kinds, &el.ty(env.common_kinds), &acc)
        }),
    )
    .map_err(|error| type_inference::Error::unification_error(env.source, position, error))?;

    let arg_bound_vars = arg_tys
        .iter()
        .flat_map(|arg_ty| arg_ty.names().into_iter())
        .collect::<Vec<_>>();
    let body = type_inference_state.with_bound_vars(&arg_bound_vars, |type_inference_state| {
        type_inference::check(
            type_inference::Env {
                common_kinds: env.common_kinds,
                modules: env.module_context,
                types: env.type_context,
                type_variables: &type_variables,
                type_signatures: &type_signatures,
                source: env.source,
            },
            type_inference_state,
            body,
            &out_ty,
        )
    })?;

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
        env.common_kinds,
        env.implications,
        env.type_context,
        &type_variables,
        &mut type_inference_state,
        env.source,
        ty_var_kinds.len(),
        body,
        ty.clone(),
    )?;

    type_variables.delete(ty_var_kinds.len());

    Ok(Checked::Definition {
        name: name.to_string(),
        sig,
        body: Rc::new(body),
    })
}

pub fn check_class_member(
    common_kinds: &CommonKinds,
    type_context: &HashMap<Rc<str>, Kind>,
    bound_tyvars: &BoundVars<Kind>,
    source: &Source,
    kind_inference_state: &mut kind_inference::State,
    type_solutions: &mut type_inference::unification::Solutions,
    class_args_kinds: &[(Rc<str>, Kind)],
    name: &str,
    ty: &syntax::Type<Rc<str>>,
) -> Result<core::ClassMember, Error> {
    let ty_var_kinds: Vec<(Rc<str>, Kind)> = {
        let mut seen_names: HashSet<&str> = class_args_kinds
            .iter()
            .map(|(name, _)| name.as_ref())
            .collect();
        ty.iter_vars()
            .filter_map(|name| {
                if !seen_names.contains(name.as_ref()) {
                    seen_names.insert(name);
                    Some((name.clone(), kind_inference_state.fresh_meta()))
                } else {
                    None
                }
            })
            .collect()
    };

    let mut bound_tyvars = bound_tyvars.clone();
    bound_tyvars.insert(&ty_var_kinds);
    let ty = check_kind(
        common_kinds,
        type_context,
        &bound_tyvars,
        kind_inference_state,
        source,
        // TODO: make `ty` `Spanned` and use its position here.
        None,
        ty,
        &Kind::Type,
    )?;
    bound_tyvars.delete(ty_var_kinds.len());

    let ty_vars: Vec<(Rc<str>, Kind)> = ty_var_kinds
        .into_iter()
        .map(|(name, kind)| (name, kind_inference_state.zonk(true, kind)))
        .collect();
    let sig = core::TypeSig::new(
        ty_vars,
        type_solutions.zonk(&kind_inference_state.kind_solutions, ty),
    );
    Ok(core::ClassMember {
        name: name.to_string(),
        sig,
    })
}

pub fn check_class(
    env: Env,
    supers: &[Spanned<syntax::Type<Rc<str>>>],
    name: &Rc<str>,
    args: &[Spanned<Rc<str>>],
    members: &[(String, syntax::Type<Rc<str>>)],
) -> Result<Checked, Error> {
    let mut type_variables = BoundVars::new();
    let mut type_solutions = type_inference::unification::Solutions::new();
    let mut kind_inference_state = kind_inference::State::new();

    let args_kinds: Vec<(Rc<str>, Kind)> = {
        let mut seen_names: HashSet<&str> = HashSet::new();
        args.iter()
            .map(|arg| {
                if !seen_names.contains(arg.item.as_ref()) {
                    seen_names.insert(arg.item.as_ref());
                    Ok((arg.item.clone(), kind_inference_state.fresh_meta()))
                } else {
                    Err(Error::DuplicateClassArgument {
                        source: env.source.clone(),
                        pos: arg.pos,
                    })
                }
            })
            .collect::<Result<_, _>>()
    }?;

    type_variables.insert(&args_kinds);

    let supers = supers
        .iter()
        .map(|superclass| {
            check_kind(
                env.common_kinds,
                env.type_context,
                &type_variables,
                &mut kind_inference_state,
                env.source,
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
                env.common_kinds,
                env.type_context,
                &type_variables,
                env.source,
                &mut kind_inference_state,
                &mut type_solutions,
                &args_kinds,
                member_name,
                member_type,
            )
        })
        .collect::<Result<_, _>>()?;

    type_variables.delete(args_kinds.len());

    Ok(Checked::Class(core::ClassDeclaration {
        supers,
        name: name.clone(),
        args: args_kinds
            .into_iter()
            .map(|(name, kind)| (name, kind_inference_state.zonk(true, kind)))
            .collect::<Vec<(Rc<str>, Kind)>>(),
        members,
    }))
}

pub fn check_instance(
    env: Env,
    assumes: &[Spanned<syntax::Type<Rc<str>>>],
    name: &Spanned<Rc<str>>,
    args: &[Spanned<syntax::Type<Rc<str>>>],
    members: &[syntax::InstanceMember],
) -> Result<Checked, Error> {
    let mut type_variables = BoundVars::new();
    let mut type_inference_state = type_inference::State::new();

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

    let class_decl: core::ClassDeclaration = match env.class_context.get(&name.item) {
        None => Err(Error::NoSuchClass {
            source: env.source.clone(),
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
                    Some((
                        name.clone(),
                        type_inference_state.kind_inference_state.fresh_meta(),
                    ))
                } else {
                    None
                }
            })
            .collect()
    };

    type_variables.insert(&ty_var_kinds);

    let args: Vec<core::Type> = args
        .iter()
        .map(|arg| {
            let res = infer_kind(
                env.common_kinds,
                env.type_context,
                &type_variables,
                &mut type_inference_state.kind_inference_state,
                env.source,
                arg.pos,
                &arg.item,
            )?;
            Ok(res.0)
        })
        .collect::<Result<_, Error>>()?;

    let assumes: Vec<(EVar, core::Type)> = assumes
        .iter()
        .map(|assume| {
            let constraint = check_kind(
                env.common_kinds,
                env.type_context,
                &type_variables,
                &mut type_inference_state.kind_inference_state,
                env.source,
                Some(assume.pos),
                &assume.item,
                &Kind::Constraint,
            )?;
            let evar = type_inference_state
                .evidence
                .assume(assume.pos, evidence::Constraint::from_type(&constraint));
            Ok((evar, constraint))
        })
        .collect::<Result<_, Error>>()?;

    // locate evidence for superclasses
    let superclass_constructors: Vec<core::Expr> = {
        let mut superclass_constructors = Vec::new();

        for superclass in &class_decl.supers {
            let superclass = superclass.instantiate_many(&args);

            match solve_constraint(
                constraint_solving::Env {
                    common_kinds: env.common_kinds,
                    types: env.type_context,
                    implications: env.implications,
                    type_variables: &type_variables,
                    source: env.source,
                },
                &mut type_inference_state,
                name.pos,
                &evidence::Constraint::from_type(&superclass),
            )
            .map_err(|error| {
                Error::from(
                    error.with_hint(constraint_solving::ErrorHint::WhileSolving {
                        constraint: fill_ty_names(&type_variables, superclass.to_syntax()),
                    }),
                )
            }) {
                Err(err) => {
                    return Err(err);
                }
                Ok(evidence) => {
                    superclass_constructors.push(evidence.as_ref().clone());
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
        env.common_kinds,
        env.type_context,
        &type_variables,
        &mut type_inference_state.kind_inference_state,
        env.source,
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
                return Err(Error::NotAMember {
                    source: env.source.clone(),
                    pos: member.name.pos,
                    cls: name.item.clone(),
                })
            }
            Some(member_type) => {
                type_variables.insert(&member_type.sig.ty_vars);

                match {
                    let expr = Spanned {
                        pos: member.name.pos,
                        item: syntax::Expr::mk_lam(member.args.clone(), member.body.clone()),
                    };
                    let ty = &member_type.sig.body;

                    let member_body = type_inference::check(
                        type_inference::Env {
                            common_kinds: env.common_kinds,
                            modules: env.module_context,
                            types: env.type_context,
                            type_variables: &type_variables,
                            type_signatures: env.context,
                            source: env.source,
                        },
                        &mut type_inference_state,
                        &expr,
                        ty,
                    )?;

                    generalise(
                        env.common_kinds,
                        env.implications,
                        env.type_context,
                        &type_variables,
                        &mut type_inference_state,
                        env.source,
                        member_type.sig.ty_vars.len(),
                        member_body,
                        member_type.sig.body.clone(),
                    )
                } {
                    Err(err) => return Err(err),
                    Ok((member_body, _)) => {
                        type_variables.delete(member_type.sig.ty_vars.len());
                        checked_members.push(member_body);
                    }
                };
            }
        }
    }

    type_variables.delete(ty_var_kinds.len());

    let ty_vars = ty_var_kinds
        .into_iter()
        .map(|(name, kind)| {
            (
                name,
                type_inference_state.kind_inference_state.zonk(true, kind),
            )
        })
        .collect();

    let evidence = {
        let mut dictionary: Vec<core::Expr> = superclass_constructors;
        dictionary.extend(checked_members.into_iter());

        let mut evidence = core::Expr::mk_record(
            dictionary
                .into_iter()
                .enumerate()
                .map(|(ix, val)| (core::Expr::Int(ix as u32), val))
                .collect(),
            None,
        );

        evidence.subst_placeholder(&mut |p| -> Result<_, Error> {
            let (expr, _solved_constraint) = solve_placeholder(
                constraint_solving::Env {
                    common_kinds: env.common_kinds,
                    types: env.type_context,
                    implications: env.implications,
                    type_variables: &type_variables,
                    source: env.source,
                },
                &mut type_inference_state,
                *p,
            )?;

            Ok(expr.as_ref().clone())
        })?;

        for (evar, _) in assumes.iter().rev() {
            evidence = evidence.abstract_evar(*evar);
        }

        Rc::new(evidence)
    };

    Ok(Checked::Instance {
        evidence_name: evidence_name.clone(),
        evidence_body: evidence,
        instance_ty_vars: ty_vars,
        instance_assumes: assumes
            .into_iter()
            .map(|(_, assume)| type_inference_state.zonk_type(assume))
            .collect(),
        instance_head: type_inference_state.zonk_type(head),
        instance_evidence: evidence_name,
    })
}
