//! Declaration checking.

use crate::type_inference::{self, infer_pattern};
use crate::{
    abstract_evidence, check_kind, evidence, evidence::solver, fill_ty_names, generalise,
    infer_kind, kind_inference, module, BoundVars, Implication, SolveConstraintContext, TypeError,
};
use ipso_core::{self as core, CommonKinds};
use ipso_diagnostic::Source;
use ipso_syntax::{self as syntax, kind::Kind, ModuleId, Modules, Spanned};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    todo,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Declarations {
    Zero,
    One(core::Declaration),
    Two(core::Declaration, core::Declaration),
}

pub fn check(
    common_kinds: &CommonKinds,
    implications: &mut Vec<Implication>,
    type_context: &mut HashMap<Rc<str>, Kind>,
    context: &mut HashMap<String, core::Signature>,
    class_context: &HashMap<Rc<str>, core::ClassDeclaration>,
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
            implications,
            type_context,
            context,
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
                    module::register_instance(
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
            type_context,
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
            implications,
            type_context,
            context,
            class_context,
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

pub fn check_definition(
    common_kinds: &CommonKinds,
    implications: &[Implication],
    types: &mut HashMap<Rc<str>, Kind>,
    type_signatures: &mut HashMap<String, core::Signature>,
    module_context: &HashMap<ModuleId, HashMap<String, core::Signature>>,
    source: &Source,
    name: &str,
    ty: &syntax::Type<Rc<str>>,
    args: &[Spanned<syntax::Pattern>],
    body: &Spanned<syntax::Expr>,
) -> Result<core::Declaration, TypeError> {
    let mut type_variables = BoundVars::new();
    let mut type_inference_state = type_inference::State::new();

    let ty_var_kinds: Vec<(Rc<str>, Kind)> = {
        let mut seen_names: HashSet<&str> = HashSet::new();
        ty.iter_vars()
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
        common_kinds,
        types,
        &type_variables,
        &mut type_inference_state.kind_inference_state,
        source,
        // TODO: make `ty` `Spanned` and use its position here.
        None,
        ty,
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
                    common_kinds,
                    modules: module_context,
                    types,
                    type_variables: &type_variables,
                    type_signatures,
                    source,
                },
                &mut type_inference_state,
                arg,
            )
        })
        .collect();
    let out_ty = type_inference_state.fresh_type_meta(Kind::Type);

    type_inference::unification::unify(
        type_inference::unification::Env {
            common_kinds,
            types,
            type_variables: &type_variables,
        },
        &mut type_inference_state.kind_inference_state,
        &mut type_inference_state.type_solutions,
        ty,
        &arg_tys.iter().rev().fold(out_ty.clone(), |acc, el| {
            core::Type::mk_arrow(common_kinds, &el.ty(common_kinds), &acc)
        }),
    )
    .map_err(|error| type_inference::Error::unification_error(source, error))?;

    let arg_bound_vars = arg_tys
        .iter()
        .flat_map(|arg_ty| arg_ty.names().into_iter())
        .collect::<Vec<_>>();
    let body = type_inference_state.with_bound_vars(&arg_bound_vars, |type_inference_state| {
        type_inference::check(
            type_inference::Env {
                common_kinds,
                modules: module_context,
                types,
                type_variables: &type_variables,
                type_signatures,
                source,
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
        common_kinds,
        types,
        &mut type_inference_state,
        implications,
        &type_variables,
        source,
        body,
        ty.clone(),
    )?;

    type_variables.delete(ty_var_kinds.len());

    type_signatures.remove(name);

    Ok(core::Declaration::Definition {
        name: name.to_string(),
        sig,
        body: Rc::new(body),
    })
}

pub fn check_class_member(
    common_kinds: &CommonKinds,
    kind_inference_state: &mut kind_inference::State,
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
                    Some((name.clone(), kind_inference_state.fresh_meta()))
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
    common_kinds: &CommonKinds,
    types: &mut HashMap<Rc<str>, Kind>,
    source: &Source,
    supers: &[Spanned<syntax::Type<Rc<str>>>],
    name: &Rc<str>,
    args: &[Spanned<Rc<str>>],
    members: &[(String, syntax::Type<Rc<str>>)],
) -> Result<core::Declaration, TypeError> {
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
                    Err(TypeError::DuplicateClassArgument {
                        source: source.clone(),
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
                common_kinds,
                types,
                &type_variables,
                &mut kind_inference_state,
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
                &mut kind_inference_state,
                &mut type_solutions,
                types,
                &mut type_variables,
                source,
                &args_kinds,
                member_name,
                member_type,
            )
        })
        .collect::<Result<_, _>>()?;

    type_variables.delete(args_kinds.len());

    Ok(core::Declaration::Class(core::ClassDeclaration {
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
    common_kinds: &CommonKinds,
    implications: &[Implication],
    types: &mut HashMap<Rc<str>, Kind>,
    context: &HashMap<String, core::Signature>,
    class_context: &HashMap<Rc<str>, core::ClassDeclaration>,
    module_context: &HashMap<ModuleId, HashMap<String, core::Signature>>,
    source: &Source,
    assumes: &[Spanned<syntax::Type<Rc<str>>>],
    name: &Spanned<Rc<str>>,
    args: &[Spanned<syntax::Type<Rc<str>>>],
    members: &[syntax::InstanceMember],
) -> Result<(core::Declaration, core::Declaration), TypeError> {
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
                common_kinds,
                types,
                &type_variables,
                &mut type_inference_state.kind_inference_state,
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
                types,
                &type_variables,
                &mut type_inference_state.kind_inference_state,
                source,
                Some(assume.pos),
                &assume.item,
                &Kind::Constraint,
            )?;
            let _ = type_inference_state
                .evidence
                .assume(assume.pos, evidence::Constraint::from_type(&constraint));
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
                    types,
                    type_inference_state: &mut type_inference_state,
                    implications,
                    type_variables: &type_variables,
                    source,
                },
                name.pos,
                &Some(SolveConstraintContext {
                    constraint: fill_ty_names(&type_variables, superclass.to_syntax()),
                }),
                &evidence::Constraint::from_type(&superclass),
            )
            .and_then(|evidence_expr| {
                abstract_evidence(
                    common_kinds,
                    types,
                    &mut type_inference_state,
                    implications,
                    &type_variables,
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
        types,
        &type_variables,
        &mut type_inference_state.kind_inference_state,
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
                type_variables.insert(&member_type.sig.ty_vars);

                match {
                    let member_body = type_inference::check(
                        type_inference::Env {
                            common_kinds,
                            modules: module_context,
                            types,
                            type_variables: &type_variables,
                            type_signatures: context,
                            source,
                        },
                        &mut type_inference_state,
                        &Spanned {
                            pos: member.name.pos,
                            item: syntax::Expr::mk_lam(member.args.clone(), member.body.clone()),
                        },
                        &member_type.sig.body,
                    )?;
                    generalise(
                        common_kinds,
                        types,
                        &mut type_inference_state,
                        implications,
                        &type_variables,
                        source,
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
            .map(|assume| type_inference_state.zonk_type(assume))
            .collect(),
        head: type_inference_state.zonk_type(head),
        evidence: evidence_name,
    };

    Ok((evidence_decl, instance_decl))
}
