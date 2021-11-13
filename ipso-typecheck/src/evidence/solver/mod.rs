use crate::{
    substitution::Substitution, Implication, SolveConstraintContext, TypeError, Typechecker,
    UnifyTypeContext,
};
use ipso_core::{self as core, Expr, Placeholder};
use ipso_syntax::{kind::Kind, r#type::Type, Binop};

use super::Constraint;

mod test;

pub fn lookup_evidence(tc: &Typechecker, constraint: &Constraint) -> Option<core::Expr> {
    tc.evidence
        .environment
        .iter()
        .find_map(|(other_constraint, _, other_evidence)| {
            if tc.eq_zonked_constraint(constraint, other_constraint) {
                other_evidence.as_ref().cloned()
            } else {
                None
            }
        })
}

pub fn solve_constraint(
    context: &Option<SolveConstraintContext>,
    tc: &mut Typechecker,
    constraint: &Constraint,
) -> Result<core::Expr, TypeError> {
    match constraint {
        Constraint::Type(constraint) => {
            let _ = tc.check_kind(None, constraint.get_value(), &Kind::Constraint)?;

            match tc.evidence.find(tc, &Constraint::from_type(constraint)) {
                None => {}
                Some(expr) => {
                    return Ok(expr);
                }
            }

            let implications = tc.implications.clone();

            let mut result = None;
            for implication in implications.into_iter() {
                let metas: Vec<Type<usize>> = implication
                    .ty_vars
                    .iter()
                    .map(|kind| tc.fresh_typevar(kind.clone()).get_value().clone())
                    .collect();

                let implication = implication.instantiate_many(&metas);

                let mut subst = Substitution::new();
                let unify_context = UnifyTypeContext {
                    expected: constraint.get_value().clone(),
                    actual: implication.consequent.get_value().clone(),
                };

                match tc.unify_type_subst(
                    &mut subst,
                    &unify_context,
                    constraint.get_value(),
                    implication.consequent.get_value(),
                ) {
                    Err(_) => {
                        continue;
                    }
                    Ok(()) => {
                        tc.commit_substitutions(subst);
                        let implication = Implication {
                            ty_vars: implication.ty_vars,
                            antecedents: implication
                                .antecedents
                                .into_iter()
                                .map(|x| {
                                    core::Type::unsafe_new(
                                        tc.zonk_type(x.get_value()),
                                        x.get_kind().clone(),
                                    )
                                })
                                .collect(),
                            consequent: core::Type::unsafe_new(
                                tc.zonk_type(implication.consequent.get_value()),
                                implication.consequent.get_kind().clone(),
                            ),
                            evidence: implication.evidence,
                        };

                        let mut evidence = Ok(implication.evidence);

                        for antecedent in &implication.antecedents {
                            match solve_constraint(context, tc, &Constraint::from_type(antecedent))
                            {
                                Err(err) => {
                                    evidence = Err(err);
                                    break;
                                }
                                Ok(arg) => {
                                    evidence = evidence.map(|evidence| Expr::mk_app(evidence, arg));
                                }
                            }
                        }

                        match evidence {
                            Err(_err) => {
                                continue;
                            }
                            Ok(evidence) => {
                                result = Some(evidence);
                                break;
                            }
                        }
                    }
                }
            }

            match result {
                None => Err(TypeError::CannotDeduce {
                    source: tc.source(),
                    context: context.clone(),
                }),
                Some(evidence) => Ok(evidence),
            }
        }
        Constraint::HasField { field, rest } => {
            let _ = tc.check_kind(None, rest.get_value(), &Kind::Row)?;
            let new_evidence = match rest.get_value() {
                Type::RowNil => Ok(core::Expr::Int(0)),
                Type::RowCons(other_field, _, other_rest) => {
                    if field <= other_field {
                        solve_constraint(
                            context,
                            tc,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: core::Type::unsafe_new(
                                    other_rest.as_ref().clone(),
                                    Kind::Row,
                                ),
                            },
                        )
                    } else {
                        let ev = solve_constraint(
                            context,
                            tc,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: core::Type::unsafe_new(
                                    other_rest.as_ref().clone(),
                                    Kind::Row,
                                ),
                            },
                        )?;
                        Ok(core::Expr::mk_binop(Binop::Add, core::Expr::Int(1), ev))
                    }
                }

                Type::Name(n) => todo!("deduce HasField for Name({})", n),

                Type::Var(_) => {
                    let evidence = lookup_evidence(tc, constraint);
                    match evidence {
                        None => {
                            // we're allow to conjure evidence for non-extistent HasField constraints,
                            // so the user doesn't have to write them
                            let ev = tc.evidence.assume(None, constraint.clone());
                            debug_assert!(tc.evidence.lookup_evar(&ev) != None);
                            Ok(core::Expr::EVar(ev))
                        }
                        Some(evidence) => Ok(evidence),
                    }
                }
                Type::App(_, _) => {
                    let evidence = lookup_evidence(tc, constraint);
                    match evidence {
                        None => {
                            panic!(
                                "impossible: cannot deduce HasField({:?}, {:?})",
                                field, rest
                            )
                        }
                        Some(evidence) => Ok(evidence),
                    }
                }
                Type::Meta(n) => {
                    let (kind, sol) = &tc.type_solutions[*n];
                    // we assume solving is done after unification, so any unsolved variables
                    // will never recieve solutions
                    match sol.clone() {
                        None => {
                            match tc.zonk_kind(false, kind) {
                                // row metavariables can be safely defaulted to the empty row in the
                                // presence of ambiguity
                                Kind::Row => {
                                    let unify_type_context = UnifyTypeContext {
                                        expected: Type::Meta(*n),
                                        actual: Type::RowNil,
                                    };
                                    tc.solve_typevar_left(
                                        &unify_type_context,
                                        *n,
                                        &core::Type::mk_rownil(),
                                    )?;
                                    solve_constraint(context, tc, constraint)
                                }
                                _ => {
                                    let evidence = lookup_evidence(tc, constraint);
                                    match evidence {
                                        None => {
                                            panic!(
                                        "impossible: cannot deduce HasField({:?}, {:?}) given {:?}",
                                        field, rest, tc.evidence
                                    )
                                        }
                                        Some(evidence) => Ok(evidence),
                                    }
                                }
                            }
                        }
                        Some(sol) => {
                            let kind = kind.clone();
                            solve_constraint(
                                context,
                                tc,
                                &Constraint::HasField {
                                    field: field.clone(),
                                    rest: core::Type::unsafe_new(sol, kind),
                                },
                            )
                        }
                    }
                }

                Type::Bool
                | Type::Int
                | Type::Char
                | Type::String
                | Type::Bytes
                | Type::Arrow
                | Type::FatArrow
                | Type::Constraints(_)
                | Type::HasField(_, _)
                | Type::Array
                | Type::Record
                | Type::Variant
                | Type::IO
                | Type::Unit => panic!("impossible"),
            }?;
            Ok(new_evidence)
        }
    }
}

pub fn solve_placeholder(
    tc: &mut Typechecker,
    p: Placeholder,
) -> Result<(core::Expr, Constraint), TypeError> {
    let (constraint, pos, evidence) = tc.evidence.environment[p.0].clone();
    match evidence {
        None => {
            let expr = solve_constraint(
                &Some(SolveConstraintContext {
                    pos: pos.unwrap_or(0),
                    constraint: tc.fill_ty_names(tc.zonk_type(constraint.to_type().get_value())),
                }),
                tc,
                &constraint,
            )?;
            tc.evidence.environment[p.0].2 = Some(expr.clone());
            Ok((expr, constraint.clone()))
        }
        Some(evidence) => Ok((evidence, constraint.clone())),
    }
}
