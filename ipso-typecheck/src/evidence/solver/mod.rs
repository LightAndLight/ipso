use crate::{
    substitution::Substitution, Implication, SolveConstraintContext, TypeError, Typechecker,
    UnifyTypeContextRefs,
};
use ipso_core::{self as core, Expr, Placeholder};
use ipso_syntax::{kind::Kind, Binop};

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
            debug_assert!(tc.zonk_kind(false, &constraint.kind()) == Kind::Constraint);

            match tc.evidence.find(tc, &Constraint::from_type(constraint)) {
                None => {}
                Some(expr) => {
                    return Ok(expr);
                }
            }

            let implications = tc.implications.clone();

            let mut result = None;
            for implication in implications.into_iter() {
                let metas: Vec<core::Type> = implication
                    .ty_vars
                    .iter()
                    .map(|kind| tc.fresh_typevar(kind.clone()))
                    .collect();

                let implication = implication.instantiate_many(&metas);

                let mut subst = Substitution::new();
                let unify_context = UnifyTypeContextRefs {
                    expected: constraint,
                    actual: &implication.consequent,
                };

                match tc.unify_type_subst(
                    &mut subst,
                    &unify_context,
                    constraint,
                    &implication.consequent,
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
                                .map(|x| tc.zonk_type(&x))
                                .collect(),
                            consequent: tc.zonk_type(&implication.consequent),
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
            debug_assert!(tc.zonk_kind(false, &rest.kind()) == Kind::Row);

            let new_evidence = match rest {
                core::Type::RowNil => Ok(core::Expr::Int(0)),
                core::Type::RowCons(other_field, _, other_rest) => {
                    if field <= other_field {
                        solve_constraint(
                            context,
                            tc,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: other_rest.as_ref().clone(),
                            },
                        )
                    } else {
                        let ev = solve_constraint(
                            context,
                            tc,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: other_rest.as_ref().clone(),
                            },
                        )?;
                        Ok(core::Expr::mk_binop(Binop::Add, core::Expr::Int(1), ev))
                    }
                }

                core::Type::Name(_, n) => todo!("deduce HasField for Name({})", n),

                core::Type::Var(_, _) => {
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
                core::Type::App(_, _, _) => {
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
                core::Type::Meta(_, n) => {
                    let (kind, sol) = &tc.type_solutions[*n];
                    // we assume solving is done after unification, so any unsolved variables
                    // will never recieve solutions
                    match sol.clone() {
                        None => {
                            match tc.zonk_kind(false, kind) {
                                // row metavariables can be safely defaulted to the empty row in the
                                // presence of ambiguity
                                Kind::Row => {
                                    let unify_type_context = UnifyTypeContextRefs {
                                        expected: &core::Type::Meta(Kind::Row, *n),
                                        actual: &core::Type::RowNil,
                                    };
                                    tc.solve_typevar_left(
                                        &unify_type_context,
                                        *n,
                                        &core::Type::RowNil,
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
                        Some(sol) => solve_constraint(
                            context,
                            tc,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: sol,
                            },
                        ),
                    }
                }

                core::Type::Bool
                | core::Type::Int
                | core::Type::Char
                | core::Type::String
                | core::Type::Bytes
                | core::Type::Arrow(_)
                | core::Type::FatArrow(_)
                | core::Type::Constraints(_)
                | core::Type::HasField(_, _)
                | core::Type::Array(_)
                | core::Type::Record(_)
                | core::Type::Variant(_)
                | core::Type::IO(_)
                | core::Type::Unit
                | core::Type::Cmd => panic!("impossible"),
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
                    constraint: tc.fill_ty_names(tc.zonk_type(&constraint.to_type()).to_syntax()),
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
