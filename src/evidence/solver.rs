use crate::{
    core::{self, Expr, Placeholder},
    syntax::{self, Binop, Kind, Type},
    typecheck::{
        substitution::Substitution, Implication, SolveConstraintContext, TypeError, Typechecker,
        UnifyTypeContext,
    },
};

use super::Constraint;

mod test;

pub fn lookup_evidence(tc: &Typechecker, constraint: &Constraint) -> Option<core::Expr> {
    tc.evidence.environment.iter().enumerate().find_map(
        |(ix, (other_constraint, _, other_evidence))| {
            if tc.eq_zonked_constraint(constraint, other_constraint) {
                match other_evidence {
                    None => Some(core::Expr::mk_evar(ix)),
                    Some(expr) => Some(expr.clone()),
                }
            } else {
                None
            }
        },
    )
}

pub fn solve_constraint(
    context: &Option<SolveConstraintContext>,
    tc: &mut Typechecker,
    constraint: &Constraint,
) -> Result<core::Expr, TypeError> {
    match constraint {
        Constraint::Type(constraint) => {
            let _ = tc.check_kind(None, constraint, Kind::Constraint)?;

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
                    .map(|kind| tc.fresh_typevar(kind.clone()))
                    .collect();

                let implication = implication.instantiate_many(&metas);

                let mut subst = Substitution::new();
                let unify_context = UnifyTypeContext {
                    expected: constraint.clone(),
                    actual: implication.consequent.clone(),
                };

                match tc.unify_type_subst(
                    &mut subst,
                    &unify_context,
                    constraint.clone(),
                    implication.consequent.clone(),
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
                                .map(|x| tc.zonk_type(x))
                                .collect(),
                            consequent: tc.zonk_type(implication.consequent),
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
                    context: context.clone(),
                }),
                Some(evidence) => Ok(evidence),
            }
        }
        Constraint::HasField { field, rest } => {
            let _ = tc.check_kind(None, rest, Kind::Row)?;
            let new_evidence = match rest {
                syntax::Type::RowNil => Ok(core::Expr::Int(0)),
                syntax::Type::RowCons(other_field, _, other_rest) => {
                    if field <= other_field {
                        solve_constraint(
                            context,
                            tc,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: (**other_rest).clone(),
                            },
                        )
                    } else {
                        let ev = solve_constraint(
                            context,
                            tc,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: (**other_rest).clone(),
                            },
                        )?;
                        Ok(core::Expr::mk_binop(Binop::Add, core::Expr::Int(1), ev))
                    }
                }

                syntax::Type::Name(n) => todo!("deduce HasField for Name({})", n),

                syntax::Type::Var(_) => {
                    let evidence = lookup_evidence(tc, constraint);
                    match evidence {
                        None => {
                            // we're allow to conjure evidence for non-extistent HasField constraints,
                            // so the user doesn't have to write them
                            let ev = tc.evidence.assume(None, constraint.clone());
                            Ok(core::Expr::EVar(ev))
                        }
                        Some(evidence) => Ok(evidence),
                    }
                }
                syntax::Type::App(_, _) => {
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
                syntax::Type::Meta(n) => {
                    let (kind, sol) = &tc.type_solutions[*n];
                    // we assume solving is done after unification, so any unsolved variables
                    // will never recieve solutions
                    match sol {
                        None => {
                            match tc.zonk_kind(false, kind.clone()) {
                                // row metavariables can be safely defaulted to the empty row in the
                                // presence of ambiguity
                                Kind::Row => {
                                    let unify_type_context = UnifyTypeContext {
                                        expected: Type::Meta(*n),
                                        actual: Type::RowNil,
                                    };
                                    tc.solve_typevar_left(&unify_type_context, *n, Type::RowNil)?;
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
                                rest: sol.clone(),
                            },
                        ),
                    }
                }

                syntax::Type::Bool
                | syntax::Type::Int
                | syntax::Type::Char
                | syntax::Type::String
                | syntax::Type::Bytes
                | syntax::Type::Arrow
                | syntax::Type::FatArrow
                | syntax::Type::Constraints(_)
                | syntax::Type::HasField(_, _)
                | syntax::Type::Array
                | syntax::Type::Record
                | syntax::Type::Variant
                | syntax::Type::IO
                | syntax::Type::Unit => panic!("impossible"),
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
    match evidence.clone() {
        None => {
            let expr = solve_constraint(
                &Some(SolveConstraintContext {
                    pos: match pos {
                        None => 0,
                        Some(pos) => pos,
                    },
                    constraint: tc.fill_ty_names(tc.zonk_type(constraint.to_type())),
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
