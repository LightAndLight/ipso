#[cfg(test)]
mod test;

use super::{Constraint, Evidence};
use crate::{
    eq_zonked_constraint, fill_ty_names, kind_inference, metavariables,
    type_inference::{self, unify},
    BoundVars, Implication, SolveConstraintContext, TypeError,
};
use ipso_core::{self as core, Binop, CommonKinds, Expr, Placeholder};
use ipso_diagnostic::Source;
use ipso_syntax::kind::Kind;
use std::{collections::HashMap, rc::Rc};

pub fn lookup_evidence(
    type_solutions: &type_inference::unification::Solutions,
    evidence: &Evidence,
    constraint: &Constraint,
) -> Option<Rc<core::Expr>> {
    evidence.environment.iter().find_map(
        |super::Item {
             constraint: other_constraint,
             expr: other_evidence,
             ..
         }| {
            if eq_zonked_constraint(type_solutions, constraint, other_constraint) {
                other_evidence.as_ref().cloned()
            } else {
                None
            }
        },
    )
}

/**
Constraint olver context.

[`solve_placeholder`] and [`solve_constraint`] take many unchanging arguments, so we bundle the
arguments into a struct for convenience.
*/
pub struct Context<'a> {
    pub common_kinds: &'a CommonKinds,
    pub types: &'a HashMap<Rc<str>, Kind>,
    pub kind_inference_ctx: &'a mut kind_inference::State,
    pub type_solutions: &'a mut type_inference::unification::Solutions,
    pub implications: &'a [Implication],
    pub type_variables: &'a BoundVars<Kind>,
    pub evidence: &'a mut Evidence,
    pub source: &'a Source,
}

pub fn solve_constraint(
    ctx: &mut Context,
    pos: usize,
    context: &Option<SolveConstraintContext>,
    constraint: &Constraint,
) -> Result<Rc<core::Expr>, TypeError> {
    match constraint {
        Constraint::Type(constraint) => {
            debug_assert!(
                ctx.kind_inference_ctx
                    .kind_solutions()
                    .zonk(false, constraint.kind())
                    == Kind::Constraint
            );

            match ctx
                .evidence
                .find(ctx.type_solutions, &Constraint::from_type(constraint))
            {
                None => {}
                Some(expr) => {
                    return Ok(expr);
                }
            }

            let mut result = None;
            for implication in ctx.implications.iter() {
                let metas: Vec<core::Type> = implication
                    .ty_vars
                    .iter()
                    .map(|kind| core::Type::Meta(kind.clone(), ctx.type_solutions.fresh_meta()))
                    .collect();

                let implication = implication.instantiate_many(&metas);

                match unify(
                    ctx.common_kinds,
                    ctx.types,
                    ctx.type_variables,
                    ctx.kind_inference_ctx,
                    ctx.type_solutions,
                    ctx.source,
                    None,
                    constraint,
                    &implication.consequent,
                ) {
                    Err(_) => {
                        continue;
                    }
                    Ok(()) => {
                        let implication = Implication {
                            ty_vars: implication.ty_vars,
                            antecedents: implication
                                .antecedents
                                .into_iter()
                                .map(|x| {
                                    ctx.type_solutions
                                        .zonk(ctx.kind_inference_ctx.kind_solutions(), x)
                                })
                                .collect(),
                            consequent: ctx.type_solutions.zonk(
                                ctx.kind_inference_ctx.kind_solutions(),
                                implication.consequent,
                            ),
                            evidence: implication.evidence,
                        };

                        let mut evidence_result = Ok(implication.evidence);

                        for antecedent in &implication.antecedents {
                            match solve_constraint(
                                ctx,
                                pos,
                                context,
                                &Constraint::from_type(antecedent),
                            ) {
                                Err(err) => {
                                    evidence_result = Err(err);
                                    break;
                                }
                                Ok(arg) => {
                                    evidence_result = evidence_result
                                        .map(|evidence| Rc::new(Expr::app(evidence, arg)));
                                }
                            }
                        }

                        match evidence_result {
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
                    pos,
                    source: ctx.source.clone(),
                    context: context.clone(),
                }),
                Some(evidence) => Ok(evidence),
            }
        }
        Constraint::HasField { field, rest } => {
            debug_assert!(
                ctx.kind_inference_ctx
                    .kind_solutions()
                    .zonk(false, rest.kind())
                    == Kind::Row
            );

            let new_evidence: Rc<core::Expr> = match rest {
                core::Type::RowNil => Ok(Rc::new(core::Expr::Int(0))),
                core::Type::RowCons(other_field, _, other_rest) => {
                    if field <= other_field {
                        solve_constraint(
                            ctx,
                            pos,
                            context,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: other_rest.as_ref().clone(),
                            },
                        )
                    } else {
                        let ev = solve_constraint(
                            ctx,
                            pos,
                            context,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: other_rest.as_ref().clone(),
                            },
                        )?;

                        Ok(Rc::new(core::Expr::mk_binop_l(
                            Binop::Add,
                            core::Expr::Int(1),
                            ev,
                        )))
                    }
                }

                core::Type::Name(_, n) => todo!("deduce HasField for Name({})", n),

                core::Type::Var(_, _) => {
                    let evidence_result =
                        lookup_evidence(ctx.type_solutions, ctx.evidence, constraint);
                    match evidence_result {
                        None => {
                            // we're allow to conjure evidence for non-extistent HasField constraints,
                            // so the user doesn't have to write them
                            let ev = ctx.evidence.assume(0, constraint.clone());
                            debug_assert!(ctx.evidence.lookup_evar(&ev) != None);
                            Ok(Rc::new(core::Expr::EVar(ev)))
                        }
                        Some(evidence) => Ok(evidence),
                    }
                }
                core::Type::App(_, _, _) => {
                    let evidence_result =
                        lookup_evidence(ctx.type_solutions, ctx.evidence, constraint);
                    match evidence_result {
                        None => {
                            panic!(
                                "impossible: cannot deduce HasField({:?}, {:?})",
                                field, rest
                            )
                        }
                        Some(evidence) => Ok(evidence),
                    }
                }
                core::Type::Meta(kind, n) => {
                    let sol = ctx.type_solutions.get(*n);
                    // we assume solving is done after unification, so any unsolved variables
                    // will never recieve solutions
                    match sol.clone() {
                        metavariables::Solution::Unsolved => {
                            match ctx
                                .kind_inference_ctx
                                .kind_solutions()
                                .zonk(false, kind.clone())
                            {
                                // row metavariables can be safely defaulted to the empty row in the
                                // presence of ambiguity
                                Kind::Row => {
                                    unify(
                                        ctx.common_kinds,
                                        ctx.types,
                                        ctx.type_variables,
                                        ctx.kind_inference_ctx,
                                        ctx.type_solutions,
                                        ctx.source,
                                        None,
                                        &core::Type::RowNil,
                                        rest,
                                    )?;
                                    solve_constraint(ctx, pos, context, constraint)
                                }
                                _ => match lookup_evidence(
                                    ctx.type_solutions,
                                    ctx.evidence,
                                    constraint,
                                ) {
                                    None => {
                                        panic!(
                                        "impossible: cannot deduce HasField({:?}, {:?}) given {:?}",
                                        field, rest, ctx.evidence
                                    )
                                    }
                                    Some(evidence) => Ok(evidence),
                                },
                            }
                        }
                        metavariables::Solution::Solved(sol) => solve_constraint(
                            ctx,
                            pos,
                            context,
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
    ctx: &mut Context,
    p: Placeholder,
) -> Result<(Rc<core::Expr>, Constraint), TypeError> {
    let item = ctx.evidence.environment[p.0].clone();
    match item.expr {
        None => {
            let expr = solve_constraint(
                ctx,
                item.pos,
                &Some(SolveConstraintContext {
                    constraint: fill_ty_names(
                        ctx.type_variables,
                        ctx.type_solutions
                            .zonk(
                                ctx.kind_inference_ctx.kind_solutions(),
                                item.constraint.to_type(),
                            )
                            .to_syntax(),
                    ),
                }),
                &item.constraint,
            )?;
            ctx.evidence.environment[p.0].expr = Some(expr.clone());
            Ok((expr, item.constraint.clone()))
        }
        Some(evidence) => Ok((evidence, item.constraint.clone())),
    }
}
