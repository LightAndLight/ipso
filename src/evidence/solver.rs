use crate::{
    core,
    syntax::{self, Binop, Kind},
    typecheck::{TypeError, Typechecker},
};

use super::{Constraint, EVar};

mod test;

pub fn lookup_evidence(tc: &Typechecker, constraint: &Constraint) -> Option<core::Expr> {
    tc.evidence
        .0
        .iter()
        .enumerate()
        .find_map(|(ix, (other_constraint, other_evidence))| {
            if tc.eq_zonked_constraint(constraint, other_constraint) {
                match other_evidence {
                    None => Some(core::Expr::mk_evar(ix)),
                    Some(expr) => Some(expr.clone()),
                }
            } else {
                None
            }
        })
}

pub fn solve_constraint<'a>(
    tc: &mut Typechecker,
    constraint: &Constraint,
) -> Result<core::Expr, TypeError> {
    match constraint {
        Constraint::Type(constraint) => {
            todo!("solve constraint {:?}", constraint)
        }
        Constraint::HasField { field, rest } => {
            let _ = tc.check_kind(rest, Kind::Row)?;
            let new_evidence = match rest {
                syntax::Type::RowNil => Ok(core::Expr::Int(0)),
                syntax::Type::RowCons(other_field, _, other_rest) => {
                    if field <= other_field {
                        solve_constraint(
                            tc,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: (**other_rest).clone(),
                            },
                        )
                    } else {
                        let ev = solve_constraint(
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
                            let ev = tc.evidence.fresh_evar(constraint.clone());
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
                    let (_, sol) = &tc.type_solutions[*n];
                    match sol {
                        None => {
                            println!("tys: {:?}", tc.type_solutions);
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
                        Some(sol) => solve_constraint(
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

pub fn solve_evar(tc: &mut Typechecker, ev: EVar) -> Result<(core::Expr, Constraint), TypeError> {
    let (constraint, evidence) = tc.evidence.0[ev.0].clone();
    match evidence.clone() {
        None => {
            let expr = solve_constraint(tc, &constraint)?;
            tc.evidence.0[ev.0].1 = Some(expr.clone());
            Ok((expr, constraint.clone()))
        }
        Some(evidence) => Ok((evidence, constraint.clone())),
    }
}
