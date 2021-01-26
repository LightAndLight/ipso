use crate::{
    core::{self, Expr, Placeholder},
    syntax::{self, Binop, Kind, Type},
    typecheck::{Implication, TypeError, Typechecker, UnifyTypeContext},
};

use super::Constraint;

mod test;

pub fn lookup_evidence(tc: &Typechecker, constraint: &Constraint) -> Option<core::Expr> {
    tc.evidence.environment.iter().enumerate().find_map(
        |(ix, (other_constraint, other_evidence))| {
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

pub fn lookup_implication(tc: &mut Typechecker, constraint: &Type<usize>) -> Option<Implication> {
    let (head, args) = constraint.unwrap_app();
    let args: Vec<Type<usize>> = args.into_iter().map(|x| x.clone()).collect();
    tc.implications.iter().find_map(|implication| {
        let (i_head, i_args) = implication.consequent.unwrap_app();
        if head == i_head {
            debug_assert!(args.len() == i_args.len());
            for (arg_ty, i_arg_ty) in args.iter().zip(i_args.iter()) {
                let (arg_ty_head, arg_ty_args) = arg_ty.unwrap_app();
                let (i_arg_ty_head, i_arg_ty_args) = i_arg_ty.unwrap_app();
                if arg_ty_head != i_arg_ty_head {
                    return None;
                }
            }
            Some(implication.instantiate_many(&args))
        } else {
            None
        }
    })
}

pub struct SolveConstraintContext {
    pub pos: usize,
}

pub fn solve_constraint(
    context: &Option<SolveConstraintContext>,
    tc: &mut Typechecker,
    constraint: &Constraint,
) -> Result<core::Expr, TypeError> {
    println!("constraint {:?}", constraint);
    match constraint {
        Constraint::Type(constraint) => {
            let _ = tc.check_kind(None, constraint, Kind::Constraint)?;
            match lookup_implication(tc, constraint) {
                None => Err(TypeError::CannotDeduce {
                    pos: match context {
                        None => 0,
                        Some(context) => context.pos,
                    },
                }),
                Some(implication) => {
                    let mut evidence = implication.evidence;
                    for antecedent in &implication.antecedents {
                        match solve_constraint(context, tc, &Constraint::from_type(antecedent)) {
                            Err(err) => return Err(err),
                            Ok(arg) => {
                                evidence = Expr::mk_app(evidence, arg);
                            }
                        }
                    }
                    Ok(evidence)
                }
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
                            let ev = tc.evidence.assume(constraint.clone());
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
    context: &Option<SolveConstraintContext>,
    tc: &mut Typechecker,
    p: Placeholder,
) -> Result<(core::Expr, Constraint), TypeError> {
    let (constraint, evidence) = tc.evidence.environment[p.0].clone();
    match evidence.clone() {
        None => {
            let expr = solve_constraint(context, tc, &constraint)?;
            tc.evidence.environment[p.0].1 = Some(expr.clone());
            Ok((expr, constraint.clone()))
        }
        Some(evidence) => Ok((evidence, constraint.clone())),
    }
}
