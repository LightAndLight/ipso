use crate::{
    core,
    syntax::{Binop, Type},
    typecheck::Typechecker,
};
use crate::{
    syntax::{self, Kind},
    typecheck::TypeError,
};

use super::{Constraint, EVar};

mod test;

pub struct Implication {
    pub ty_vars: Vec<syntax::Kind>,
    pub antecedents: Vec<syntax::Type<usize>>,
    pub consequent: syntax::Type<usize>,
    pub evidence: core::Expr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum SolveError {
    TypeError(TypeError),
}

impl From<TypeError> for SolveError {
    fn from(err: TypeError) -> Self {
        SolveError::TypeError(err)
    }
}

fn eq_zonked_type(tc: &Typechecker, t1: &Type<usize>, t2: &Type<usize>) -> bool {
    fn zonk_just_enough<'a>(tc: &'a Typechecker, t: &'a Type<usize>) -> &'a Type<usize> {
        match t {
            Type::Meta(n) => match &tc.type_solutions[*n].1 {
                None => t,
                Some(sol) => zonk_just_enough(tc, sol),
            },
            t => t,
        }
    }
    let t2: &Type<usize> = zonk_just_enough(tc, t2);
    match t1 {
        Type::Name(n) => match t2 {
            Type::Name(n2) => n == n2,
            _ => false,
        },
        Type::Var(v) => match t2 {
            Type::Var(v2) => v == v2,
            _ => false,
        },
        Type::Bool => match t2 {
            Type::Bool => true,
            _ => false,
        },
        Type::Int => match t2 {
            Type::Int => true,
            _ => false,
        },
        Type::Char => match t2 {
            Type::Char => true,
            _ => false,
        },
        Type::String => match t2 {
            Type::String => true,
            _ => false,
        },
        Type::Bytes => match t2 {
            Type::Bytes => true,
            _ => false,
        },
        Type::Arrow => match t2 {
            Type::Arrow => true,
            _ => false,
        },
        Type::FatArrow => match t2 {
            Type::FatArrow => true,
            _ => false,
        },
        Type::Array => match t2 {
            Type::Arrow => true,
            _ => false,
        },
        Type::Record => match t2 {
            Type::Record => true,
            _ => false,
        },
        Type::Variant => match t2 {
            Type::Variant => true,
            _ => false,
        },
        Type::IO => match t2 {
            Type::IO => true,
            _ => false,
        },
        Type::RowNil => match t2 {
            Type::RowNil => true,
            _ => false,
        },
        Type::Unit => match t2 {
            Type::Unit => true,
            _ => false,
        },
        Type::Constraints(cs) => match t2 {
            Type::Constraints(cs2) => {
                cs.len() == cs2.len()
                    && cs
                        .iter()
                        .zip(cs2.iter())
                        .all(|(a, b)| eq_zonked_type(tc, a, b))
            }
            _ => false,
        },
        Type::App(a, b) => match t2 {
            Type::App(a2, b2) => eq_zonked_type(tc, a, a2) && eq_zonked_type(tc, b, b2),
            _ => false,
        },
        Type::RowCons(a, b, c) => match t2 {
            Type::RowCons(a2, b2, c2) => {
                a == a2 && eq_zonked_type(tc, b, b2) && eq_zonked_type(tc, c, c2)
            }
            _ => false,
        },
        Type::Meta(n) => match &tc.type_solutions[*n].1 {
            None => match t2 {
                Type::Meta(n2) => n == n2,
                _ => false,
            },
            Some(sol) => eq_zonked_type(tc, sol, t2),
        },
    }
}

fn eq_zonked_constraint(tc: &Typechecker, c1: &Constraint, c2: &Constraint) -> bool {
    match c1 {
        Constraint::HasField { field, rest } => match c2 {
            Constraint::HasField {
                field: field2,
                rest: rest2,
            } => field == field2 && eq_zonked_type(tc, rest, rest2),
        },
    }
}

pub fn solve_constraint<'a>(
    tc: &mut Typechecker,
    implications: &Vec<Implication>,
    constraint: &Constraint,
) -> Result<core::Expr, SolveError> {
    match constraint {
        Constraint::HasField { field, rest } => {
            let _ = tc.check_kind(rest, Kind::Row)?;
            let new_evidence = match rest {
                syntax::Type::RowNil => Ok(core::Expr::Int(0)),
                syntax::Type::RowCons(other_field, _, other_rest) => {
                    if field <= other_field {
                        solve_constraint(
                            tc,
                            implications,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: (**other_rest).clone(),
                            },
                        )
                    } else {
                        let ev = solve_constraint(
                            tc,
                            implications,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: (**other_rest).clone(),
                            },
                        )?;
                        Ok(core::Expr::mk_binop(Binop::Add, core::Expr::Int(1), ev))
                    }
                }

                syntax::Type::Name(n) => todo!("deduce HasField for Name({})", n),

                syntax::Type::Var(_) | syntax::Type::App(_, _) => {
                    let evidence =
                        tc.evidence
                            .0
                            .iter()
                            .find_map(|(other_constraint, other_evidence)| {
                                if eq_zonked_constraint(tc, constraint, other_constraint) {
                                    other_evidence.clone()
                                } else {
                                    None
                                }
                            });
                    match evidence {
                        None => {
                            panic!("impossible: cannot deduce HasField({:?}, {:?}", field, rest)
                        }
                        Some(evidence) => Ok(evidence),
                    }
                }
                syntax::Type::Meta(n) => {
                    let (_, sol) = &tc.type_solutions[*n];
                    match sol {
                        None => Ok(core::Expr::EVar(tc.evidence.fresh_evar(
                            Constraint::HasField {
                                field: field.clone(),
                                rest: rest.clone(),
                            },
                        ))),
                        Some(sol) => solve_constraint(
                            tc,
                            implications,
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

pub fn solve_evar(
    tc: &mut Typechecker,
    implications: &Vec<Implication>,
    ev: EVar,
) -> Result<core::Expr, SolveError> {
    let (constraint, evidence) = tc.evidence.0[ev.0].clone();
    match evidence.clone() {
        None => solve_constraint(tc, implications, &constraint),
        Some(evidence) => Ok(evidence),
    }
}
