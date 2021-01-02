#[cfg(test)]
use super::solve_constraint;
#[cfg(test)]
use crate::{core::Expr, evidence::Constraint, syntax::Type, typecheck::Typechecker};
use crate::{
    evidence::Evidence,
    syntax::{Binop, Kind},
};

#[test]
fn solve_constraint_1() {
    let mut tc = Typechecker::new();
    let implications = Vec::new();
    let constraint = Constraint::HasField {
        field: String::from("x"),
        rest: Type::mk_rows(
            vec![
                (String::from("y"), Type::Int),
                (String::from("z"), Type::Bool),
            ],
            None,
        ),
    };
    let expected = solve_constraint(&mut tc, &implications, &constraint);
    let actual = Ok(Expr::Int(0));
    assert_eq!(expected, actual)
}

#[test]
fn solve_constraint_2() {
    let mut tc = Typechecker::new();
    let implications = Vec::new();
    let constraint = Constraint::HasField {
        field: String::from("y"),
        rest: Type::mk_rows(
            vec![
                (String::from("x"), Type::Int),
                (String::from("z"), Type::Bool),
            ],
            None,
        ),
    };
    let expected = solve_constraint(&mut tc, &implications, &constraint);
    let actual = Ok(Expr::mk_binop(Binop::Add, Expr::Int(1), Expr::Int(0)));
    assert_eq!(expected, actual)
}

#[test]
fn solve_constraint_3() {
    let mut tc = Typechecker::new();
    let var = tc.fresh_typevar(Kind::Row);
    let implications = Vec::new();
    let constraint = Constraint::HasField {
        field: String::from("z"),
        rest: Type::mk_rows(
            vec![
                (String::from("x"), Type::Int),
                (String::from("y"), Type::Bool),
            ],
            Some(var.clone()),
        ),
    };

    let expected_result = solve_constraint(&mut tc, &implications, &constraint);
    let actual_result = Ok(Expr::mk_binop(
        Binop::Add,
        Expr::Int(1),
        Expr::mk_binop(Binop::Add, Expr::Int(1), Expr::mk_evar(0)),
    ));
    assert_eq!(expected_result, actual_result);

    let expected_evidence = Evidence(vec![(
        Constraint::HasField {
            field: String::from("z"),
            rest: var,
        },
        None,
    )]);
    let actual_evidence = tc.evidence;
    assert_eq!(expected_evidence, actual_evidence)
}
