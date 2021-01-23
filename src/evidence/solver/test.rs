#[cfg(test)]
use crate::{
    core::{EVar, Expr},
    evidence::{solver::solve_constraint, Constraint, Evidence},
    syntax::{Binop, Kind, Type},
    typecheck::Typechecker,
};

#[test]
fn solve_constraint_1() {
    let mut tc = Typechecker::new();
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
    let expected = solve_constraint(&mut tc, &constraint);
    let actual = Ok(Expr::Int(0));
    assert_eq!(expected, actual)
}

#[test]
fn solve_constraint_2() {
    let mut tc = Typechecker::new();
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
    let expected = solve_constraint(&mut tc, &constraint);
    let actual = Ok(Expr::mk_binop(Binop::Add, Expr::Int(1), Expr::Int(0)));
    assert_eq!(expected, actual)
}

#[test]
fn solve_constraint_3() {
    let mut tc = Typechecker::new();
    let var = tc.fresh_typevar(Kind::Row);
    tc.evidence.assume(Constraint::HasField {
        field: String::from("z"),
        rest: var.clone(),
    });
    // HasField "z" (x : Int, y : Bool, ?0)
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

    let expected_result = Ok(Expr::Int(2));
    let actual_result = solve_constraint(&mut tc, &constraint);
    assert_eq!(expected_result, actual_result);

    let expected_evidence = Evidence {
        evars: 1,
        environment: vec![(
            Constraint::HasField {
                field: String::from("z"),
                rest: var,
            },
            Some(Expr::EVar(EVar(0))),
        )],
    };
    let actual_evidence = tc.evidence;
    assert_eq!(expected_evidence, actual_evidence)
}