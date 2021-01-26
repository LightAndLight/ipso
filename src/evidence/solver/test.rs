#[cfg(test)]
use crate::{
    core::{Builtin, ClassDeclaration, ClassMember, EVar, Expr, InstanceMember, TypeSig},
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
    let expected = solve_constraint(&None, &mut tc, &constraint);
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
    let expected = solve_constraint(&None, &mut tc, &constraint);
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
    let actual_result = solve_constraint(&None, &mut tc, &constraint);
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

#[test]
fn solve_constraint_4() {
    let mut tc = Typechecker::new_with_builtins();

    tc.register_class(&ClassDeclaration {
        supers: Vec::new(),
        name: String::from("Eq"),
        args: vec![(String::from("a"), Kind::Type)],
        members: vec![ClassMember {
            name: String::from("eq"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::mk_arrow(Type::Var(0), Type::mk_arrow(Type::Var(0), Type::Bool)),
            },
        }],
    });

    tc.register_instance(
        &Vec::new(),
        &Vec::new(),
        &Vec::new(),
        &Type::mk_app(Type::Name(String::from("Eq")), Type::Int),
        &vec![InstanceMember {
            name: String::from("Eq"),
            body: Expr::Builtin(Builtin::EqInt),
        }],
    );

    tc.register_instance(
        &vec![(String::from("a"), Kind::Type)],
        &Vec::new(),
        &vec![Type::mk_app(Type::Name(String::from("Eq")), Type::Var(0))],
        &Type::mk_app(
            Type::Name(String::from("Eq")),
            Type::mk_app(Type::Array, Type::Var(0)),
        ),
        &vec![InstanceMember {
            name: String::from("Eq"),
            body: Expr::Builtin(Builtin::EqArray),
        }],
    );

    println!("impls: {:?}", tc.implications);

    assert_eq!(
        solve_constraint(
            &None,
            &mut tc,
            &Constraint::from_type(&Type::mk_app(
                Type::Name(String::from("Eq")),
                Type::mk_app(Type::Array, Type::Int)
            ))
        ),
        Ok(Expr::mk_app(
            Expr::Builtin(Builtin::EqArray),
            Expr::Builtin(Builtin::EqInt)
        ))
    )
}
