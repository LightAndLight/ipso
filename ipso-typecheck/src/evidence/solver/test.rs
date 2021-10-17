#[cfg(test)]
use crate::{
    evidence::{solver::solve_constraint, Constraint, Evidence},
    Typechecker,
};
#[cfg(test)]
use ipso_core::{Builtin, ClassDeclaration, ClassMember, EVar, Expr, InstanceMember, TypeSig};
#[cfg(test)]
use ipso_syntax::{Binop, Kind, Type};
#[cfg(test)]
use std::rc::Rc;

#[test]
fn solve_constraint_1() {
    crate::current_dir_with_tc!(|mut tc: Typechecker| {
        let constraint = Constraint::HasField {
            field: Rc::from("x"),
            rest: Type::mk_rows(
                vec![(Rc::from("y"), Type::Int), (Rc::from("z"), Type::Bool)],
                None,
            ),
        };
        let expected = solve_constraint(&None, &mut tc, &constraint);
        let actual = Ok(Expr::Int(0));
        assert_eq!(expected, actual)
    })
}

#[test]
fn solve_constraint_2() {
    crate::current_dir_with_tc!(|mut tc: Typechecker| {
        let constraint = Constraint::HasField {
            field: Rc::from("y"),
            rest: Type::mk_rows(
                vec![(Rc::from("x"), Type::Int), (Rc::from("z"), Type::Bool)],
                None,
            ),
        };
        let expected = solve_constraint(&None, &mut tc, &constraint);
        let actual = Ok(Expr::mk_binop(Binop::Add, Expr::Int(1), Expr::Int(0)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn solve_constraint_3() {
    crate::current_dir_with_tc!(|mut tc: Typechecker| {
        let var = tc.fresh_typevar(Kind::Row);
        tc.evidence.assume(
            None,
            Constraint::HasField {
                field: Rc::from("z"),
                rest: var.clone(),
            },
        );
        // HasField "z" (x : Int, y : Bool, ?0)
        let constraint = Constraint::HasField {
            field: Rc::from("z"),
            rest: Type::mk_rows(
                vec![(Rc::from("x"), Type::Int), (Rc::from("y"), Type::Bool)],
                Some(var.clone()),
            ),
        };

        let expected_result = Ok(Expr::Int(2));
        let actual_result = solve_constraint(&None, &mut tc, &constraint);
        assert_eq!(expected_result, actual_result);

        let expected_evidence = Evidence {
            evars: vec![Constraint::HasField {
                field: Rc::from("z"),
                rest: var.clone(),
            }],
            environment: vec![(
                Constraint::HasField {
                    field: Rc::from("z"),
                    rest: var,
                },
                None,
                Some(Expr::EVar(EVar(0))),
            )],
        };
        let actual_evidence = tc.evidence;
        assert_eq!(expected_evidence, actual_evidence)
    })
}

#[test]
fn solve_constraint_4() {
    crate::current_dir_with_tc!(|mut tc: Typechecker| {
        tc.register_class(&ClassDeclaration {
            supers: Vec::new(),
            name: Rc::from("Eq"),
            args: vec![(Rc::from("a"), Kind::Type)],
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
            &Type::mk_app(Type::Name(Rc::from("Eq")), Type::Int),
            &[InstanceMember {
                name: String::from("Eq"),
                body: Expr::Builtin(Builtin::EqInt),
            }],
        );

        tc.register_instance(
            &[(Rc::from("a"), Kind::Type)],
            &Vec::new(),
            &[Type::mk_app(Type::Name(Rc::from("Eq")), Type::Var(0))],
            &Type::mk_app(
                Type::Name(Rc::from("Eq")),
                Type::mk_app(Type::Array, Type::Var(0)),
            ),
            &[InstanceMember {
                name: String::from("Eq"),
                body: Expr::Builtin(Builtin::EqArray),
            }],
        );

        let expected = Ok(Expr::mk_record(
            vec![(
                Expr::Int(0),
                Expr::mk_app(
                    Expr::Builtin(Builtin::EqArray),
                    Expr::mk_record(vec![(Expr::Int(0), Expr::Builtin(Builtin::EqInt))], None),
                ),
            )],
            None,
        ));
        let actual = solve_constraint(
            &None,
            &mut tc,
            &Constraint::from_type(&Type::mk_app(
                Type::Name(Rc::from("Eq")),
                Type::mk_app(Type::Array, Type::Int),
            )),
        );

        assert_eq!(expected, actual)
    })
}
