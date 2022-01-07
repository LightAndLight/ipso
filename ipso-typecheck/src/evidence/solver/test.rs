#[cfg(test)]
use crate::{
    evidence::{solver::solve_constraint, Constraint, Evidence},
    Typechecker,
};
#[cfg(test)]
use ipso_core::{
    self as core, Builtin, ClassDeclaration, ClassMember, EVar, Expr, InstanceMember, TypeSig,
};
#[cfg(test)]
use ipso_syntax::{kind::Kind, Binop};
#[cfg(test)]
use std::rc::Rc;

#[test]
fn solve_constraint_1() {
    crate::current_dir_with_tc!(|mut tc: Typechecker| {
        let constraint = Constraint::HasField {
            field: Rc::from("x"),
            rest: core::Type::mk_rows(
                vec![
                    (Rc::from("y"), core::Type::Int),
                    (Rc::from("z"), core::Type::Bool),
                ],
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
            rest: core::Type::mk_rows(
                vec![
                    (Rc::from("x"), core::Type::Int),
                    (Rc::from("z"), core::Type::Bool),
                ],
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
        let var = tc.fresh_type_meta(&Kind::Row);
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
            rest: core::Type::mk_rows(
                vec![
                    (Rc::from("x"), core::Type::Int),
                    (Rc::from("y"), core::Type::Bool),
                ],
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
        {
            let a = core::Type::unsafe_mk_var(0, Kind::Type);
            tc.register_class(&ClassDeclaration {
                supers: Vec::new(),
                name: Rc::from("Eq"),
                args: vec![(Rc::from("a"), a.kind())],
                members: vec![ClassMember {
                    name: String::from("eq"),
                    sig: {
                        TypeSig {
                            ty_vars: Vec::new(),
                            body: core::Type::mk_arrow(
                                tc.common_kinds,
                                &a,
                                &core::Type::arrow(tc.common_kinds, a, core::Type::Bool),
                            ),
                        }
                    },
                }],
            });
        }

        let eq_ty = core::Type::unsafe_mk_name(
            Rc::from("Eq"),
            Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
        );
        tc.register_instance(
            &Vec::new(),
            &Vec::new(),
            &Vec::new(),
            &core::Type::app(eq_ty.clone(), core::Type::Int),
            &[InstanceMember {
                name: String::from("Eq"),
                body: Expr::Builtin(Builtin::EqInt),
            }],
        );

        let a = core::Type::unsafe_mk_var(0, Kind::Type);
        tc.register_instance(
            &[(Rc::from("a"), a.kind())],
            &Vec::new(),
            &[core::Type::app(eq_ty.clone(), a.clone())],
            &core::Type::app(
                eq_ty.clone(),
                core::Type::app(core::Type::mk_array(tc.common_kinds), a),
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
        let constraint = &Constraint::from_type(&core::Type::app(
            eq_ty,
            core::Type::app(core::Type::mk_array(tc.common_kinds), core::Type::Int),
        ));
        let actual = solve_constraint(&None, &mut tc, constraint);

        assert_eq!(expected, actual)
    })
}
