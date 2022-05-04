use crate::{
    evidence::{solver::solve_constraint, Constraint, Evidence},
    register_class, register_instance, Typechecker,
};
use ipso_core::{self as core, Binop, ClassDeclaration, ClassMember, EVar, Expr, Name, TypeSig};
use ipso_syntax::kind::Kind;
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
        let expected = Ok(Rc::new(Expr::Int(0)));
        let actual = solve_constraint(0, &None, &mut tc, &constraint);
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
        let expected = Ok(Rc::new(Expr::mk_binop(
            Binop::Add,
            Expr::Int(1),
            Expr::Int(0),
        )));
        let actual = solve_constraint(0, &None, &mut tc, &constraint);
        assert_eq!(expected, actual)
    })
}

#[test]
fn solve_constraint_3() {
    crate::current_dir_with_tc!(|mut tc: Typechecker| {
        let var = tc.fresh_type_meta(&Kind::Row);
        tc.evidence.assume(
            0,
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

        let expected_result = Ok(Rc::new(Expr::Int(2)));
        let actual_result = solve_constraint(0, &None, &mut tc, &constraint);
        assert_eq!(expected_result, actual_result);

        let expected_evidence = Evidence {
            evars: vec![Constraint::HasField {
                field: Rc::from("z"),
                rest: var.clone(),
            }],
            environment: vec![crate::evidence::Item {
                pos: 0,
                constraint: Constraint::HasField {
                    field: Rc::from("z"),
                    rest: var,
                },
                expr: Some(Rc::new(Expr::EVar(EVar(0)))),
            }],
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
            register_class(
                tc.common_kinds,
                &mut tc.type_context,
                &mut tc.implications,
                &mut tc.context,
                &mut tc.class_context,
                &ClassDeclaration {
                    supers: Vec::new(),
                    name: Rc::from("MyEq"),
                    args: vec![(Rc::from("a"), a.kind())],
                    members: vec![ClassMember {
                        name: String::from("myeq"),
                        sig: {
                            TypeSig {
                                ty_vars: Vec::new(),
                                body: core::Type::arrow(
                                    tc.common_kinds,
                                    a.clone(),
                                    core::Type::arrow(tc.common_kinds, a, core::Type::Bool),
                                ),
                            }
                        },
                    }],
                },
            );
        }

        let eq_ty = core::Type::unsafe_mk_name(
            Rc::from("MyEq"),
            Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
        );
        register_instance(
            &mut tc.implications,
            None,
            &Vec::new(),
            &Vec::new(),
            &core::Type::app(eq_ty.clone(), core::Type::Int),
            /*
            Rc::new(Expr::mk_record(
                vec![(Expr::Int(0), Expr::Builtin(Builtin::EqInt))],
                None,
            )),
             */
            Rc::from("MyEq Int"),
        );

        let a = core::Type::unsafe_mk_var(0, Kind::Type);
        register_instance(
            &mut tc.implications,
            None,
            &[(Rc::from("a"), a.kind())],
            &[core::Type::app(eq_ty.clone(), a.clone())],
            &core::Type::app(
                eq_ty.clone(),
                core::Type::app(core::Type::mk_array(tc.common_kinds), a),
            ),
            /*
            Rc::new(Expr::mk_lam(
                true,
                Expr::mk_record(
                    vec![(
                        Expr::Int(0),
                        Expr::mk_app(
                            Expr::Builtin(Builtin::EqArray),
                            Expr::mk_app(Expr::Name(Name::definition("myeq")), Expr::Var(0)),
                        ),
                    )],
                    None,
                ),
            )),
             */
            Rc::from("MyEq a => MyEq (Array a)"),
        );

        let expected = Ok(Rc::new(Expr::mk_app(
            Expr::Name(Name::evidence("MyEq a => MyEq (Array a)")),
            Expr::Name(Name::evidence("MyEq Int")),
        )));
        let constraint = &Constraint::from_type(&core::Type::app(
            eq_ty,
            core::Type::app(core::Type::mk_array(tc.common_kinds), core::Type::Int),
        ));
        let actual = solve_constraint(0, &None, &mut tc, constraint);

        assert_eq!(expected, actual)
    })
}
