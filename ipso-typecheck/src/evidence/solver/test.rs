use crate::{
    evidence::{solver, Constraint, Evidence},
    register_class, register_instance, type_inference, SolveConstraintContext, TypeError,
};
use ipso_core::{self as core, Binop, ClassDeclaration, ClassMember, EVar, Expr, Name, TypeSig};
use ipso_diagnostic::Source;
use ipso_syntax::kind::Kind;
use std::rc::Rc;

fn solve_constraint(
    pos: usize,
    context: &Option<SolveConstraintContext>,
    constraint: &Constraint,
) -> std::result::Result<std::rc::Rc<ipso_core::Expr>, TypeError> {
    let common_kinds = Default::default();
    let types = Default::default();
    let type_variables = Default::default();
    let mut evidence = Default::default();
    let mut variables = Default::default();
    let mut type_inference_state = type_inference::State::new(&mut variables, &mut evidence);
    let implications = Default::default();
    let source = Source::Interactive {
        label: String::from("test"),
    };
    solver::solve_constraint(
        &mut solver::Context {
            common_kinds: &common_kinds,
            types: &types,
            type_inference_state: &mut type_inference_state,
            implications,
            type_variables: &type_variables,
            source: &source,
        },
        pos,
        context,
        constraint,
    )
}

#[test]
fn solve_constraint_1() {
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
    let actual = solve_constraint(0, &None, &constraint);
    assert_eq!(expected, actual)
}

#[test]
fn solve_constraint_2() {
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
    let actual = solve_constraint(0, &None, &constraint);
    assert_eq!(expected, actual)
}

#[test]
fn solve_constraint_3() {
    let common_kinds = Default::default();
    let types = Default::default();
    let type_variables = Default::default();
    let mut variables = Default::default();
    let mut evidence = Evidence::default();
    let mut type_inference_state = type_inference::State::new(&mut variables, &mut evidence);
    let implications = Default::default();
    let source = Source::Interactive {
        label: String::from("test"),
    };

    let var = type_inference_state.fresh_type_meta(Kind::Row);
    type_inference_state.evidence.assume(
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
    let actual_result = solver::solve_constraint(
        &mut solver::Context {
            common_kinds: &common_kinds,
            types: &types,
            type_inference_state: &mut type_inference_state,
            implications,
            type_variables: &type_variables,
            source: &source,
        },
        0,
        &None,
        &constraint,
    );
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
    let actual_evidence = evidence;
    assert_eq!(expected_evidence, actual_evidence)
}

#[test]
fn solve_constraint_4() {
    let common_kinds = Default::default();
    let mut types = Default::default();
    let type_variables = Default::default();
    let mut evidence = Default::default();
    let mut variables = Default::default();
    let mut type_inference_state = type_inference::State::new(&mut variables, &mut evidence);
    let mut implications = Default::default();
    let source = Source::Interactive {
        label: String::from("test"),
    };

    let mut context = Default::default();
    let mut class_context = Default::default();

    {
        let a = core::Type::unsafe_mk_var(0, Kind::Type);
        register_class(
            &common_kinds,
            &mut types,
            &mut implications,
            &mut context,
            &mut class_context,
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
                                &common_kinds,
                                a.clone(),
                                core::Type::arrow(&common_kinds, a, core::Type::Bool),
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
        &mut implications,
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
        &mut implications,
        None,
        &[(Rc::from("a"), a.kind())],
        &[core::Type::app(eq_ty.clone(), a.clone())],
        &core::Type::app(
            eq_ty.clone(),
            core::Type::app(core::Type::mk_array(&common_kinds), a),
        ),
        Rc::from("MyEq a => MyEq (Array a)"),
    );

    let expected = Ok(Rc::new(Expr::mk_app(
        Expr::Name(Name::evidence("MyEq a => MyEq (Array a)")),
        Expr::Name(Name::evidence("MyEq Int")),
    )));
    let constraint = &Constraint::from_type(&core::Type::app(
        eq_ty,
        core::Type::app(core::Type::mk_array(&common_kinds), core::Type::Int),
    ));
    let actual = solver::solve_constraint(
        &mut solver::Context {
            common_kinds: &common_kinds,
            types: &types,
            type_inference_state: &mut type_inference_state,
            implications: &implications,
            type_variables: &type_variables,
            source: &source,
        },
        0,
        &None,
        constraint,
    );

    assert_eq!(expected, actual)
}
