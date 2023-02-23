use crate::constraint_solving::{self, solve_placeholder};
use crate::declaration;
use crate::{evidence::Constraint, kind_inference, module, type_inference, BoundVars};
use ipso_core::{self as core, ClassMember, CommonKinds, Placeholder, Signature, TypeSig};
use ipso_diagnostic::Source;
use ipso_syntax::{self as syntax, kind::Kind, r#type::Type, InstanceMember, Spanned};
use pretty_assertions::assert_eq;
use std::collections::HashSet;
use std::rc::Rc;
use syntax::desugar::desugar_decl;

pub enum Void {}

fn zonk_constraint(
    kind_solutions: &kind_inference::unification::Solutions,
    type_solutions: &type_inference::unification::Solutions,
    constraint: &Constraint,
) -> Constraint {
    match constraint {
        Constraint::HasField { field, rest } => Constraint::HasField {
            field: field.clone(),
            rest: type_solutions.zonk(kind_solutions, rest.clone()),
        },
        Constraint::DebugRecordFields(ty) => {
            Constraint::DebugRecordFields(type_solutions.zonk(kind_solutions, ty.clone()))
        }
        Constraint::DebugVariantCtor(ty) => {
            Constraint::DebugVariantCtor(type_solutions.zonk(kind_solutions, ty.clone()))
        }
        Constraint::Type(ty) => Constraint::Type(type_solutions.zonk(kind_solutions, ty.clone())),
    }
}

#[test]
fn context_test_1() {
    let mut ctx = BoundVars::new();
    ctx.insert(&[
        (Rc::from("a"), Type::Unit::<usize>),
        (Rc::from("b"), Type::Bool),
        (Rc::from("c"), Type::String),
    ]);
    assert_eq!(
        ctx,
        BoundVars {
            indices: vec![
                (Rc::from("a"), vec![2]),
                (Rc::from("b"), vec![1]),
                (Rc::from("c"), vec![0])
            ]
            .into_iter()
            .collect(),
            info: vec![
                (Rc::from("a"), Type::Unit),
                (Rc::from("b"), Type::Bool),
                (Rc::from("c"), Type::String),
            ]
        }
    );
    assert_eq!(ctx.lookup_name(&String::from("a")), Some((2, &Type::Unit)));
    assert_eq!(ctx.lookup_name(&String::from("b")), Some((1, &Type::Bool)));
    assert_eq!(
        ctx.lookup_name(&String::from("c")),
        Some((0, &Type::String))
    );
}

#[test]
#[should_panic]
fn context_test_2() {
    let mut ctx = BoundVars::new();
    ctx.insert(&[
        (Rc::from("a"), Type::Unit::<usize>),
        (Rc::from("a"), Type::Bool),
        (Rc::from("c"), Type::String),
    ]);
}

#[test]
fn context_test_3() {
    let mut ctx = BoundVars::new();
    ctx.insert(&[(Rc::from("a"), Type::Unit::<usize>)]);
    assert_eq!(
        ctx,
        BoundVars {
            indices: vec![(Rc::from("a"), vec![0]),].into_iter().collect(),
            info: vec![(Rc::from("a"), Type::Unit),]
        }
    );
    ctx.insert(&[(Rc::from("b"), Type::Bool)]);
    assert_eq!(
        ctx,
        BoundVars {
            indices: vec![(Rc::from("a"), vec![1]), (Rc::from("b"), vec![0]),]
                .into_iter()
                .collect(),
            info: vec![(Rc::from("a"), Type::Unit), (Rc::from("b"), Type::Bool),]
        }
    );
    ctx.insert(&[(Rc::from("c"), Type::String)]);
    assert_eq!(
        ctx,
        BoundVars {
            indices: vec![
                (Rc::from("a"), vec![2]),
                (Rc::from("b"), vec![1]),
                (Rc::from("c"), vec![0])
            ]
            .into_iter()
            .collect(),
            info: vec![
                (Rc::from("a"), Type::Unit),
                (Rc::from("b"), Type::Bool),
                (Rc::from("c"), Type::String),
            ]
        }
    );
}

#[test]
fn context_test_4() {
    let mut ctx = BoundVars::new();
    ctx.insert(&[(Rc::from("a"), Type::Unit::<usize>)]);
    assert_eq!(
        ctx,
        BoundVars {
            indices: vec![(Rc::from("a"), vec![0]),].into_iter().collect(),
            info: vec![(Rc::from("a"), Type::Unit),]
        }
    );
    ctx.delete(1);
    assert_eq!(ctx, BoundVars::new())
}

#[test]
fn context_test_5() {
    let mut ctx = BoundVars::new();
    ctx.insert(&[(Rc::from("a"), Type::Unit::<usize>)]);
    ctx.insert(&[(Rc::from("b"), Type::Bool)]);
    assert_eq!(
        ctx,
        BoundVars {
            indices: vec![(Rc::from("a"), vec![1]), (Rc::from("b"), vec![0])]
                .into_iter()
                .collect(),
            info: vec![(Rc::from("a"), Type::Unit), (Rc::from("b"), Type::Bool)]
        }
    );
    ctx.delete(1);
    assert_eq!(
        ctx,
        BoundVars {
            indices: vec![(Rc::from("a"), vec![0]),].into_iter().collect(),
            info: vec![(Rc::from("a"), Type::Unit),]
        }
    )
}

#[test]
fn context_test_6() {
    let mut ctx = BoundVars::new();
    ctx.insert(&[(Rc::from("a"), Type::Unit::<usize>)]);
    ctx.insert(&[(Rc::from("b"), Type::Bool)]);
    assert_eq!(
        ctx,
        BoundVars {
            indices: vec![(Rc::from("a"), vec![1]), (Rc::from("b"), vec![0])]
                .into_iter()
                .collect(),
            info: vec![(Rc::from("a"), Type::Unit), (Rc::from("b"), Type::Bool)]
        }
    );
    ctx.delete(2);
    assert_eq!(ctx, BoundVars::new())
}

#[test]
fn infer_record_1() {
    let common_kinds = Default::default();
    let modules = Default::default();
    let types = Default::default();
    let type_variables = Default::default();
    let implications = Default::default();
    let type_signatures = Default::default();
    let source = Source::Interactive {
        label: String::from("test"),
    };

    let expected_expr = core::Expr::mk_record(
        vec![
            (core::Expr::Placeholder(Placeholder(0)), core::Expr::False),
            (
                core::Expr::Placeholder(Placeholder(1)),
                core::Expr::String(Vec::new()),
            ),
            (core::Expr::Placeholder(Placeholder(2)), core::Expr::Int(0)),
        ],
        None,
    );
    let expected_ty = core::Type::mk_record(
        &common_kinds,
        vec![
            (Rc::from("z"), core::Type::Bool),
            (Rc::from("y"), core::Type::String),
            (Rc::from("x"), core::Type::Int),
        ],
        None,
    );
    let expected_result = Ok((expected_expr, expected_ty));
    // { z = False, y = "", x = 0 }
    let expr = syntax::Spanned {
        pos: 0,
        item: syntax::Expr::mk_record(
            vec![
                (
                    String::from("z"),
                    syntax::Spanned {
                        pos: 1,
                        item: syntax::Expr::False,
                    },
                ),
                (
                    String::from("y"),
                    syntax::Spanned {
                        pos: 2,
                        item: syntax::Expr::String(Vec::new()),
                    },
                ),
                (
                    String::from("x"),
                    syntax::Spanned {
                        pos: 3,
                        item: syntax::Expr::Int(0),
                    },
                ),
            ],
            None,
        ),
    };
    let mut type_inference_state = type_inference::State::new();
    let actual_result = type_inference::infer(
        type_inference::Env {
            common_kinds: &common_kinds,
            modules: &modules,
            types: &types,
            type_variables: &type_variables,
            type_signatures: &type_signatures,
            source: &source,
        },
        &mut type_inference_state,
        &expr,
    )
    .map(|(expr, ty)| (expr, type_inference_state.zonk_type(ty)));
    assert_eq!(expected_result, actual_result, "checking results");

    let (mut actual_expr, _actual_ty) = actual_result.unwrap();

    let mut placeholders: HashSet<Placeholder> = HashSet::new();
    let _: Result<(), Void> = actual_expr.subst_placeholder(&mut |p| {
        placeholders.insert(*p);
        Ok(core::Expr::Placeholder(*p))
    });

    assert_eq!(3, placeholders.len(), "checking number of Placeholders");

    let p0 = placeholders.get(&Placeholder(0)).unwrap();
    let p1 = placeholders.get(&Placeholder(1)).unwrap();
    let p2 = placeholders.get(&Placeholder(2)).unwrap();

    assert_eq!(
        Ok((
            Rc::new(core::Expr::Int(2)),
            Constraint::HasField {
                field: Rc::from("z"),
                rest: core::Type::mk_rows(
                    vec![
                        (Rc::from("z"), core::Type::Bool),
                        (Rc::from("y"), core::Type::String),
                        (Rc::from("x"), core::Type::Int)
                    ],
                    None
                )
            }
        )),
        solve_placeholder(
            constraint_solving::Env {
                common_kinds: &common_kinds,
                types: &types,
                implications,
                type_variables: &type_variables,
                source: &source,
            },
            &mut type_inference_state,
            *p0
        )
        .map(|(expr, constraint)| (
            expr,
            zonk_constraint(
                &type_inference_state.kind_inference_state.kind_solutions,
                &type_inference_state.type_solutions,
                &constraint
            )
        ))
    );

    assert_eq!(
        Ok((
            Rc::new(core::Expr::Int(1)),
            Constraint::HasField {
                field: Rc::from("y"),
                rest: core::Type::mk_rows(
                    vec![
                        (Rc::from("z"), core::Type::Bool),
                        (Rc::from("y"), core::Type::String),
                        (Rc::from("x"), core::Type::Int)
                    ],
                    None
                )
            }
        )),
        solve_placeholder(
            constraint_solving::Env {
                common_kinds: &common_kinds,
                types: &types,
                implications,
                type_variables: &type_variables,
                source: &source,
            },
            &mut type_inference_state,
            *p1
        )
        .map(|(expr, constraint)| (
            expr,
            zonk_constraint(
                &type_inference_state.kind_inference_state.kind_solutions,
                &type_inference_state.type_solutions,
                &constraint
            )
        ))
    );

    assert_eq!(
        Ok((
            Rc::new(core::Expr::Int(0)),
            Constraint::HasField {
                field: Rc::from("x"),
                rest: core::Type::mk_rows(
                    vec![
                        (Rc::from("z"), core::Type::Bool),
                        (Rc::from("y"), core::Type::String),
                        (Rc::from("x"), core::Type::Int)
                    ],
                    None
                )
            }
        )),
        solve_placeholder(
            constraint_solving::Env {
                common_kinds: &common_kinds,
                types: &types,
                implications,
                type_variables: &type_variables,
                source: &source,
            },
            &mut type_inference_state,
            *p2
        )
        .map(|(expr, constraint)| (
            expr,
            zonk_constraint(
                &type_inference_state.kind_inference_state.kind_solutions,
                &type_inference_state.type_solutions,
                &constraint
            )
        ))
    );
}

fn check_declaration(
    decl: &Spanned<syntax::Declaration>,
) -> std::result::Result<declaration::Checked, crate::Error> {
    let common_kinds = Default::default();
    let types = Default::default();
    let implications = Default::default();
    let context = Default::default();
    let class_context = Default::default();
    let modules = Default::default();
    let module_context = Default::default();
    let source = Source::Interactive {
        label: String::from("test"),
    };
    crate::declaration::check(
        crate::declaration::Env {
            common_kinds: &common_kinds,
            modules: &modules,
            module_context: &module_context,
            type_context: &types,
            class_context: &class_context,
            context: &context,
            implications,
            source: &source,
        },
        decl,
    )
}

#[test]
fn check_definition_1() {
    let common_kinds = Default::default();

    /*
    id : a -> a
    id x = x
    */
    let decl = syntax::Spanned {
        pos: 0,
        item: desugar_decl(
            &Source::Interactive {
                label: String::from("test"),
            },
            syntax::Declaration::Definition {
                name: Spanned {
                    item: Rc::from("id"),
                    pos: 12,
                },
                ty: Spanned {
                    pos: 5,
                    item: Type::mk_arrow(Type::Var(Rc::from("a")), Type::Var(Rc::from("a"))),
                },
                args: vec![syntax::Spanned {
                    pos: 14,
                    item: syntax::Pattern::Name(syntax::Spanned {
                        pos: 14,
                        item: Rc::from("x"),
                    }),
                }],
                body: syntax::Spanned {
                    pos: 18,
                    item: syntax::Expr::Var(String::from("x")),
                },
            },
        )
        .unwrap(),
    };

    let a = core::Type::unsafe_mk_var(0, Kind::Type);
    let expected = Ok(declaration::Checked::Definition {
        name: Rc::from("id"),
        sig: core::TypeSig {
            ty_vars: vec![(Rc::from("a"), a.kind())],
            body: core::Type::arrow(&common_kinds, a.clone(), a),
        },
        body: Rc::new(core::Expr::mk_lam(true, core::Expr::Var(0))),
    });
    let actual = check_declaration(&decl);
    assert_eq!(expected, actual,)
}

#[test]
fn check_definition_2() {
    let common_kinds = CommonKinds::default();

    /*
    thing : { r } -> { x : Int, r }
    thing r = { x = 0, ..r }
    */
    let decl = syntax::Spanned {
        pos: 0,
        item: desugar_decl(
            &Source::Interactive {
                label: String::from("test"),
            },
            syntax::Declaration::Definition {
                name: Spanned {
                    item: Rc::from("thing"),
                    pos: 31,
                },
                ty: Spanned {
                    pos: 8,
                    item: Type::mk_arrow(
                        Type::mk_record(Vec::new(), Some(Type::Var(Rc::from("r")))),
                        Type::mk_record(
                            vec![(Rc::from("x"), Type::Int)],
                            Some(Type::Var(Rc::from("r"))),
                        ),
                    ),
                },
                args: vec![syntax::Spanned {
                    pos: 37,
                    item: syntax::Pattern::Name(syntax::Spanned {
                        pos: 37,
                        item: Rc::from("r"),
                    }),
                }],
                body: syntax::Spanned {
                    pos: 41,
                    item: syntax::Expr::mk_record(
                        vec![(
                            String::from("x"),
                            syntax::Spanned {
                                pos: 47,
                                item: syntax::Expr::Int(0),
                            },
                        )],
                        Some(syntax::Spanned {
                            pos: 52,
                            item: syntax::Expr::Var(String::from("r")),
                        }),
                    ),
                },
            },
        )
        .unwrap(),
    };

    let r = core::Type::unsafe_mk_var(0, Kind::Row);
    let expected = Ok(declaration::Checked::Definition {
        name: Rc::from("thing"),
        sig: core::TypeSig {
            ty_vars: vec![(Rc::from("r"), r.kind())],
            body: core::Type::mk_fatarrow(
                &common_kinds,
                core::Type::mk_hasfield(Rc::from("x"), r.clone()),
                core::Type::arrow(
                    &common_kinds,
                    core::Type::mk_record(&common_kinds, Vec::new(), Some(r.clone())),
                    core::Type::mk_record(
                        &common_kinds,
                        vec![(Rc::from("x"), core::Type::Int)],
                        Some(r),
                    ),
                ),
            ),
        },
        body: Rc::new(core::Expr::mk_lam(
            true,
            core::Expr::mk_lam(
                true,
                core::Expr::mk_extend(core::Expr::Var(1), core::Expr::Int(0), core::Expr::Var(0)),
            ),
        )),
    });
    let actual = check_declaration(&decl);
    assert_eq!(expected, actual)
}

#[test]
fn check_definition_3() {
    let common_kinds = CommonKinds::default();

    /*
    thing : { z : Bool, y : String, x : Int }
    thing = { z = False, y = "", x = 0 }
    */
    let decl = syntax::Spanned {
        pos: 0,
        item: syntax::Declaration::Definition {
            name: Spanned {
                item: Rc::from("thing"),
                pos: 41,
            },
            ty: Spanned {
                pos: 8,
                item: Type::mk_record(
                    vec![
                        (Rc::from("z"), Type::Bool),
                        (Rc::from("y"), Type::String),
                        (Rc::from("x"), Type::Int),
                    ],
                    None,
                ),
            },
            args: Vec::new(),
            body: syntax::Spanned {
                pos: 1,
                item: syntax::Expr::mk_record(
                    vec![
                        (
                            String::from("z"),
                            syntax::Spanned {
                                pos: 3,
                                item: syntax::Expr::False,
                            },
                        ),
                        (
                            String::from("y"),
                            syntax::Spanned {
                                pos: 4,
                                item: syntax::Expr::String(Vec::new()),
                            },
                        ),
                        (
                            String::from("x"),
                            syntax::Spanned {
                                pos: 5,
                                item: syntax::Expr::Int(0),
                            },
                        ),
                    ],
                    None,
                ),
            },
        },
    };
    let expected = Ok(declaration::Checked::Definition {
        name: Rc::from("thing"),
        sig: core::TypeSig {
            ty_vars: Vec::new(),
            body: core::Type::mk_record(
                &common_kinds,
                vec![
                    (Rc::from("z"), core::Type::Bool),
                    (Rc::from("y"), core::Type::String),
                    (Rc::from("x"), core::Type::Int),
                ],
                None,
            ),
        },
        body: Rc::new(core::Expr::mk_record(
            vec![
                (core::Expr::Int(2), core::Expr::False),
                (core::Expr::Int(1), core::Expr::String(Vec::new())),
                (core::Expr::Int(0), core::Expr::Int(0)),
            ],
            None,
        )),
    });
    let actual = check_declaration(&decl);
    assert_eq!(expected, actual)
}

#[test]
fn check_definition_4() {
    let common_kinds = CommonKinds::default();

    /*
    getx : { x : Int, r } -> Int
    getx { x, r } = x
    */
    let decl = syntax::Spanned {
        pos: 0,
        item: desugar_decl(
            &Source::Interactive {
                label: String::from("test"),
            },
            syntax::Declaration::Definition {
                name: Spanned {
                    item: Rc::from("getx"),
                    pos: 29,
                },
                ty: Spanned {
                    pos: 7,
                    item: Type::mk_arrow(
                        Type::mk_record(
                            vec![(Rc::from("x"), Type::Int)],
                            Some(Type::Var(Rc::from("r"))),
                        ),
                        Type::Int,
                    ),
                },
                args: vec![syntax::Spanned {
                    pos: 3,
                    item: syntax::Pattern::Record {
                        names: vec![syntax::Spanned {
                            pos: 1,
                            item: Rc::from("x"),
                        }],
                        rest: Some(syntax::Spanned {
                            pos: 2,
                            item: Rc::from("r"),
                        }),
                    },
                }],
                body: syntax::Spanned {
                    pos: 2,
                    item: syntax::Expr::Var(String::from("x")),
                },
            },
        )
        .unwrap(),
    };
    let expected = Ok(declaration::Checked::Definition {
        name: Rc::from("getx"),
        sig: {
            let r = core::Type::unsafe_mk_var(0, Kind::Row);
            core::TypeSig {
                ty_vars: vec![(Rc::from("r"), r.kind())],
                body: core::Type::mk_fatarrow(
                    &common_kinds,
                    core::Type::mk_hasfield(Rc::from("x"), r.clone()),
                    core::Type::arrow(
                        &common_kinds,
                        core::Type::mk_record(
                            &common_kinds,
                            vec![(Rc::from("x"), core::Type::Int)],
                            Some(r),
                        ),
                        core::Type::Int,
                    ),
                ),
            }
        },
        body: Rc::new(core::Expr::mk_lam(
            true,
            core::Expr::mk_lam(
                true,
                core::Expr::mk_case(
                    core::Expr::Var(0),
                    vec![core::Branch {
                        pattern: core::Pattern::Record {
                            names: vec![core::Expr::Var(1)],
                            rest: true,
                        },
                        body: core::Expr::Var(1),
                    }],
                ),
            ),
        )),
    });
    let actual = check_declaration(&decl);
    assert_eq!(expected, actual)
}

#[test]
fn check_class_1() {
    let common_kinds = CommonKinds::default();
    let mut state = module::State::new();

    let expected = {
        let a = core::Type::unsafe_mk_var(0, Kind::Type);
        Ok(declaration::Checked::Class(core::ClassDeclaration {
            supers: Vec::new(),
            name: Rc::from("MyEq"),
            args: vec![(Rc::from("a"), a.kind())],
            members: vec![ClassMember {
                name: String::from("myeq"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: core::Type::arrow(
                        &common_kinds,
                        a.clone(),
                        core::Type::arrow(&common_kinds, a, core::Type::Bool),
                    ),
                },
            }],
        }))
    };
    /*
    class MyEq a where
      myeq : a -> a -> Bool
    */
    let actual = check_declaration(&Spanned {
        pos: 0,
        item: syntax::Declaration::Class {
            supers: Vec::new(),
            name: Rc::from("MyEq"),
            args: vec![Spanned {
                pos: 9,
                item: Rc::from("a"),
            }],
            members: vec![(
                String::from("myeq"),
                Spanned {
                    pos: 25,
                    item: Type::mk_arrow(
                        Type::Var(Rc::from("a")),
                        Type::mk_arrow(Type::Var(Rc::from("a")), Type::Bool),
                    ),
                },
            )],
        },
    });
    assert_eq!(expected, actual);

    let checked = actual.unwrap();
    state.add_declaration(&common_kinds, checked);

    let expected_class: core::ClassDeclaration = {
        let a = core::Type::unsafe_mk_var(0, Kind::Type);
        core::ClassDeclaration {
            supers: Vec::new(),
            args: vec![(Rc::from("a"), a.kind())],
            name: Rc::from("MyEq"),
            members: vec![core::ClassMember {
                name: String::from("myeq"),
                sig: core::TypeSig {
                    ty_vars: vec![],
                    body: core::Type::arrow(
                        &common_kinds,
                        a.clone(),
                        core::Type::arrow(&common_kinds, a, core::Type::Bool),
                    ),
                },
            }],
        }
    };

    assert_eq!(&expected_class, state.class_context.get("MyEq").unwrap());

    let expected_member = {
        let a = core::Type::unsafe_mk_var(0, Kind::Type);
        let eq_ty = core::Type::unsafe_mk_name(
            Rc::from("MyEq"),
            Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
        );
        core::TypeSig {
            ty_vars: vec![(Rc::from("a"), a.kind())],
            body: core::Type::mk_fatarrow(
                &common_kinds,
                core::Type::app(eq_ty, a.clone()),
                core::Type::arrow(
                    &common_kinds,
                    a.clone(),
                    core::Type::arrow(&common_kinds, a, core::Type::Bool),
                ),
            ),
        }
    };
    assert_eq!(
        Some(&Signature::TypeSig(expected_member)),
        state.context.get(&String::from("myeq"))
    );
}

#[test]
fn check_class_2() {
    let common_kinds = CommonKinds::default();
    let mut state = module::State::new();

    let expected = {
        let a = core::Type::unsafe_mk_var(1, Kind::Type);
        let b = core::Type::unsafe_mk_var(0, Kind::Type);
        Ok(declaration::Checked::Class(core::ClassDeclaration {
            supers: Vec::new(),
            name: Rc::from("Wut"),
            args: vec![(Rc::from("a"), a.kind())],
            members: vec![ClassMember {
                name: String::from("wut"),
                sig: TypeSig {
                    ty_vars: vec![(Rc::from("b"), b.kind())],
                    body: core::Type::arrow(
                        &common_kinds,
                        a,
                        core::Type::arrow(&common_kinds, b, core::Type::Bool),
                    ),
                },
            }],
        }))
    };
    /*
    class Wut a where
      wut : a -> b -> Bool
    */
    let actual = check_declaration(&Spanned {
        pos: 0,
        item: syntax::Declaration::Class {
            supers: Vec::new(),
            name: Rc::from("Wut"),
            args: vec![Spanned {
                pos: 9,
                item: Rc::from("a"),
            }],
            members: vec![(
                String::from("wut"),
                Spanned {
                    pos: 24,
                    item: Type::mk_arrow(
                        Type::Var(Rc::from("a")),
                        Type::mk_arrow(Type::Var(Rc::from("b")), Type::Bool),
                    ),
                },
            )],
        },
    });
    assert_eq!(expected, actual);

    let checked = actual.unwrap();
    state.add_declaration(&common_kinds, checked);

    let expected_class_decl: core::ClassDeclaration = {
        let a = core::Type::unsafe_mk_var(1, Kind::Type);
        let b = core::Type::unsafe_mk_var(0, Kind::Type);
        core::ClassDeclaration {
            supers: Vec::new(),
            args: vec![(Rc::from("a"), a.kind())],
            name: Rc::from("Wut"),
            members: vec![core::ClassMember {
                name: String::from("wut"),
                sig: core::TypeSig {
                    ty_vars: vec![(Rc::from("b"), b.kind())],
                    body: core::Type::arrow(
                        &common_kinds,
                        a,
                        core::Type::arrow(&common_kinds, b, core::Type::Bool),
                    ),
                },
            }],
        }
    };

    assert_eq!(
        &expected_class_decl,
        state.class_context.get("Wut").unwrap()
    );

    let expected_member = {
        let wut_ty = core::Type::unsafe_mk_name(
            Rc::from("Wut"),
            Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
        );
        let a = core::Type::unsafe_mk_var(1, Kind::Type);
        let b = core::Type::unsafe_mk_var(0, Kind::Type);
        core::TypeSig {
            ty_vars: vec![(Rc::from("a"), a.kind()), (Rc::from("b"), b.kind())],
            body: core::Type::mk_fatarrow(
                &common_kinds,
                core::Type::app(wut_ty, a.clone()),
                core::Type::arrow(
                    &common_kinds,
                    a,
                    core::Type::arrow(&common_kinds, b, core::Type::Bool),
                ),
            ),
        }
    };
    assert_eq!(
        Some(&Signature::TypeSig(expected_member)),
        state.context.get(&String::from("wut")),
        "expected member"
    );
}

#[test]
fn check_instance_1() {
    let common_kinds = CommonKinds::default();
    let modules = Default::default();
    let module_context = Default::default();
    let mut state = module::State::new();
    let source = Source::Interactive {
        label: String::from("test"),
    };

    let expected = {
        let eq_ty = core::Type::unsafe_mk_name(
            Rc::from("Eq"),
            Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
        );
        let evidence_name: Rc<str> = Rc::from("Eq ()");
        Ok(declaration::Checked::Instance {
            evidence_name: evidence_name.clone(),
            evidence_body: Rc::new(core::Expr::mk_record(
                vec![(
                    // eq
                    core::Expr::Int(0),
                    // \a b -> True
                    core::Expr::mk_lam(true, core::Expr::mk_lam(true, core::Expr::True)),
                )],
                None,
            )),
            instance_ty_vars: Vec::new(),
            instance_assumes: Vec::new(),
            instance_head: core::Type::app(eq_ty, core::Type::Unit),
            instance_evidence: evidence_name,
        })
    };
    {
        let a = core::Type::unsafe_mk_var(0, Kind::Type);
        state.add_declaration(
            &common_kinds,
            declaration::Checked::Class(core::ClassDeclaration {
                supers: Vec::new(),
                name: Rc::from("Eq"),
                args: vec![(Rc::from("a"), Kind::Type)],
                members: vec![ClassMember {
                    name: String::from("eq"),
                    sig: TypeSig {
                        ty_vars: vec![],
                        body: core::Type::arrow(
                            &common_kinds,
                            a.clone(),
                            core::Type::arrow(&common_kinds, a, core::Type::Bool),
                        ),
                    },
                }],
            }),
        )
    };
    /*
    instance Eq () where
      eq x y = True
    */
    let actual = crate::declaration::check(
        crate::declaration::Env {
            common_kinds: &common_kinds,
            modules: &modules,
            module_context: &module_context,
            type_context: &state.type_context,
            class_context: &state.class_context,
            context: &state.context,
            implications: &state.implications,
            source: &source,
        },
        &Spanned {
            pos: 0,
            item: syntax::Declaration::Instance {
                assumes: Vec::new(),
                name: Spanned {
                    pos: 9,
                    item: Rc::from("Eq"),
                },
                args: vec![Spanned {
                    pos: 11,
                    item: Type::Unit,
                }],
                members: vec![InstanceMember {
                    name: Spanned {
                        pos: 22,
                        item: String::from("eq"),
                    },
                    args: vec![
                        syntax::Spanned {
                            pos: 25,
                            item: syntax::Pattern::Name(Spanned {
                                pos: 25,
                                item: Rc::from("x"),
                            }),
                        },
                        syntax::Spanned {
                            pos: 27,
                            item: syntax::Pattern::Name(Spanned {
                                pos: 27,
                                item: Rc::from("y"),
                            }),
                        },
                    ],
                    body: Spanned {
                        pos: 31,
                        item: syntax::Expr::True,
                    },
                }],
            },
        },
    );
    assert_eq!(expected, actual)
}
