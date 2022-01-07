use super::{InferenceContext, InferenceError, InferredPattern, Solutions};
use crate::{
    evidence::{self, solver},
    kind_inference, BoundVars,
};
use ipso_core::{Branch, CommonKinds, Expr, Pattern, Placeholder, Type};
use ipso_diagnostic::Source;
use ipso_syntax::{self as syntax, kind::Kind, Spanned};
use ipso_util::void::Void;
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};
use syntax::Binop;

const SOURCE_LABEL: &str = "test";

fn with_empty_ctx<A>(f: &dyn Fn(&mut InferenceContext) -> A) -> A {
    let common_kinds = CommonKinds::default();
    let source = Source::Interactive {
        label: String::from(SOURCE_LABEL),
    };
    let mut ctx = InferenceContext::new(
        &common_kinds,
        &source,
        &HashMap::new(),
        &HashMap::new(),
        &BoundVars::new(),
        &mut kind_inference::Solutions::new(),
        &mut Solutions::new(),
        &HashMap::new(),
        &mut BoundVars::new(),
        &mut evidence::Evidence::new(),
    );
    f(&mut ctx)
}

#[test]
fn infer_pattern_1() {
    with_empty_ctx(&|ctx| {
        let pattern = syntax::Pattern::Name(Spanned {
            pos: 0,
            item: String::from("x"),
        });
        assert_eq!(
            ctx.infer_pattern(&pattern),
            InferredPattern {
                pattern: Pattern::Name,
                names: vec![(Rc::from("x"), Type::Meta(Kind::Type, 0))],
                ty: Type::Meta(Kind::Type, 0),
            }
        )
    })
}

#[test]
fn infer_pattern_2() {
    with_empty_ctx(&|ctx| {
        let pat = syntax::Pattern::Record {
            names: vec![
                syntax::Spanned {
                    pos: 0,
                    item: String::from("x"),
                },
                syntax::Spanned {
                    pos: 2,
                    item: String::from("y"),
                },
                syntax::Spanned {
                    pos: 4,
                    item: String::from("z"),
                },
            ],
            rest: None,
        };
        assert_eq!(
            ctx.infer_pattern(&pat),
            InferredPattern {
                pattern: Pattern::Record {
                    names: vec![
                        Expr::mk_placeholder(2),
                        Expr::mk_placeholder(1),
                        Expr::mk_placeholder(0)
                    ],
                    rest: false
                },
                ty: Type::mk_record(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Meta(Kind::Type, 0)),
                        (Rc::from("y"), Type::Meta(Kind::Type, 1)),
                        (Rc::from("z"), Type::Meta(Kind::Type, 2))
                    ],
                    None
                ),
                names: vec![
                    (Rc::from("x"), Type::Meta(Kind::Type, 0)),
                    (Rc::from("y"), Type::Meta(Kind::Type, 1)),
                    (Rc::from("z"), Type::Meta(Kind::Type, 2)),
                ]
            }
        )
    })
}

#[test]
fn infer_pattern_3() {
    with_empty_ctx(&|ctx| {
        let pat = syntax::Pattern::Record {
            names: vec![
                syntax::Spanned {
                    pos: 0,
                    item: String::from("x"),
                },
                syntax::Spanned {
                    pos: 2,
                    item: String::from("y"),
                },
                syntax::Spanned {
                    pos: 4,
                    item: String::from("z"),
                },
            ],
            rest: Some(syntax::Spanned {
                pos: 6,
                item: String::from("w"),
            }),
        };
        assert_eq!(
            ctx.infer_pattern(&pat),
            InferredPattern {
                pattern: Pattern::Record {
                    names: vec![
                        Expr::mk_placeholder(2),
                        Expr::mk_placeholder(1),
                        Expr::mk_placeholder(0)
                    ],
                    rest: true
                },
                ty: Type::mk_record(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Meta(Kind::Type, 0)),
                        (Rc::from("y"), Type::Meta(Kind::Type, 1)),
                        (Rc::from("z"), Type::Meta(Kind::Type, 2))
                    ],
                    Some(Type::Meta(Kind::Row, 3))
                ),
                names: vec![
                    (Rc::from("x"), Type::Meta(Kind::Type, 0)),
                    (Rc::from("y"), Type::Meta(Kind::Type, 1)),
                    (Rc::from("z"), Type::Meta(Kind::Type, 2)),
                    (
                        Rc::from("w"),
                        Type::mk_record(
                            ctx.common_kinds,
                            Vec::new(),
                            Some(Type::Meta(Kind::Row, 3))
                        )
                    ),
                ]
            }
        )
    })
}

#[test]
fn infer_pattern_4() {
    with_empty_ctx(&|ctx| {
        let pat = syntax::Pattern::Variant {
            name: String::from("just"),
            arg: syntax::Spanned {
                pos: 5,
                item: String::from("x"),
            },
        };
        assert_eq!(
            ctx.infer_pattern(&pat),
            InferredPattern {
                pattern: Pattern::mk_variant(Expr::mk_placeholder(0)),
                ty: Type::mk_variant(
                    ctx.common_kinds,
                    vec![(Rc::from("just"), Type::Meta(Kind::Type, 0))],
                    Some(Type::Meta(Kind::Row, 1))
                ),
                names: vec![(Rc::from("x"), Type::Meta(Kind::Type, 0))]
            }
        )
    })
}

#[test]
fn infer_lam_1() {
    with_empty_ctx(&|ctx| {
        // \x -> x
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Pattern::Name(syntax::Spanned {
                    pos: 1,
                    item: String::from("x"),
                })],
                syntax::Spanned {
                    pos: 6,
                    item: syntax::Expr::Var(String::from("x")),
                },
            ),
        };
        let expected = Ok((
            Expr::mk_lam(true, Expr::Var(0)),
            Type::arrow(
                ctx.common_kinds,
                Type::Meta(Kind::Type, 4),
                Type::Meta(Kind::Type, 4),
            ),
        ));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_lam_2() {
    with_empty_ctx(&|ctx| {
        // \{x, y} -> x
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Pattern::Record {
                    names: vec![
                        syntax::Spanned {
                            pos: 2,
                            item: String::from("x"),
                        },
                        syntax::Spanned {
                            pos: 5,
                            item: String::from("y"),
                        },
                    ],
                    rest: None,
                }],
                syntax::Spanned {
                    pos: 11,
                    item: syntax::Expr::Var(String::from("x")),
                },
            ),
        };
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        let expected = Ok((
            Expr::mk_lam(
                true,
                Expr::mk_case(
                    Expr::Var(0),
                    vec![Branch {
                        pattern: Pattern::Record {
                            names: vec![Expr::mk_placeholder(1), Expr::mk_placeholder(0)],
                            rest: false,
                        },
                        body: Expr::Var(1),
                    }],
                ),
            ),
            Type::arrow(
                ctx.common_kinds,
                Type::mk_record(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Meta(Kind::Type, 4)),
                        (Rc::from("y"), Type::Meta(Kind::Type, 5)),
                    ],
                    None,
                ),
                Type::Meta(Kind::Type, 4),
            ),
        ));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_lam_3() {
    with_empty_ctx(&|ctx| {
        // \{x, y} -> y
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Pattern::Record {
                    names: vec![
                        syntax::Spanned {
                            pos: 2,
                            item: String::from("x"),
                        },
                        syntax::Spanned {
                            pos: 5,
                            item: String::from("y"),
                        },
                    ],
                    rest: None,
                }],
                syntax::Spanned {
                    pos: 11,
                    item: syntax::Expr::Var(String::from("y")),
                },
            ),
        };
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        let expected = Ok((
            Expr::mk_lam(
                true,
                Expr::mk_case(
                    Expr::Var(0),
                    vec![Branch {
                        pattern: Pattern::Record {
                            names: vec![Expr::mk_placeholder(1), Expr::mk_placeholder(0)],
                            rest: false,
                        },
                        body: Expr::Var(0),
                    }],
                ),
            ),
            Type::arrow(
                ctx.common_kinds,
                Type::mk_record(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Meta(Kind::Type, 4)),
                        (Rc::from("y"), Type::Meta(Kind::Type, 5)),
                    ],
                    None,
                ),
                Type::Meta(Kind::Type, 5),
            ),
        ));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_lam_4() {
    with_empty_ctx(&|ctx| {
        // \{x, y, ...z} -> z
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Pattern::Record {
                    names: vec![
                        syntax::Spanned {
                            pos: 2,
                            item: String::from("x"),
                        },
                        syntax::Spanned {
                            pos: 5,
                            item: String::from("y"),
                        },
                    ],
                    rest: Some(syntax::Spanned {
                        pos: 11,
                        item: String::from("z"),
                    }),
                }],
                syntax::Spanned {
                    pos: 17,
                    item: syntax::Expr::Var(String::from("z")),
                },
            ),
        };
        let expected = Ok((
            Expr::mk_lam(
                true,
                Expr::mk_case(
                    Expr::Var(0),
                    vec![Branch {
                        pattern: Pattern::Record {
                            names: vec![Expr::mk_placeholder(1), Expr::mk_placeholder(0)],
                            rest: true,
                        },
                        body: Expr::Var(0),
                    }],
                ),
            ),
            Type::arrow(
                ctx.common_kinds,
                Type::mk_record(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Meta(Kind::Type, 4)),
                        (Rc::from("y"), Type::Meta(Kind::Type, 5)),
                    ],
                    Some(Type::Meta(Kind::Row, 6)),
                ),
                Type::mk_record(ctx.common_kinds, vec![], Some(Type::Meta(Kind::Row, 6))),
            ),
        ));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_lam_5() {
    with_empty_ctx(&|ctx| {
        // \f x -> f x
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![
                    syntax::Pattern::Name(syntax::Spanned {
                        pos: 1,
                        item: String::from("f"),
                    }),
                    syntax::Pattern::Name(syntax::Spanned {
                        pos: 1,
                        item: String::from("x"),
                    }),
                ],
                syntax::Expr::mk_app(
                    syntax::Spanned {
                        pos: 8,
                        item: syntax::Expr::Var(String::from("f")),
                    },
                    syntax::Spanned {
                        pos: 10,
                        item: syntax::Expr::Var(String::from("x")),
                    },
                ),
            ),
        };

        let expected = Ok((
            Expr::mk_lam(
                true,
                Expr::mk_lam(true, Expr::mk_app(Expr::Var(1), Expr::Var(0))),
            ),
            Type::arrow(
                ctx.common_kinds,
                Type::arrow(
                    ctx.common_kinds,
                    Type::Meta(Kind::Type, 6),
                    Type::Meta(Kind::Type, 8),
                ),
                Type::arrow(
                    ctx.common_kinds,
                    Type::Meta(Kind::Type, 6),
                    Type::Meta(Kind::Type, 8),
                ),
            ),
        ));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_array_1() {
    with_empty_ctx(&|ctx| {
        // [1, 2, 3]
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::Array(vec![
                syntax::Spanned {
                    pos: 1,
                    item: syntax::Expr::Int(1),
                },
                syntax::Spanned {
                    pos: 4,
                    item: syntax::Expr::Int(2),
                },
                syntax::Spanned {
                    pos: 7,
                    item: syntax::Expr::Int(3),
                },
            ]),
        };
        assert_eq!(
            ctx.infer(&term),
            Ok((
                Expr::Array(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)]),
                Type::app(Type::mk_array(ctx.common_kinds), Type::Int)
            ))
        )
    })
}

#[test]
fn infer_array_2() {
    with_empty_ctx(&|ctx| {
        // [1, true, 3]
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::Array(vec![
                syntax::Spanned {
                    pos: 1,
                    item: syntax::Expr::Int(1),
                },
                syntax::Spanned {
                    pos: 4,
                    item: syntax::Expr::True,
                },
                syntax::Spanned {
                    pos: 10,
                    item: syntax::Expr::Int(3),
                },
            ]),
        };
        assert_eq!(
            ctx.infer(&term),
            Err(InferenceError::mismatch(
                &Source::Interactive {
                    label: String::from(SOURCE_LABEL)
                },
                &syntax::Type::Int,
                &syntax::Type::Bool
            )
            .with_position(4))
        )
    })
}

#[test]
fn unify_rows_1() {
    with_empty_ctx(&|ctx| {
        assert_eq!(
            ctx.unify(
                &Type::mk_record(
                    ctx.common_kinds,
                    vec![(Rc::from("x"), Type::Int), (Rc::from("y"), Type::Bool)],
                    None
                ),
                &Type::mk_record(
                    ctx.common_kinds,
                    vec![(Rc::from("y"), Type::Bool), (Rc::from("x"), Type::Int)],
                    None
                )
            ),
            Ok(())
        )
    })
}

#[test]
fn unify_rows_2() {
    with_empty_ctx(&|ctx| {
        assert_eq!(
            ctx.unify(
                &Type::mk_record(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Int),
                        (Rc::from("x"), Type::Bool),
                        (Rc::from("y"), Type::Bool)
                    ],
                    None
                ),
                &Type::mk_record(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("y"), Type::Bool),
                        (Rc::from("x"), Type::Int),
                        (Rc::from("x"), Type::Bool)
                    ],
                    None
                )
            ),
            Ok(())
        )
    })
}

#[test]
fn unify_rows_3() {
    with_empty_ctx(&|ctx| {
        assert_eq!(
            ctx.unify(
                &Type::mk_record(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Int),
                        (Rc::from("x"), Type::Bool),
                        (Rc::from("y"), Type::Bool)
                    ],
                    None
                ),
                &Type::mk_record(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Int),
                        (Rc::from("y"), Type::Bool),
                        (Rc::from("x"), Type::Bool)
                    ],
                    None
                )
            ),
            Ok(())
        )
    })
}

#[test]
fn unify_rows_4() {
    with_empty_ctx(&|ctx| {
        assert_eq!(
            ctx.unify(
                &Type::mk_record(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Int),
                        (Rc::from("x"), Type::Bool),
                        (Rc::from("y"), Type::Bool)
                    ],
                    None
                ),
                &Type::mk_record(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Int),
                        (Rc::from("y"), Type::Bool),
                        (Rc::from("x"), Type::Int)
                    ],
                    None
                )
            ),
            Err(InferenceError::mismatch(
                &Source::Interactive {
                    label: String::from(SOURCE_LABEL)
                },
                &syntax::Type::Bool,
                &syntax::Type::Int
            )
            .with_position(0))
        )
    })
}

#[test]
fn infer_record_1() {
    with_empty_ctx(&|ctx| {
        // {}
        let term = syntax::Expr::mk_record(Vec::new(), None);
        assert_eq!(
            ctx.infer(&syntax::Spanned { pos: 0, item: term })
                .map(|(expr, ty)| (expr, ctx.zonk_type(ty))),
            Ok((
                Expr::mk_record(Vec::new(), None),
                Type::mk_record(ctx.common_kinds, Vec::new(), None)
            ))
        )
    })
}

#[test]
fn infer_record_2() {
    with_empty_ctx(&|ctx| {
        // { x = 1, y = true }
        let term = syntax::Expr::mk_record(
            vec![
                (
                    String::from("x"),
                    syntax::Spanned {
                        pos: 2,
                        item: syntax::Expr::Int(1),
                    },
                ),
                (
                    String::from("y"),
                    syntax::Spanned {
                        pos: 13,
                        item: syntax::Expr::True,
                    },
                ),
            ],
            None,
        );
        assert_eq!(
            ctx.infer(&syntax::Spanned { pos: 0, item: term })
                .map(|(expr, ty)| (expr, ctx.zonk_type(ty))),
            Ok((
                Expr::mk_record(
                    vec![
                        (Expr::mk_placeholder(1), Expr::Int(1)),
                        (Expr::mk_placeholder(0), Expr::True)
                    ],
                    None
                ),
                Type::mk_record(
                    ctx.common_kinds,
                    vec![(Rc::from("x"), Type::Int), (Rc::from("y"), Type::Bool)],
                    None
                )
            ))
        )
    })
}

#[test]
fn infer_record_3() {
    with_empty_ctx(&|ctx| {
        // { x = 1, y = true, ...{ z = 'c' } }
        let term = syntax::Expr::mk_record(
            vec![
                (
                    String::from("x"),
                    syntax::Spanned {
                        pos: 2,
                        item: syntax::Expr::Int(1),
                    },
                ),
                (
                    String::from("y"),
                    syntax::Spanned {
                        pos: 13,
                        item: syntax::Expr::True,
                    },
                ),
            ],
            Some(syntax::Spanned {
                pos: 22,
                item: syntax::Expr::mk_record(
                    vec![(
                        String::from("z"),
                        syntax::Spanned {
                            pos: 24,
                            item: syntax::Expr::Char('c'),
                        },
                    )],
                    None,
                ),
            }),
        );
        assert_eq!(
            ctx.infer(&syntax::Spanned { pos: 0, item: term })
                .map(|(expr, ty)| (expr, ctx.zonk_type(ty))),
            Ok((
                Expr::mk_record(
                    vec![
                        (Expr::mk_placeholder(1), Expr::Int(1)),
                        (Expr::mk_placeholder(0), Expr::True)
                    ],
                    Some(Expr::mk_record(
                        vec![(Expr::mk_placeholder(2), Expr::Char('c'))],
                        None
                    ))
                ),
                Type::mk_record(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Int),
                        (Rc::from("y"), Type::Bool),
                        (Rc::from("z"), Type::Char)
                    ],
                    None
                )
            ))
        )
    })
}

#[test]
fn infer_record_4() {
    with_empty_ctx(&|ctx| {
        // { x = 1, y = true, ...1 }
        let term = syntax::Expr::mk_record(
            vec![
                (
                    String::from("x"),
                    syntax::Spanned {
                        pos: 2,
                        item: syntax::Expr::Int(1),
                    },
                ),
                (
                    String::from("y"),
                    syntax::Spanned {
                        pos: 13,
                        item: syntax::Expr::True,
                    },
                ),
            ],
            Some(syntax::Spanned {
                pos: 22,
                item: syntax::Expr::Int(1),
            }),
        );
        assert_eq!(
            ctx.infer(&syntax::Spanned { pos: 0, item: term })
                .map(|(expr, ty)| (expr, ctx.zonk_type(ty))),
            Err(InferenceError::mismatch(
                &Source::Interactive {
                    label: String::from(SOURCE_LABEL),
                },
                &syntax::Type::mk_record(Vec::new(), Some(syntax::Type::Meta(0))),
                &syntax::Type::Int
            )
            .with_position(22))
        )
    })
}

#[test]
fn infer_record_5() {
    with_empty_ctx(&|ctx| {
        let expected_expr = Expr::mk_record(
            vec![
                (Expr::Placeholder(Placeholder(2)), Expr::False),
                (Expr::Placeholder(Placeholder(1)), Expr::String(Vec::new())),
                (Expr::Placeholder(Placeholder(0)), Expr::Int(0)),
            ],
            None,
        );
        let expected_ty = Type::mk_record(
            ctx.common_kinds,
            vec![
                (Rc::from("z"), Type::Bool),
                (Rc::from("y"), Type::String),
                (Rc::from("x"), Type::Int),
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
        let actual_result = ctx.infer(&expr).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected_result, actual_result, "checking results");

        let (mut actual_expr, _actual_ty) = actual_result.unwrap();

        let mut placeholders: HashSet<Placeholder> = HashSet::new();
        let _: Result<(), Void> = actual_expr.subst_placeholder(&mut |p| {
            placeholders.insert(*p);
            Ok(Expr::Placeholder(*p))
        });

        assert_eq!(3, placeholders.len(), "checking number of Placeholders");

        let p0 = placeholders.get(&Placeholder(0)).unwrap();
        let p1 = placeholders.get(&Placeholder(1)).unwrap();
        let p2 = placeholders.get(&Placeholder(2)).unwrap();

        assert_eq!(
            Ok((
                Expr::Int(0),
                evidence::Constraint::HasField {
                    field: Rc::from("x"),
                    rest: Type::RowNil
                }
            )),
            solver::solve_placeholder(&mut ctx, *p0)
                .map(|(expr, constraint)| (expr, ctx.zonk_constraint(&constraint)))
        );

        assert_eq!(
            Ok((
                Expr::mk_binop(Binop::Add, Expr::Int(1), Expr::Int(0)),
                evidence::Constraint::HasField {
                    field: Rc::from("y"),
                    rest: Type::mk_rows(vec![(Rc::from("x"), Type::Int)], None)
                }
            )),
            solver::solve_placeholder(&mut ctx, *p1)
                .map(|(expr, constraint)| (expr, ctx.zonk_constraint(&constraint)))
        );

        assert_eq!(
            Ok((
                Expr::mk_binop(
                    Binop::Add,
                    Expr::Int(1),
                    Expr::mk_binop(Binop::Add, Expr::Int(1), Expr::Int(0))
                ),
                evidence::Constraint::HasField {
                    field: Rc::from("z"),
                    rest: Type::mk_rows(
                        vec![(Rc::from("y"), Type::String), (Rc::from("x"), Type::Int)],
                        None
                    )
                }
            )),
            solver::solve_placeholder(&mut ctx, *p2)
                .map(|(expr, constraint)| (expr, ctx.zonk_constraint(&constraint)))
        );
    })
}

#[test]
fn infer_case_1() {
    with_empty_ctx(&|ctx| {
        /*
        \x -> case x of
          X a -> a
        */
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Pattern::Name(syntax::Spanned {
                    pos: 1,
                    item: String::from("x"),
                })],
                syntax::Spanned {
                    pos: 6,
                    item: syntax::Expr::mk_case(
                        syntax::Spanned {
                            pos: 11,
                            item: syntax::Expr::Var(String::from("x")),
                        },
                        vec![syntax::Branch {
                            pattern: syntax::Spanned {
                                pos: 18,
                                item: syntax::Pattern::Variant {
                                    name: String::from("X"),
                                    arg: syntax::Spanned {
                                        pos: 20,
                                        item: String::from("a"),
                                    },
                                },
                            },
                            body: syntax::Spanned {
                                pos: 25,
                                item: syntax::Expr::Var(String::from("a")),
                            },
                        }],
                    ),
                },
            ),
        };
        let expected = Ok((
            Expr::mk_lam(
                true,
                Expr::mk_case(
                    Expr::Var(0),
                    vec![Branch {
                        pattern: Pattern::mk_variant(Expr::mk_placeholder(0)),
                        body: Expr::Var(0),
                    }],
                ),
            ),
            Type::mk_arrow(
                ctx.common_kinds,
                &Type::mk_variant(
                    ctx.common_kinds,
                    vec![(Rc::from("X"), Type::Meta(Kind::Type, 6))],
                    None,
                ),
                &Type::Meta(Kind::Type, 6),
            ),
        ));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_case_2() {
    with_empty_ctx(&|ctx| {
        /*
        \x -> case x of
          Left a -> a
          Right b -> b
        */
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Pattern::Name(syntax::Spanned {
                    pos: 1,
                    item: String::from("x"),
                })],
                syntax::Spanned {
                    pos: 6,
                    item: syntax::Expr::mk_case(
                        syntax::Spanned {
                            pos: 11,
                            item: syntax::Expr::Var(String::from("x")),
                        },
                        vec![
                            syntax::Branch {
                                pattern: syntax::Spanned {
                                    pos: 18,
                                    item: syntax::Pattern::Variant {
                                        name: String::from("Left"),
                                        arg: syntax::Spanned {
                                            pos: 23,
                                            item: String::from("a"),
                                        },
                                    },
                                },
                                body: syntax::Spanned {
                                    pos: 28,
                                    item: syntax::Expr::Var(String::from("a")),
                                },
                            },
                            syntax::Branch {
                                pattern: syntax::Spanned {
                                    pos: 32,
                                    item: syntax::Pattern::Variant {
                                        name: String::from("Right"),
                                        arg: syntax::Spanned {
                                            pos: 34,
                                            item: String::from("b"),
                                        },
                                    },
                                },
                                body: syntax::Spanned {
                                    pos: 39,
                                    item: syntax::Expr::Var(String::from("b")),
                                },
                            },
                        ],
                    ),
                },
            ),
        };
        let expected = Ok((
            Expr::mk_lam(
                true,
                Expr::mk_case(
                    Expr::Var(0),
                    vec![
                        Branch {
                            pattern: Pattern::mk_variant(Expr::mk_placeholder(0)),
                            body: Expr::Var(0),
                        },
                        Branch {
                            pattern: Pattern::mk_variant(Expr::mk_placeholder(1)),
                            body: Expr::Var(0),
                        },
                    ],
                ),
            ),
            Type::mk_arrow(
                ctx.common_kinds,
                &Type::mk_variant(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("Left"), Type::Meta(Kind::Type, 8)),
                        (Rc::from("Right"), Type::Meta(Kind::Type, 8)),
                    ],
                    None,
                ),
                &Type::Meta(Kind::Type, 8),
            ),
        ));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_case_3() {
    with_empty_ctx(&|ctx| {
        /*
        \x -> case x of
          Left a -> a
          Right b -> b
          _ -> 1
        */
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Pattern::Name(syntax::Spanned {
                    pos: 1,
                    item: String::from("x"),
                })],
                syntax::Spanned {
                    pos: 6,
                    item: syntax::Expr::mk_case(
                        syntax::Spanned {
                            pos: 11,
                            item: syntax::Expr::Var(String::from("x")),
                        },
                        vec![
                            syntax::Branch {
                                pattern: syntax::Spanned {
                                    pos: 18,
                                    item: syntax::Pattern::Variant {
                                        name: String::from("Left"),
                                        arg: syntax::Spanned {
                                            pos: 23,
                                            item: String::from("a"),
                                        },
                                    },
                                },
                                body: syntax::Spanned {
                                    pos: 28,
                                    item: syntax::Expr::Var(String::from("a")),
                                },
                            },
                            syntax::Branch {
                                pattern: syntax::Spanned {
                                    pos: 32,
                                    item: syntax::Pattern::Variant {
                                        name: String::from("Right"),
                                        arg: syntax::Spanned {
                                            pos: 34,
                                            item: String::from("b"),
                                        },
                                    },
                                },
                                body: syntax::Spanned {
                                    pos: 39,
                                    item: syntax::Expr::Var(String::from("b")),
                                },
                            },
                            syntax::Branch {
                                pattern: syntax::Spanned {
                                    pos: 43,
                                    item: syntax::Pattern::Wildcard,
                                },
                                body: syntax::Spanned {
                                    pos: 48,
                                    item: syntax::Expr::Int(1),
                                },
                            },
                        ],
                    ),
                },
            ),
        };
        let expected = Ok((
            Expr::mk_lam(
                true,
                Expr::mk_case(
                    Expr::Var(0),
                    vec![
                        Branch {
                            pattern: Pattern::mk_variant(Expr::mk_placeholder(0)),
                            body: Expr::Var(0),
                        },
                        Branch {
                            pattern: Pattern::mk_variant(Expr::mk_placeholder(1)),
                            body: Expr::Var(0),
                        },
                        Branch {
                            pattern: Pattern::Wildcard,
                            body: Expr::Int(1),
                        },
                    ],
                ),
            ),
            Type::mk_arrow(
                ctx.common_kinds,
                &Type::mk_variant(
                    ctx.common_kinds,
                    vec![
                        (Rc::from("Left"), Type::Int),
                        (Rc::from("Right"), Type::Int),
                    ],
                    Some(Type::Meta(Kind::Row, 9)),
                ),
                &Type::Int,
            ),
        ));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_case_4() {
    with_empty_ctx(&|ctx| {
        /*
        \x -> case x of
          Left a -> a
          Left b -> b
          _ -> 1
        */
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Pattern::Name(syntax::Spanned {
                    pos: 1,
                    item: String::from("x"),
                })],
                syntax::Spanned {
                    pos: 6,
                    item: syntax::Expr::mk_case(
                        syntax::Spanned {
                            pos: 11,
                            item: syntax::Expr::Var(String::from("x")),
                        },
                        vec![
                            syntax::Branch {
                                pattern: syntax::Spanned {
                                    pos: 18,
                                    item: syntax::Pattern::Variant {
                                        name: String::from("Left"),
                                        arg: syntax::Spanned {
                                            pos: 23,
                                            item: String::from("a"),
                                        },
                                    },
                                },
                                body: syntax::Spanned {
                                    pos: 28,
                                    item: syntax::Expr::Var(String::from("a")),
                                },
                            },
                            syntax::Branch {
                                pattern: syntax::Spanned {
                                    pos: 32,
                                    item: syntax::Pattern::Variant {
                                        name: String::from("Left"),
                                        arg: syntax::Spanned {
                                            pos: 34,
                                            item: String::from("b"),
                                        },
                                    },
                                },
                                body: syntax::Spanned {
                                    pos: 38,
                                    item: syntax::Expr::Var(String::from("b")),
                                },
                            },
                            syntax::Branch {
                                pattern: syntax::Spanned {
                                    pos: 42,
                                    item: syntax::Pattern::Wildcard,
                                },
                                body: syntax::Spanned {
                                    pos: 47,
                                    item: syntax::Expr::Int(1),
                                },
                            },
                        ],
                    ),
                },
            ),
        };
        assert_eq!(
            ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty))),
            Err(InferenceError::redundant_pattern(&Source::Interactive {
                label: String::from("(typechecker)"),
            })
            .with_position(32))
        )
    })
}
