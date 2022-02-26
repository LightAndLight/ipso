use super::{InferenceContext, InferenceError, InferredPattern, Solutions};
use crate::{evidence, kind_inference, BoundVars};
use ipso_core::{Branch, CommonKinds, Expr, Pattern, Type};
use ipso_diagnostic::Source;
use ipso_syntax::{self as syntax, kind::Kind, Spanned};
use std::{collections::HashMap, rc::Rc};

const SOURCE_LABEL: &str = "test";

fn with_type_variables_ctx<A, F: FnOnce(&mut InferenceContext) -> A>(
    type_variables: BoundVars<Kind>,
    f: F,
) -> A {
    let common_kinds = CommonKinds::default();
    let source = Source::Interactive {
        label: String::from(SOURCE_LABEL),
    };
    let modules = HashMap::new();
    let types = HashMap::new();
    let mut kind_solutions = kind_inference::Solutions::new();
    let mut type_solutions = Solutions::new();
    let type_signatures = HashMap::new();
    let mut variables = BoundVars::new();
    let mut evidence = evidence::Evidence::new();
    let mut ctx = InferenceContext::new(
        &common_kinds,
        &source,
        &modules,
        &types,
        &type_variables,
        &mut kind_solutions,
        &mut type_solutions,
        &type_signatures,
        &mut variables,
        &mut evidence,
    );
    f(&mut ctx)
}

fn with_empty_ctx<A, F: FnOnce(&mut InferenceContext) -> A>(f: F) -> A {
    with_type_variables_ctx(BoundVars::new(), f)
}

#[test]
fn occurs_1() {
    with_empty_ctx(|ctx| {
        let v1 = ctx.fresh_type_meta(&Kind::Type);
        let v2 = ctx.fresh_type_meta(&Kind::Type);
        let expected = Err(InferenceError::occurs(
            &Source::Interactive {
                label: String::from(SOURCE_LABEL),
            },
            0,
            syntax::Type::mk_arrow(
                v1.to_syntax()
                    .map(&mut |ix| ctx.type_variables.lookup_index(*ix).unwrap().0.clone()),
                v2.to_syntax()
                    .map(&mut |ix| ctx.type_variables.lookup_index(*ix).unwrap().0.clone()),
            ),
        ));
        let actual = ctx.unify(None, &v1, &Type::mk_arrow(ctx.common_kinds, &v1, &v2));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_pattern_1() {
    with_empty_ctx(|ctx| {
        let pattern = syntax::Spanned {
            pos: 0,
            item: syntax::Pattern::Name(Spanned {
                pos: 0,
                item: String::from("x"),
            }),
        };
        assert_eq!(
            ctx.infer_pattern(&pattern),
            InferredPattern::Any {
                pattern: Pattern::Name,
                names: vec![(Rc::from("x"), Type::Meta(Kind::Type, 0))],
                ty: Type::Meta(Kind::Type, 0),
            }
        )
    })
}

#[test]
fn infer_pattern_2() {
    with_empty_ctx(|ctx| {
        let pat = syntax::Spanned {
            pos: 0,
            item: syntax::Pattern::Record {
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
            },
        };
        let expected = InferredPattern::Any {
            pattern: Pattern::Record {
                names: vec![
                    Expr::mk_placeholder(0),
                    Expr::mk_placeholder(1),
                    Expr::mk_placeholder(2),
                ],
                rest: false,
            },
            ty: Type::mk_record(
                ctx.common_kinds,
                vec![
                    (Rc::from("x"), Type::Meta(Kind::Type, 0)),
                    (Rc::from("y"), Type::Meta(Kind::Type, 1)),
                    (Rc::from("z"), Type::Meta(Kind::Type, 2)),
                ],
                None,
            ),
            names: vec![
                (Rc::from("x"), Type::Meta(Kind::Type, 0)),
                (Rc::from("y"), Type::Meta(Kind::Type, 1)),
                (Rc::from("z"), Type::Meta(Kind::Type, 2)),
            ],
        };
        let actual = ctx.infer_pattern(&pat);
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_pattern_3() {
    with_empty_ctx(|ctx| {
        let pat = syntax::Spanned {
            pos: 0,
            item: syntax::Pattern::Record {
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
            },
        };
        let expected = InferredPattern::Any {
            pattern: Pattern::Record {
                names: vec![
                    Expr::mk_placeholder(0),
                    Expr::mk_placeholder(1),
                    Expr::mk_placeholder(2),
                ],
                rest: true,
            },
            ty: Type::mk_record(
                ctx.common_kinds,
                vec![
                    (Rc::from("x"), Type::Meta(Kind::Type, 0)),
                    (Rc::from("y"), Type::Meta(Kind::Type, 1)),
                    (Rc::from("z"), Type::Meta(Kind::Type, 2)),
                ],
                Some(Type::Meta(Kind::Row, 3)),
            ),
            names: vec![
                (Rc::from("x"), Type::Meta(Kind::Type, 0)),
                (Rc::from("y"), Type::Meta(Kind::Type, 1)),
                (Rc::from("z"), Type::Meta(Kind::Type, 2)),
                (
                    Rc::from("w"),
                    Type::mk_record(ctx.common_kinds, Vec::new(), Some(Type::Meta(Kind::Row, 3))),
                ),
            ],
        };
        let actual = ctx.infer_pattern(&pat);
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_pattern_4() {
    with_empty_ctx(|ctx| {
        let pat = syntax::Spanned {
            pos: 0,
            item: syntax::Pattern::Variant {
                name: String::from("just"),
                arg: syntax::Spanned {
                    pos: 5,
                    item: String::from("x"),
                },
            },
        };
        let expected = InferredPattern::Variant {
            tag: Rc::new(Expr::mk_placeholder(0)),
            ctor: Rc::from("just"),
            arg_name: Rc::from("x"),
            arg_ty: Type::Meta(Kind::Type, 0),
            rest: Type::Meta(Kind::Row, 1),
        };
        let actual = ctx.infer_pattern(&pat);
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_lam_1() {
    with_empty_ctx(|ctx| {
        // \x -> x
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Spanned {
                    pos: 1,
                    item: syntax::Pattern::Name(syntax::Spanned {
                        pos: 1,
                        item: String::from("x"),
                    }),
                }],
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
                Type::Meta(Kind::Type, 0),
                Type::Meta(Kind::Type, 0),
            ),
        ));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_lam_2() {
    with_empty_ctx(|ctx| {
        // \{x, y} -> x
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Spanned {
                    pos: 1,
                    item: syntax::Pattern::Record {
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
                    },
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
                            names: vec![Expr::mk_placeholder(0), Expr::mk_placeholder(1)],
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
                        (Rc::from("x"), Type::Meta(Kind::Type, 0)),
                        (Rc::from("y"), Type::Meta(Kind::Type, 1)),
                    ],
                    None,
                ),
                Type::Meta(Kind::Type, 0),
            ),
        ));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_lam_3() {
    with_empty_ctx(|ctx| {
        // \{x, y} -> y
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Spanned {
                    pos: 1,
                    item: syntax::Pattern::Record {
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
                    },
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
                            names: vec![Expr::mk_placeholder(0), Expr::mk_placeholder(1)],
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
                        (Rc::from("x"), Type::Meta(Kind::Type, 0)),
                        (Rc::from("y"), Type::Meta(Kind::Type, 1)),
                    ],
                    None,
                ),
                Type::Meta(Kind::Type, 1),
            ),
        ));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_lam_4() {
    with_empty_ctx(|ctx| {
        // \{x, y, ...z} -> z
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Spanned {
                    pos: 1,
                    item: syntax::Pattern::Record {
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
                    },
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
                            names: vec![Expr::mk_placeholder(0), Expr::mk_placeholder(1)],
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
                        (Rc::from("x"), Type::Meta(Kind::Type, 0)),
                        (Rc::from("y"), Type::Meta(Kind::Type, 1)),
                    ],
                    Some(Type::Meta(Kind::Row, 2)),
                ),
                Type::mk_record(ctx.common_kinds, vec![], Some(Type::Meta(Kind::Row, 2))),
            ),
        ));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_lam_5() {
    with_empty_ctx(|ctx| {
        // \f x -> f x
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![
                    syntax::Spanned {
                        pos: 1,
                        item: syntax::Pattern::Name(syntax::Spanned {
                            pos: 1,
                            item: String::from("f"),
                        }),
                    },
                    syntax::Spanned {
                        pos: 3,
                        item: syntax::Pattern::Name(syntax::Spanned {
                            pos: 3,
                            item: String::from("x"),
                        }),
                    },
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
                    Type::Meta(Kind::Type, 1),
                    Type::Meta(Kind::Type, 3),
                ),
                Type::arrow(
                    ctx.common_kinds,
                    Type::Meta(Kind::Type, 1),
                    Type::Meta(Kind::Type, 3),
                ),
            ),
        ));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_array_1() {
    with_empty_ctx(|ctx| {
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
        let expected = Ok((
            Expr::Array(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)]),
            Type::app(Type::mk_array(ctx.common_kinds), Type::Int),
        ));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_array_2() {
    with_empty_ctx(|ctx| {
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
                syntax::Type::Int,
                syntax::Type::Bool
            )
            .with_position(4))
        )
    })
}

#[test]
fn unify_1() {
    let type_variables = {
        let mut type_variables = BoundVars::new();
        type_variables.insert(&[(Rc::from("r"), Kind::Row)]);
        type_variables
    };
    with_type_variables_ctx(type_variables, |ctx| {
        let real = Type::arrow(
            ctx.common_kinds,
            Type::app(
                Type::mk_record_ctor(ctx.common_kinds),
                Type::mk_rowcons(Rc::from("x"), Type::Int, Type::Var(Kind::Row, 0)),
            ),
            Type::Int,
        );
        let m_0 = ctx.fresh_type_meta(&Kind::Type);
        let m_1 = ctx.fresh_type_meta(&Kind::Type);
        let holey = Type::arrow(ctx.common_kinds, m_1, m_0);
        let expected = Ok(real.clone());
        let actual = ctx.unify(None, &real, &holey).map(|_| ctx.zonk_type(holey));
        assert_eq!(expected, actual)
    })
}

#[test]
fn unify_rows_1() {
    with_empty_ctx(|ctx| {
        assert_eq!(
            ctx.unify(
                None,
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
    with_empty_ctx(|ctx| {
        assert_eq!(
            ctx.unify(
                None,
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
    with_empty_ctx(|ctx| {
        assert_eq!(
            ctx.unify(
                None,
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
    with_empty_ctx(|ctx| {
        let ty1 = Type::mk_record(
            ctx.common_kinds,
            vec![
                (Rc::from("x"), Type::Int),
                (Rc::from("x"), Type::Bool),
                (Rc::from("y"), Type::Bool),
            ],
            None,
        );
        let ty2 = Type::mk_record(
            ctx.common_kinds,
            vec![
                (Rc::from("x"), Type::Int),
                (Rc::from("y"), Type::Bool),
                (Rc::from("x"), Type::Int),
            ],
            None,
        );
        let expected = Err(InferenceError::mismatch(
            &Source::Interactive {
                label: String::from(SOURCE_LABEL),
            },
            ctx.zonk_type(ty1.clone())
                .to_syntax()
                .map(&mut |ix| ctx.type_variables.lookup_index(*ix).unwrap().0.clone()),
            ctx.zonk_type(ty2.clone())
                .to_syntax()
                .map(&mut |ix| ctx.type_variables.lookup_index(*ix).unwrap().0.clone()),
        ));
        let actual = ctx.unify(None, &ty1, &ty2);
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_record_1() {
    with_empty_ctx(|ctx| {
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
    with_empty_ctx(|ctx| {
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
                        (Expr::mk_placeholder(0), Expr::Int(1)),
                        (Expr::mk_placeholder(1), Expr::True)
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
    with_empty_ctx(|ctx| {
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
        let expected = Ok((
            Expr::mk_record(
                vec![
                    (Expr::mk_placeholder(0), Expr::Int(1)),
                    (Expr::mk_placeholder(1), Expr::True),
                ],
                Some(Expr::mk_record(
                    vec![(Expr::mk_placeholder(2), Expr::Char('c'))],
                    None,
                )),
            ),
            Type::mk_record(
                ctx.common_kinds,
                vec![
                    (Rc::from("x"), Type::Int),
                    (Rc::from("y"), Type::Bool),
                    (Rc::from("z"), Type::Char),
                ],
                None,
            ),
        ));
        let actual = ctx
            .infer(&syntax::Spanned { pos: 0, item: term })
            .map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_record_4() {
    with_empty_ctx(|ctx| {
        // { x = 1, y = true, ..1 }
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
        let expected = Err(InferenceError::mismatch(
            &Source::Interactive {
                label: String::from(SOURCE_LABEL),
            },
            syntax::Type::mk_record(Vec::new(), Some(syntax::Type::Meta(0))),
            syntax::Type::Int,
        )
        .with_position(22));
        let actual = ctx
            .infer(&syntax::Spanned { pos: 0, item: term })
            .map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_case_1() {
    with_empty_ctx(|ctx| {
        /*
        \x -> case x of
          X a -> a
        */
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Spanned {
                    pos: 1,
                    item: syntax::Pattern::Name(syntax::Spanned {
                        pos: 1,
                        item: String::from("x"),
                    }),
                }],
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
                    vec![(Rc::from("X"), Type::Meta(Kind::Type, 2))],
                    None,
                ),
                &Type::Meta(Kind::Type, 2),
            ),
        ));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_case_2() {
    with_empty_ctx(|ctx| {
        /*
        \x -> case x of
          Left a -> a
          Right b -> b
        */
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Spanned {
                    pos: 1,
                    item: syntax::Pattern::Name(syntax::Spanned {
                        pos: 1,
                        item: String::from("x"),
                    }),
                }],
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
                        (Rc::from("Left"), Type::Meta(Kind::Type, 4)),
                        (Rc::from("Right"), Type::Meta(Kind::Type, 4)),
                    ],
                    None,
                ),
                &Type::Meta(Kind::Type, 4),
            ),
        ));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_case_3() {
    with_empty_ctx(|ctx| {
        /*
        \x -> case x of
          Left a -> a
          Right b -> b
          _ -> 1
        */
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Spanned {
                    pos: 1,
                    item: syntax::Pattern::Name(syntax::Spanned {
                        pos: 1,
                        item: String::from("x"),
                    }),
                }],
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
                    Some(Type::Meta(Kind::Row, 5)),
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
    with_empty_ctx(|ctx| {
        /*
        \x -> case x of
          Left a -> a
          Left b -> b
          _ -> 1
        */
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Spanned {
                    pos: 1,
                    item: syntax::Pattern::Name(syntax::Spanned {
                        pos: 1,
                        item: String::from("x"),
                    }),
                }],
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
        let expected = Err(InferenceError::redundant_pattern(&Source::Interactive {
            label: String::from(SOURCE_LABEL),
        })
        .with_position(32));
        let actual = ctx.infer(&term).map(|(expr, ty)| (expr, ctx.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn unify_variant_1() {
    let type_variables = {
        let mut type_variables = BoundVars::new();
        type_variables.insert(&[(Rc::from("x"), Kind::Type), (Rc::from("r"), Kind::Row)]);
        type_variables
    };

    with_type_variables_ctx(type_variables, |ctx| {
        let x = Type::Var(Kind::Type, 1);
        let r = Type::Var(Kind::Row, 0);
        let b: Rc<str> = Rc::from("B");

        // (| B : x, r |)
        let ty1 = Type::mk_variant(
            ctx.common_kinds,
            vec![(b.clone(), x.clone())],
            Some(r.clone()),
        );

        // (| B : x, A : x, r |)
        let ty2 = Type::mk_variant(
            ctx.common_kinds,
            vec![(b, x.clone()), (Rc::from("A"), x)],
            Some(r),
        );

        let expected = Err(InferenceError::mismatch(
            &Source::Interactive {
                label: String::from(SOURCE_LABEL),
            },
            ty1.to_syntax()
                .map(&mut |ix| ctx.type_variables.lookup_index(*ix).unwrap().0.clone()),
            ty2.to_syntax()
                .map(&mut |ix| ctx.type_variables.lookup_index(*ix).unwrap().0.clone()),
        ));
        let actual = ctx.unify(None, &ty1, &ty2);

        assert_eq!(expected, actual);
    })
}
