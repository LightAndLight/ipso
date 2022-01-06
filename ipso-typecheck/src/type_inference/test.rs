use super::{InferenceContext, InferenceError, InferredPattern, Solutions, UnificationError};
use crate::{evidence, kind_inference, BoundVars};
use ipso_core::{Branch, CommonKinds, Expr, Pattern, Type};
use ipso_diagnostic::Source;
use ipso_syntax::{self as syntax, kind::Kind, Spanned};
use std::{collections::HashMap, rc::Rc};

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
            Err(InferenceError::unification_error(
                &Source::Interactive {
                    label: String::from(SOURCE_LABEL)
                },
                UnificationError::Mismatch {
                    expected: syntax::Type::Int,
                    actual: syntax::Type::Bool
                }
            )
            .with_position(4))
        )
    })
}
