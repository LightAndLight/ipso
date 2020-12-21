use crate::core;
use crate::syntax::{self, Type};
use crate::typecheck::Context;
use crate::typecheck::ContextEntry;
use crate::typecheck::Rope;

use super::{Kind, TypeError, Typechecker};

#[test]
fn infer_kind_test_1() {
    let mut tc = Typechecker::new();
    let expected = Ok(Kind::Type);
    let actual = tc.infer_kind(&Type::Bool);
    assert_eq!(expected, actual)
}

#[test]
fn infer_kind_test_2() {
    let mut tc = Typechecker::new();
    let expected = Ok(Kind::Row);
    let actual = tc.infer_kind(&Type::RowNil);
    assert_eq!(expected, actual)
}

#[test]
fn infer_kind_test_3() {
    let mut tc = Typechecker::new();
    let expected = Err(TypeError::KindMismatch {
        pos: 0,
        expected: Kind::Type,
        actual: Kind::Row,
    });
    let actual = tc.infer_kind(&Type::mk_rowcons(
        String::from("x"),
        Type::RowNil,
        Type::RowNil,
    ));
    assert_eq!(expected, actual)
}

#[test]
fn infer_kind_test_4() {
    let mut tc = Typechecker::new();
    let expected = Ok(Some(Kind::Type));
    let actual = tc
        .infer_kind(&Type::mk_app(
            Type::Record,
            Type::mk_rowcons(String::from("x"), Type::Bool, Type::RowNil),
        ))
        .map(|kind| tc.zonk_kind(kind));
    assert_eq!(expected, actual)
}

#[test]
fn context_test_1() {
    assert_eq!(
        {
            let mut ctx = Context::new();
            ctx.insert(&vec![
                (&String::from("a"), Type::Unit),
                (&String::from("b"), Type::Bool),
                (&String::from("c"), Type::String),
            ]);
            ctx
        },
        Context(
            vec![
                (
                    String::from("a"),
                    vec![ContextEntry {
                        index: 2,
                        ty: Type::Unit
                    }]
                ),
                (
                    String::from("b"),
                    vec![ContextEntry {
                        index: 1,
                        ty: Type::Bool
                    }]
                ),
                (
                    String::from("c"),
                    vec![ContextEntry {
                        index: 0,
                        ty: Type::String
                    }]
                )
            ]
            .into_iter()
            .collect()
        )
    )
}

#[test]
#[should_panic]
fn context_test_2() {
    let mut ctx = Context::new();
    ctx.insert(&vec![
        (&String::from("a"), Type::Unit),
        (&String::from("a"), Type::Bool),
        (&String::from("c"), Type::String),
    ]);
}

#[test]
fn infer_pattern_test_1() {
    let mut tc = Typechecker::new();
    let pat = syntax::Pattern::Name(syntax::Spanned {
        pos: 0,
        item: String::from("x"),
    });
    assert_eq!(
        tc.infer_pattern(&pat),
        (
            core::Pattern::Name,
            syntax::Type::Meta(0),
            vec![(&String::from("x"), syntax::Type::Meta(0))]
        )
    )
}

#[test]
fn infer_pattern_test_2() {
    let mut tc = Typechecker::new();
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
        tc.infer_pattern(&pat),
        (
            core::Pattern::Record {
                names: 3,
                rest: false
            },
            syntax::Type::mk_record(
                vec![
                    (String::from("x"), syntax::Type::Meta(0)),
                    (String::from("y"), syntax::Type::Meta(1)),
                    (String::from("z"), syntax::Type::Meta(2))
                ],
                None
            ),
            vec![
                (&String::from("x"), syntax::Type::Meta(0)),
                (&String::from("y"), syntax::Type::Meta(1)),
                (&String::from("z"), syntax::Type::Meta(2)),
            ]
        )
    )
}

#[test]
fn infer_pattern_test_3() {
    let mut tc = Typechecker::new();
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
        tc.infer_pattern(&pat),
        (
            core::Pattern::Record {
                names: 3,
                rest: true
            },
            syntax::Type::mk_record(
                vec![
                    (String::from("x"), syntax::Type::Meta(0)),
                    (String::from("y"), syntax::Type::Meta(1)),
                    (String::from("z"), syntax::Type::Meta(2))
                ],
                Some(syntax::Type::Meta(3))
            ),
            vec![
                (&String::from("x"), syntax::Type::Meta(0)),
                (&String::from("y"), syntax::Type::Meta(1)),
                (&String::from("z"), syntax::Type::Meta(2)),
                (
                    &String::from("w"),
                    syntax::Type::mk_record(Vec::new(), Some(syntax::Type::Meta(3)))
                ),
            ]
        )
    )
}

#[test]
fn infer_pattern_test_4() {
    let mut tc = Typechecker::new();
    let pat = syntax::Pattern::Variant {
        name: String::from("just"),
        arg: syntax::Spanned {
            pos: 5,
            item: String::from("x"),
        },
    };
    assert_eq!(
        tc.infer_pattern(&pat),
        (
            core::Pattern::Variant {
                name: String::from("just")
            },
            syntax::Type::mk_variant(
                vec![(String::from("just"), syntax::Type::Meta(0))],
                Some(syntax::Type::Meta(1))
            ),
            vec![(&String::from("x"), syntax::Type::Meta(0))]
        )
    )
}

#[test]
fn infer_lam_test_1() {
    let mut tc = Typechecker::new();
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
    assert_eq!(
        tc.infer_expr(term),
        Ok((
            core::Expr::mk_lam(core::Pattern::Name, core::Expr::Var(0)),
            syntax::Type::mk_arrow(syntax::Type::Meta(0), syntax::Type::Meta(0))
        ))
    )
}

#[test]
fn infer_lam_test_2() {
    let mut tc = Typechecker::new();
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
    assert_eq!(
        tc.infer_expr(term),
        Ok((
            core::Expr::mk_lam(
                core::Pattern::Record {
                    names: 2,
                    rest: false
                },
                core::Expr::Var(1)
            ),
            syntax::Type::mk_arrow(
                syntax::Type::mk_record(
                    vec![
                        (String::from("x"), syntax::Type::Meta(0)),
                        (String::from("y"), syntax::Type::Meta(1))
                    ],
                    None
                ),
                syntax::Type::Meta(0)
            )
        ))
    )
}

#[test]
fn infer_lam_test_3() {
    let mut tc = Typechecker::new();
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
    assert_eq!(
        tc.infer_expr(term),
        Ok((
            core::Expr::mk_lam(
                core::Pattern::Record {
                    names: 2,
                    rest: false
                },
                core::Expr::Var(0)
            ),
            syntax::Type::mk_arrow(
                syntax::Type::mk_record(
                    vec![
                        (String::from("x"), syntax::Type::Meta(0)),
                        (String::from("y"), syntax::Type::Meta(1))
                    ],
                    None
                ),
                syntax::Type::Meta(1)
            )
        ))
    )
}

#[test]
fn infer_lam_test_4() {
    let mut tc = Typechecker::new();
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
    assert_eq!(
        tc.infer_expr(term),
        Ok((
            core::Expr::mk_lam(
                core::Pattern::Record {
                    names: 2,
                    rest: true
                },
                core::Expr::Var(0)
            ),
            syntax::Type::mk_arrow(
                syntax::Type::mk_record(
                    vec![
                        (String::from("x"), syntax::Type::Meta(0)),
                        (String::from("y"), syntax::Type::Meta(1))
                    ],
                    Some(syntax::Type::Meta(2))
                ),
                syntax::Type::mk_record(vec![], Some(syntax::Type::Meta(2)))
            )
        ))
    )
}

#[test]
fn infer_array_test_1() {
    let mut tc = Typechecker::new();
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
        tc.infer_expr(term),
        Ok((
            core::Expr::Array(vec![
                core::Expr::Int(1),
                core::Expr::Int(2),
                core::Expr::Int(3)
            ]),
            syntax::Type::mk_app(syntax::Type::Array, syntax::Type::Int)
        ))
    )
}

#[test]
fn infer_array_test_2() {
    let mut tc = Typechecker::new();
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
        tc.infer_expr(term),
        Err(TypeError::TypeMismatch {
            pos: 4,
            expected: syntax::Type::Int,
            actual: syntax::Type::Bool
        })
    )
}

#[test]
fn rope_test_1() {
    assert_eq!(
        Rope::from_vec(&vec![0, 1, 2, 3, 4])
            .delete(1)
            .map(|xs| xs.iter().collect()),
        Ok(vec![&0, &2, &3, &4])
    )
}

#[test]
fn rope_test_2() {
    assert_eq!(
        Rope::from_vec(&vec![0, 1, 2, 3, 4])
            .delete(0)
            .map(|xs| xs.iter().collect()),
        Ok(vec![&1, &2, &3, &4])
    )
}

#[test]
fn rope_test_3() {
    assert_eq!(
        Rope::from_vec(&vec![0, 1, 2, 3, 4])
            .delete(4)
            .map(|xs| xs.iter().collect()),
        Ok(vec![&0, &1, &2, &3])
    )
}

#[test]
fn rope_test_4() {
    assert_eq!(
        Rope::from_vec(&vec![0, 1, 2, 3, 4])
            .delete(1)
            .unwrap()
            .delete(2)
            .map(|xs| xs.iter().collect()),
        Ok(vec![&0, &2, &4])
    )
}

#[test]
fn rope_test_5() {
    assert_eq!(
        Rope::from_vec(&vec![0, 1, 2, 3, 4])
            .delete(3)
            .unwrap()
            .delete(1)
            .map(|xs| xs.iter().collect()),
        Ok(vec![&0, &2, &4])
    )
}
