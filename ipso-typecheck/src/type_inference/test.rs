use super::{check_pattern, fresh_type_meta, CheckedPattern, Env, Error, State};
use crate::{
    type_inference::{infer, unification},
    BoundVars,
};
use ipso_core::{Branch, CommonKinds, Expr, Pattern, Type};
use ipso_diagnostic::Source;
use ipso_syntax::{self as syntax, kind::Kind, Spanned};
use pretty_assertions::assert_eq;
use std::{collections::HashMap, rc::Rc};
use syntax::desugar::desugar_expr;

const SOURCE_LABEL: &str = "test";

fn with_type_variables_env_and_state<A, F: FnOnce(Env, &mut State) -> A>(
    type_variables: BoundVars<Kind>,
    f: F,
) -> A {
    let common_kinds = CommonKinds::default();
    let source = Source::Interactive {
        label: String::from(SOURCE_LABEL),
    };
    let modules = HashMap::new();
    let types = HashMap::new();
    let type_signatures = HashMap::new();
    let env = Env {
        common_kinds: &common_kinds,
        modules: &modules,
        types: &types,
        type_variables: &type_variables,
        type_signatures: &type_signatures,
        source: &source,
    };
    let mut state = State::new();
    f(env, &mut state)
}

fn with_empty_env_and_state<A, F: FnOnce(Env, &mut State) -> A>(f: F) -> A {
    with_type_variables_env_and_state(BoundVars::new(), f)
}

#[test]
fn occurs_1() {
    with_empty_env_and_state(|env, state| {
        let v1 = state.fresh_type_meta(Kind::Type);
        let v2 = state.fresh_type_meta(Kind::Type);
        let expected = Err(unification::Error::occurs(
            0,
            syntax::Type::mk_arrow(
                v1.to_syntax()
                    .map(&mut |ix| env.type_variables.lookup_index(*ix).unwrap().0.clone()),
                v2.to_syntax()
                    .map(&mut |ix| env.type_variables.lookup_index(*ix).unwrap().0.clone()),
            ),
        )
        .with_hint(unification::ErrorHint::WhileUnifying {
            expected: v1
                .to_syntax()
                .map(&mut |ix| env.type_variables.lookup_index(*ix).unwrap().0.clone()),
            actual: Type::mk_arrow(env.common_kinds, &v1, &v2)
                .to_syntax()
                .map(&mut |ix| env.type_variables.lookup_index(*ix).unwrap().0.clone()),
        }));
        let actual = unification::unify(
            unification::Env {
                common_kinds: env.common_kinds,
                types: env.types,
                type_variables: env.type_variables,
            },
            &mut state.kind_inference_state,
            &mut state.type_solutions,
            0,
            &v1,
            &Type::mk_arrow(env.common_kinds, &v1, &v2),
        );
        assert_eq!(expected, actual)
    })
}

pub fn infer_pattern(
    env: Env,
    state: &mut State,
    pattern: &Spanned<syntax::Pattern>,
) -> Result<(CheckedPattern, Type), Error> {
    let expected = fresh_type_meta(&mut state.type_solutions, Kind::Type);
    let pattern = check_pattern(env, state, pattern, &expected)?;
    Ok((pattern, expected))
}

#[test]
fn infer_pattern_1() {
    with_empty_env_and_state(|env, state| {
        let pattern = syntax::Spanned {
            pos: 0,
            item: syntax::Pattern::Name(Spanned {
                pos: 0,
                item: Rc::from("x"),
            }),
        };
        assert_eq!(
            infer_pattern(env, state, &pattern),
            Ok((
                CheckedPattern::Any {
                    pattern: Pattern::Name,
                    names: vec![(Rc::from("x"), Type::Meta(Kind::Type, 0))],
                },
                Type::Meta(Kind::Type, 0)
            ))
        )
    })
}

fn zonk_inferred_pattern(
    state: &mut State,
    pattern: (CheckedPattern, Type),
) -> (CheckedPattern, Type) {
    match pattern {
        (CheckedPattern::Any { pattern, names }, ty) => (
            CheckedPattern::Any {
                pattern,
                names: names
                    .into_iter()
                    .map(|(name, ty)| (name, state.zonk_type(ty)))
                    .collect(),
            },
            state.zonk_type(ty),
        ),
        (
            CheckedPattern::Variant {
                tag,
                ctor,
                arg_name,
                arg_ty,
                rest,
            },
            ty,
        ) => (
            CheckedPattern::Variant {
                tag,
                ctor,
                arg_name,
                arg_ty: state.zonk_type(arg_ty),
                rest: state.zonk_type(rest),
            },
            state.zonk_type(ty),
        ),
    }
}

#[test]
fn infer_pattern_2() {
    with_empty_env_and_state(|env, state| {
        let pat = syntax::Spanned {
            pos: 0,
            item: syntax::Pattern::Record {
                names: vec![
                    syntax::Spanned {
                        pos: 0,
                        item: Rc::from("x"),
                    },
                    syntax::Spanned {
                        pos: 2,
                        item: Rc::from("y"),
                    },
                    syntax::Spanned {
                        pos: 4,
                        item: Rc::from("z"),
                    },
                ],
                rest: None,
            },
        };
        let expected = Ok((
            CheckedPattern::Any {
                pattern: Pattern::Record {
                    names: vec![
                        Expr::mk_placeholder(0),
                        Expr::mk_placeholder(1),
                        Expr::mk_placeholder(2),
                    ],
                    rest: false,
                },
                names: vec![
                    (Rc::from("x"), Type::Meta(Kind::Type, 1)),
                    (Rc::from("y"), Type::Meta(Kind::Type, 2)),
                    (Rc::from("z"), Type::Meta(Kind::Type, 3)),
                ],
            },
            Type::mk_record(
                env.common_kinds,
                vec![
                    (Rc::from("x"), Type::Meta(Kind::Type, 1)),
                    (Rc::from("y"), Type::Meta(Kind::Type, 2)),
                    (Rc::from("z"), Type::Meta(Kind::Type, 3)),
                ],
                None,
            ),
        ));
        let actual =
            infer_pattern(env, state, &pat).map(|pattern| zonk_inferred_pattern(state, pattern));

        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_pattern_3() {
    with_empty_env_and_state(|env, state| {
        let pat = syntax::Spanned {
            pos: 0,
            item: syntax::Pattern::Record {
                names: vec![
                    syntax::Spanned {
                        pos: 0,
                        item: Rc::from("x"),
                    },
                    syntax::Spanned {
                        pos: 2,
                        item: Rc::from("y"),
                    },
                    syntax::Spanned {
                        pos: 4,
                        item: Rc::from("z"),
                    },
                ],
                rest: Some(syntax::Spanned {
                    pos: 6,
                    item: Rc::from("w"),
                }),
            },
        };
        let expected = Ok((
            CheckedPattern::Any {
                pattern: Pattern::Record {
                    names: vec![
                        Expr::mk_placeholder(0),
                        Expr::mk_placeholder(1),
                        Expr::mk_placeholder(2),
                    ],
                    rest: true,
                },
                names: vec![
                    (Rc::from("x"), Type::Meta(Kind::Type, 1)),
                    (Rc::from("y"), Type::Meta(Kind::Type, 2)),
                    (Rc::from("z"), Type::Meta(Kind::Type, 3)),
                    (
                        Rc::from("w"),
                        Type::mk_record(
                            env.common_kinds,
                            Vec::new(),
                            Some(Type::Meta(Kind::Row, 4)),
                        ),
                    ),
                ],
            },
            Type::mk_record(
                env.common_kinds,
                vec![
                    (Rc::from("x"), Type::Meta(Kind::Type, 1)),
                    (Rc::from("y"), Type::Meta(Kind::Type, 2)),
                    (Rc::from("z"), Type::Meta(Kind::Type, 3)),
                ],
                Some(Type::Meta(Kind::Row, 4)),
            ),
        ));
        let actual =
            infer_pattern(env, state, &pat).map(|pattern| zonk_inferred_pattern(state, pattern));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_pattern_4() {
    with_empty_env_and_state(|env, state| {
        let pat = syntax::Spanned {
            pos: 0,
            item: syntax::Pattern::Variant {
                name: Rc::from("just"),
                arg: syntax::Spanned {
                    pos: 5,
                    item: Box::new(syntax::Pattern::Name(Spanned {
                        pos: 5,
                        item: Rc::from("x"),
                    })),
                },
            },
        };
        let expected = Ok((
            CheckedPattern::Variant {
                tag: Rc::new(Expr::mk_placeholder(0)),
                ctor: Rc::from("just"),
                arg_name: Rc::from("x"),
                arg_ty: Type::Meta(Kind::Type, 1),
                rest: Type::Meta(Kind::Row, 2),
            },
            Type::Meta(Kind::Type, 0),
        ));
        let actual = infer_pattern(env, state, &pat);
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_lam_1() {
    with_empty_env_and_state(|env, state| {
        // \x -> x
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![syntax::Spanned {
                    pos: 1,
                    item: syntax::Pattern::Name(syntax::Spanned {
                        pos: 1,
                        item: Rc::from("x"),
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
                env.common_kinds,
                Type::Meta(Kind::Type, 1),
                Type::Meta(Kind::Type, 1),
            ),
        ));
        let actual = infer(env, state, &term).map(|(expr, ty)| (expr, state.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_lam_2() {
    with_empty_env_and_state(|env, state| {
        // \{x, y} -> x
        let term = desugar_expr(
            &Source::Interactive {
                label: String::from(SOURCE_LABEL),
            },
            syntax::Spanned {
                pos: 0,
                item: syntax::Expr::mk_lam(
                    vec![syntax::Spanned {
                        pos: 1,
                        item: syntax::Pattern::Record {
                            names: vec![
                                syntax::Spanned {
                                    pos: 2,
                                    item: Rc::from("x"),
                                },
                                syntax::Spanned {
                                    pos: 5,
                                    item: Rc::from("y"),
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
            },
        )
        .unwrap();
        let actual = infer(env, state, &term).map(|(expr, ty)| (expr, state.zonk_type(ty)));
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
                env.common_kinds,
                Type::mk_record(
                    env.common_kinds,
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
    with_empty_env_and_state(|env, state| {
        // \{x, y} -> y
        let term = desugar_expr(
            &Source::Interactive {
                label: String::from(SOURCE_LABEL),
            },
            syntax::Spanned {
                pos: 0,
                item: syntax::Expr::mk_lam(
                    vec![syntax::Spanned {
                        pos: 1,
                        item: syntax::Pattern::Record {
                            names: vec![
                                syntax::Spanned {
                                    pos: 2,
                                    item: Rc::from("x"),
                                },
                                syntax::Spanned {
                                    pos: 5,
                                    item: Rc::from("y"),
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
            },
        )
        .unwrap();
        let actual = infer(env, state, &term).map(|(expr, ty)| (expr, state.zonk_type(ty)));
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
                env.common_kinds,
                Type::mk_record(
                    env.common_kinds,
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
    with_empty_env_and_state(|env, state| {
        // \{x, y, ...z} -> z
        let term = desugar_expr(
            &Source::Interactive {
                label: String::from(SOURCE_LABEL),
            },
            syntax::Spanned {
                pos: 0,
                item: syntax::Expr::mk_lam(
                    vec![syntax::Spanned {
                        pos: 1,
                        item: syntax::Pattern::Record {
                            names: vec![
                                syntax::Spanned {
                                    pos: 2,
                                    item: Rc::from("x"),
                                },
                                syntax::Spanned {
                                    pos: 5,
                                    item: Rc::from("y"),
                                },
                            ],
                            rest: Some(syntax::Spanned {
                                pos: 11,
                                item: Rc::from("z"),
                            }),
                        },
                    }],
                    syntax::Spanned {
                        pos: 17,
                        item: syntax::Expr::Var(String::from("z")),
                    },
                ),
            },
        )
        .unwrap();
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
                env.common_kinds,
                Type::mk_record(
                    env.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Meta(Kind::Type, 4)),
                        (Rc::from("y"), Type::Meta(Kind::Type, 5)),
                    ],
                    Some(Type::Meta(Kind::Row, 6)),
                ),
                Type::mk_record(env.common_kinds, vec![], Some(Type::Meta(Kind::Row, 6))),
            ),
        ));
        let actual = infer(env, state, &term).map(|(expr, ty)| (expr, state.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_lam_5() {
    with_empty_env_and_state(|env, state| {
        // \f x -> f x
        let term = syntax::Spanned {
            pos: 0,
            item: syntax::Expr::mk_lam(
                vec![
                    syntax::Spanned {
                        pos: 1,
                        item: syntax::Pattern::Name(syntax::Spanned {
                            pos: 1,
                            item: Rc::from("f"),
                        }),
                    },
                    syntax::Spanned {
                        pos: 3,
                        item: syntax::Pattern::Name(syntax::Spanned {
                            pos: 3,
                            item: Rc::from("x"),
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
                env.common_kinds,
                Type::arrow(
                    env.common_kinds,
                    Type::Meta(Kind::Type, 2),
                    Type::Meta(Kind::Type, 5),
                ),
                Type::arrow(
                    env.common_kinds,
                    Type::Meta(Kind::Type, 2),
                    Type::Meta(Kind::Type, 5),
                ),
            ),
        ));
        let actual = infer(env, state, &term).map(|(expr, ty)| (expr, state.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_array_1() {
    with_empty_env_and_state(|env, state| {
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
            Type::app(Type::mk_array(env.common_kinds), Type::Int),
        ));
        let actual = infer(env, state, &term).map(|(expr, ty)| (expr, state.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_array_2() {
    with_empty_env_and_state(|env, state| {
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
            infer(env, state, &term),
            Err(Error::unification_error(
                &Source::Interactive {
                    label: String::from(SOURCE_LABEL)
                },
                4,
                unification::Error::mismatch(syntax::Type::Int, syntax::Type::Bool,).with_hint(
                    unification::ErrorHint::WhileUnifying {
                        expected: syntax::Type::Int,
                        actual: syntax::Type::Bool,
                    }
                )
            ))
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
    with_type_variables_env_and_state(type_variables, |env, state| {
        let real = Type::arrow(
            env.common_kinds,
            Type::app(
                Type::mk_record_ctor(env.common_kinds),
                Type::mk_rowcons(Rc::from("x"), Type::Int, Type::Var(Kind::Row, 0)),
            ),
            Type::Int,
        );
        let m_0 = state.fresh_type_meta(Kind::Type);
        let m_1 = state.fresh_type_meta(Kind::Type);
        let holey = Type::arrow(env.common_kinds, m_1, m_0);
        let expected = Ok(real.clone());
        let actual = unification::unify(
            unification::Env {
                common_kinds: env.common_kinds,
                types: env.types,
                type_variables: env.type_variables,
            },
            &mut state.kind_inference_state,
            &mut state.type_solutions,
            0,
            &real,
            &holey,
        )
        .map(|_| state.zonk_type(holey));
        assert_eq!(expected, actual)
    })
}

#[test]
fn unify_rows_1() {
    with_empty_env_and_state(|env, state| {
        assert_eq!(
            unification::unify(
                unification::Env {
                    common_kinds: env.common_kinds,
                    types: env.types,
                    type_variables: env.type_variables,
                },
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                0,
                &Type::mk_record(
                    env.common_kinds,
                    vec![(Rc::from("x"), Type::Int), (Rc::from("y"), Type::Bool)],
                    None
                ),
                &Type::mk_record(
                    env.common_kinds,
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
    with_empty_env_and_state(|env, state| {
        assert_eq!(
            unification::unify(
                unification::Env {
                    common_kinds: env.common_kinds,
                    types: env.types,
                    type_variables: env.type_variables,
                },
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                0,
                &Type::mk_record(
                    env.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Int),
                        (Rc::from("x"), Type::Bool),
                        (Rc::from("y"), Type::Bool)
                    ],
                    None
                ),
                &Type::mk_record(
                    env.common_kinds,
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
    with_empty_env_and_state(|env, state| {
        assert_eq!(
            unification::unify(
                unification::Env {
                    common_kinds: env.common_kinds,
                    types: env.types,
                    type_variables: env.type_variables,
                },
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                0,
                &Type::mk_record(
                    env.common_kinds,
                    vec![
                        (Rc::from("x"), Type::Int),
                        (Rc::from("x"), Type::Bool),
                        (Rc::from("y"), Type::Bool)
                    ],
                    None
                ),
                &Type::mk_record(
                    env.common_kinds,
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
    with_empty_env_and_state(|env, state| {
        let ty1 = Type::mk_record(
            env.common_kinds,
            vec![
                (Rc::from("x"), Type::Int),
                (Rc::from("x"), Type::Bool),
                (Rc::from("y"), Type::Bool),
            ],
            None,
        );
        let ty2 = Type::mk_record(
            env.common_kinds,
            vec![
                (Rc::from("x"), Type::Int),
                (Rc::from("y"), Type::Bool),
                (Rc::from("x"), Type::Int),
            ],
            None,
        );
        let expected = Err(
            unification::Error::mismatch(syntax::Type::Bool, syntax::Type::Int).with_hint(
                unification::ErrorHint::WhileUnifying {
                    expected: state
                        .zonk_type(ty1.clone())
                        .to_syntax()
                        .map(&mut |ix| env.type_variables.lookup_index(*ix).unwrap().0.clone()),
                    actual: state
                        .zonk_type(ty2.clone())
                        .to_syntax()
                        .map(&mut |ix| env.type_variables.lookup_index(*ix).unwrap().0.clone()),
                },
            ),
        );
        let actual = unification::unify(
            unification::Env {
                common_kinds: env.common_kinds,
                types: env.types,
                type_variables: env.type_variables,
            },
            &mut state.kind_inference_state,
            &mut state.type_solutions,
            0,
            &ty1,
            &ty2,
        );
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_record_1() {
    with_empty_env_and_state(|env, state| {
        // {}
        let term = syntax::Expr::mk_record(Vec::new(), None);
        assert_eq!(
            infer(env, state, &syntax::Spanned { pos: 0, item: term })
                .map(|(expr, ty)| (expr, state.zonk_type(ty))),
            Ok((
                Expr::mk_record(Vec::new(), None),
                Type::mk_record(env.common_kinds, Vec::new(), None)
            ))
        )
    })
}

#[test]
fn infer_record_2() {
    with_empty_env_and_state(|env, state| {
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
            infer(env, state, &syntax::Spanned { pos: 0, item: term })
                .map(|(expr, ty)| (expr, state.zonk_type(ty))),
            Ok((
                Expr::mk_record(
                    vec![
                        (Expr::mk_placeholder(0), Expr::Int(1)),
                        (Expr::mk_placeholder(1), Expr::True)
                    ],
                    None
                ),
                Type::mk_record(
                    env.common_kinds,
                    vec![(Rc::from("x"), Type::Int), (Rc::from("y"), Type::Bool)],
                    None
                )
            ))
        )
    })
}

#[test]
fn infer_record_3() {
    with_empty_env_and_state(|env, state| {
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
                env.common_kinds,
                vec![
                    (Rc::from("x"), Type::Int),
                    (Rc::from("y"), Type::Bool),
                    (Rc::from("z"), Type::Char),
                ],
                None,
            ),
        ));
        let actual = infer(env, state, &syntax::Spanned { pos: 0, item: term })
            .map(|(expr, ty)| (expr, state.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_record_4() {
    with_empty_env_and_state(|env, state| {
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
        let expected = Err(Error::unification_error(
            &Source::Interactive {
                label: String::from(SOURCE_LABEL),
            },
            22,
            unification::Error::mismatch(
                syntax::Type::mk_record(Vec::new(), Some(syntax::Type::Meta(1))),
                syntax::Type::Int,
            )
            .with_hint(unification::ErrorHint::WhileUnifying {
                expected: syntax::Type::mk_record(Vec::new(), Some(syntax::Type::Meta(1))),
                actual: syntax::Type::Int,
            }),
        ));
        let actual = infer(env, state, &syntax::Spanned { pos: 0, item: term })
            .map(|(expr, ty)| (expr, state.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_case_1() {
    with_empty_env_and_state(|env, state| {
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
                        item: Rc::from("x"),
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
                                    name: Rc::from("X"),
                                    arg: syntax::Spanned {
                                        pos: 20,
                                        item: Box::new(syntax::Pattern::Name(Spanned {
                                            pos: 20,
                                            item: Rc::from("a"),
                                        })),
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
                env.common_kinds,
                &Type::mk_variant(
                    env.common_kinds,
                    vec![(Rc::from("X"), Type::Meta(Kind::Type, 4))],
                    None,
                ),
                &Type::Meta(Kind::Type, 4),
            ),
        ));
        let actual = infer(env, state, &term).map(|(expr, ty)| (expr, state.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_case_2() {
    with_empty_env_and_state(|env, state| {
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
                        item: Rc::from("x"),
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
                                        name: Rc::from("Left"),
                                        arg: syntax::Spanned {
                                            pos: 23,
                                            item: Box::new(syntax::Pattern::Name(Spanned {
                                                pos: 23,
                                                item: Rc::from("a"),
                                            })),
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
                                        name: Rc::from("Right"),
                                        arg: syntax::Spanned {
                                            pos: 34,
                                            item: Box::new(syntax::Pattern::Name(Spanned {
                                                pos: 34,
                                                item: Rc::from("b"),
                                            })),
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
                env.common_kinds,
                &Type::mk_variant(
                    env.common_kinds,
                    vec![
                        (Rc::from("Left"), Type::Meta(Kind::Type, 6)),
                        (Rc::from("Right"), Type::Meta(Kind::Type, 6)),
                    ],
                    None,
                ),
                &Type::Meta(Kind::Type, 6),
            ),
        ));
        let actual = infer(env, state, &term).map(|(expr, ty)| (expr, state.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_case_3() {
    with_empty_env_and_state(|env, state| {
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
                        item: Rc::from("x"),
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
                                        name: Rc::from("Left"),
                                        arg: syntax::Spanned {
                                            pos: 23,
                                            item: Box::new(syntax::Pattern::Name(Spanned {
                                                pos: 23,
                                                item: Rc::from("a"),
                                            })),
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
                                        name: Rc::from("Right"),
                                        arg: syntax::Spanned {
                                            pos: 34,
                                            item: Box::new(syntax::Pattern::Name(Spanned {
                                                pos: 34,
                                                item: Rc::from("b"),
                                            })),
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
                env.common_kinds,
                &Type::mk_variant(
                    env.common_kinds,
                    vec![
                        (Rc::from("Left"), Type::Int),
                        (Rc::from("Right"), Type::Int),
                    ],
                    Some(Type::Meta(Kind::Row, 7)),
                ),
                &Type::Int,
            ),
        ));
        let actual = infer(env, state, &term).map(|(expr, ty)| (expr, state.zonk_type(ty)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_case_4() {
    with_empty_env_and_state(|env, state| {
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
                        item: Rc::from("x"),
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
                                        name: Rc::from("Left"),
                                        arg: syntax::Spanned {
                                            pos: 23,
                                            item: Box::new(syntax::Pattern::Name(Spanned {
                                                pos: 23,
                                                item: Rc::from("a"),
                                            })),
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
                                        name: Rc::from("Left"),
                                        arg: syntax::Spanned {
                                            pos: 34,
                                            item: Box::new(syntax::Pattern::Name(Spanned {
                                                pos: 34,
                                                item: Rc::from("b"),
                                            })),
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
        let expected = Err(Error::redundant_pattern(
            &Source::Interactive {
                label: String::from(SOURCE_LABEL),
            },
            32,
        ));
        let actual = infer(env, state, &term).map(|(expr, ty)| (expr, state.zonk_type(ty)));
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

    with_type_variables_env_and_state(type_variables, |env, state| {
        let x = Type::Var(Kind::Type, 1);
        let r = Type::Var(Kind::Row, 0);
        let b: Rc<str> = Rc::from("B");

        // (| B : x, r |)
        let ty1 = Type::mk_variant(
            env.common_kinds,
            vec![(b.clone(), x.clone())],
            Some(r.clone()),
        );

        // (| B : x, A : x, r |)
        let ty2 = Type::mk_variant(
            env.common_kinds,
            vec![(b, x.clone()), (Rc::from("A"), x.clone())],
            Some(r.clone()),
        );

        let expected = Err(unification::Error::mismatch(
            r.clone()
                .to_syntax()
                .map(&mut |ix| env.type_variables.lookup_index(*ix).unwrap().0.clone()),
            (Type::mk_rows(vec![(Rc::from("A"), x)], Some(r)))
                .to_syntax()
                .map(&mut |ix| env.type_variables.lookup_index(*ix).unwrap().0.clone()),
        )
        .with_hint(unification::ErrorHint::WhileUnifying {
            expected: ty1
                .to_syntax()
                .map(&mut |ix| env.type_variables.lookup_index(*ix).unwrap().0.clone()),
            actual: ty2
                .to_syntax()
                .map(&mut |ix| env.type_variables.lookup_index(*ix).unwrap().0.clone()),
        }));
        let actual = unification::unify(
            unification::Env {
                common_kinds: env.common_kinds,
                types: env.types,
                type_variables: env.type_variables,
            },
            &mut state.kind_inference_state,
            &mut state.type_solutions,
            0,
            &ty1,
            &ty2,
        );

        assert_eq!(expected, actual);
    })
}
