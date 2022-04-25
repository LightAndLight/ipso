use crate::{operator::operator, ParseResult};
use ipso_syntax::{Binop, Expr, Spanned};

#[test]
fn all_left_associative() {
    // 1 + 2 + 3 + 4
    let a = Spanned {
        pos: 0,
        item: Expr::Int(1),
    };
    let b = Spanned {
        pos: 4,
        item: Expr::Int(2),
    };
    let c = Spanned {
        pos: 8,
        item: Expr::Int(3),
    };
    let d = Spanned {
        pos: 12,
        item: Expr::Int(4),
    };
    let expected = ParseResult::pure(Expr::mk_binop(
        Spanned {
            pos: 10,
            item: Binop::Add,
        },
        Expr::mk_binop(
            Spanned {
                pos: 6,
                item: Binop::Add,
            },
            Expr::mk_binop(
                Spanned {
                    pos: 2,
                    item: Binop::Add,
                },
                a.clone(),
                b.clone(),
            ),
            c.clone(),
        ),
        d.clone(),
    ));
    let actual = operator(
        a,
        &mut vec![
            (
                Spanned {
                    pos: 2,
                    item: Binop::Add,
                },
                b,
            ),
            (
                Spanned {
                    pos: 6,
                    item: Binop::Add,
                },
                c,
            ),
            (
                Spanned {
                    pos: 10,
                    item: Binop::Add,
                },
                d,
            ),
        ]
        .into_iter()
        .map(ParseResult::pure),
    );
    assert_eq!(expected, actual)
}

#[test]
fn all_right_associative() {
    // 1 || 2 || 3 || 4
    let a = Spanned {
        pos: 0,
        item: Expr::Int(1),
    };
    let b = Spanned {
        pos: 5,
        item: Expr::Int(2),
    };
    let c = Spanned {
        pos: 10,
        item: Expr::Int(3),
    };
    let d = Spanned {
        pos: 15,
        item: Expr::Int(4),
    };
    let expected = ParseResult::pure(Expr::mk_binop(
        Spanned {
            pos: 2,
            item: Binop::Or,
        },
        a.clone(),
        Expr::mk_binop(
            Spanned {
                pos: 7,
                item: Binop::Or,
            },
            b.clone(),
            Expr::mk_binop(
                Spanned {
                    pos: 12,
                    item: Binop::Or,
                },
                c.clone(),
                d.clone(),
            ),
        ),
    ));
    let actual = operator(
        a,
        &mut vec![
            (
                Spanned {
                    pos: 2,
                    item: Binop::Or,
                },
                b,
            ),
            (
                Spanned {
                    pos: 7,
                    item: Binop::Or,
                },
                c,
            ),
            (
                Spanned {
                    pos: 12,
                    item: Binop::Or,
                },
                d,
            ),
        ]
        .into_iter()
        .map(ParseResult::pure),
    );
    assert_eq!(expected, actual)
}

#[test]
fn precedence_higher_lower() {
    // 1 * 2 + 3
    let a = Spanned {
        pos: 0,
        item: Expr::Int(1),
    };
    let b = Spanned {
        pos: 4,
        item: Expr::Int(2),
    };
    let c = Spanned {
        pos: 8,
        item: Expr::Int(3),
    };
    let expected = ParseResult::pure(Expr::mk_binop(
        Spanned {
            pos: 6,
            item: Binop::Add,
        },
        Expr::mk_binop(
            Spanned {
                pos: 2,
                item: Binop::Multiply,
            },
            a.clone(),
            b.clone(),
        ),
        c.clone(),
    ));
    let actual = operator(
        a,
        &mut vec![
            (
                Spanned {
                    pos: 2,
                    item: Binop::Multiply,
                },
                b,
            ),
            (
                Spanned {
                    pos: 6,
                    item: Binop::Add,
                },
                c,
            ),
        ]
        .into_iter()
        .map(ParseResult::pure),
    );
    assert_eq!(expected, actual)
}

#[test]
fn precedence_higher_lower_lower() {
    // 1 * 2 + 3 == 4
    let a = Spanned {
        pos: 0,
        item: Expr::Int(1),
    };
    let b = Spanned {
        pos: 4,
        item: Expr::Int(2),
    };
    let c = Spanned {
        pos: 8,
        item: Expr::Int(3),
    };
    let d = Spanned {
        pos: 13,
        item: Expr::Int(4),
    };
    let expected = ParseResult::pure(Expr::mk_binop(
        Spanned {
            pos: 10,
            item: Binop::Eq,
        },
        Expr::mk_binop(
            Spanned {
                pos: 6,
                item: Binop::Add,
            },
            Expr::mk_binop(
                Spanned {
                    pos: 2,
                    item: Binop::Multiply,
                },
                a.clone(),
                b.clone(),
            ),
            c.clone(),
        ),
        d.clone(),
    ));
    let actual = operator(
        a,
        &mut vec![
            (
                Spanned {
                    pos: 2,
                    item: Binop::Multiply,
                },
                b,
            ),
            (
                Spanned {
                    pos: 6,
                    item: Binop::Add,
                },
                c,
            ),
            (
                Spanned {
                    pos: 10,
                    item: Binop::Eq,
                },
                d,
            ),
        ]
        .into_iter()
        .map(ParseResult::pure),
    );
    assert_eq!(expected, actual)
}

#[test]
fn precedence_higher_lower_equal() {
    // 1 * 2 + 3 + 4
    let a = Spanned {
        pos: 0,
        item: Expr::Int(1),
    };
    let b = Spanned {
        pos: 4,
        item: Expr::Int(2),
    };
    let c = Spanned {
        pos: 8,
        item: Expr::Int(3),
    };
    let d = Spanned {
        pos: 12,
        item: Expr::Int(4),
    };
    let expected = ParseResult::pure(Expr::mk_binop(
        Spanned {
            pos: 10,
            item: Binop::Add,
        },
        Expr::mk_binop(
            Spanned {
                pos: 6,
                item: Binop::Add,
            },
            Expr::mk_binop(
                Spanned {
                    pos: 2,
                    item: Binop::Multiply,
                },
                a.clone(),
                b.clone(),
            ),
            c.clone(),
        ),
        d.clone(),
    ));
    let actual = operator(
        a,
        &mut vec![
            (
                Spanned {
                    pos: 2,
                    item: Binop::Multiply,
                },
                b,
            ),
            (
                Spanned {
                    pos: 6,
                    item: Binop::Add,
                },
                c,
            ),
            (
                Spanned {
                    pos: 10,
                    item: Binop::Add,
                },
                d,
            ),
        ]
        .into_iter()
        .map(ParseResult::pure),
    );
    assert_eq!(expected, actual)
}

#[test]
fn precedence_higher_lower_higher() {
    // 1 * 2 + 3 * 4
    let a = Spanned {
        pos: 0,
        item: Expr::Int(1),
    };
    let b = Spanned {
        pos: 4,
        item: Expr::Int(2),
    };
    let c = Spanned {
        pos: 8,
        item: Expr::Int(3),
    };
    let d = Spanned {
        pos: 12,
        item: Expr::Int(4),
    };
    let expected = ParseResult::pure(Expr::mk_binop(
        Spanned {
            pos: 6,
            item: Binop::Add,
        },
        Expr::mk_binop(
            Spanned {
                pos: 2,
                item: Binop::Multiply,
            },
            a.clone(),
            b.clone(),
        ),
        Expr::mk_binop(
            Spanned {
                pos: 10,
                item: Binop::Multiply,
            },
            c.clone(),
            d.clone(),
        ),
    ));
    let actual = operator(
        a,
        &mut vec![
            (
                Spanned {
                    pos: 2,
                    item: Binop::Multiply,
                },
                b,
            ),
            (
                Spanned {
                    pos: 6,
                    item: Binop::Add,
                },
                c,
            ),
            (
                Spanned {
                    pos: 10,
                    item: Binop::Multiply,
                },
                d,
            ),
        ]
        .into_iter()
        .map(ParseResult::pure),
    );
    assert_eq!(expected, actual)
}

#[test]
fn precedence_lower_higher() {
    // 1 + 2 * 3
    let a = Spanned {
        pos: 0,
        item: Expr::Int(1),
    };
    let b = Spanned {
        pos: 4,
        item: Expr::Int(2),
    };
    let c = Spanned {
        pos: 8,
        item: Expr::Int(3),
    };
    let expected = ParseResult::pure(Expr::mk_binop(
        Spanned {
            pos: 2,
            item: Binop::Add,
        },
        a.clone(),
        Expr::mk_binop(
            Spanned {
                pos: 6,
                item: Binop::Multiply,
            },
            b.clone(),
            c.clone(),
        ),
    ));
    let actual = operator(
        a,
        &mut vec![
            (
                Spanned {
                    pos: 2,
                    item: Binop::Add,
                },
                b,
            ),
            (
                Spanned {
                    pos: 6,
                    item: Binop::Multiply,
                },
                c,
            ),
        ]
        .into_iter()
        .map(ParseResult::pure),
    );
    assert_eq!(expected, actual)
}

#[test]
fn precedence_lower_higher_lower() {
    // 1 + 2 * 3 + 4
    let a = Spanned {
        pos: 0,
        item: Expr::Int(1),
    };
    let b = Spanned {
        pos: 4,
        item: Expr::Int(2),
    };
    let c = Spanned {
        pos: 8,
        item: Expr::Int(3),
    };
    let d = Spanned {
        pos: 12,
        item: Expr::Int(4),
    };
    let expected = ParseResult::pure(Expr::mk_binop(
        Spanned {
            pos: 10,
            item: Binop::Add,
        },
        Expr::mk_binop(
            Spanned {
                pos: 2,
                item: Binop::Add,
            },
            a.clone(),
            Expr::mk_binop(
                Spanned {
                    pos: 6,
                    item: Binop::Multiply,
                },
                b.clone(),
                c.clone(),
            ),
        ),
        d.clone(),
    ));
    let actual = operator(
        a,
        &mut vec![
            (
                Spanned {
                    pos: 2,
                    item: Binop::Add,
                },
                b,
            ),
            (
                Spanned {
                    pos: 6,
                    item: Binop::Multiply,
                },
                c,
            ),
            (
                Spanned {
                    pos: 10,
                    item: Binop::Add,
                },
                d,
            ),
        ]
        .into_iter()
        .map(ParseResult::pure),
    );
    assert_eq!(expected, actual)
}

#[test]
fn precedence_lower_higher_equal() {
    // 1 + 2 * 3 * 4
    let a = Spanned {
        pos: 0,
        item: Expr::Int(1),
    };
    let b = Spanned {
        pos: 4,
        item: Expr::Int(2),
    };
    let c = Spanned {
        pos: 8,
        item: Expr::Int(3),
    };
    let d = Spanned {
        pos: 12,
        item: Expr::Int(4),
    };
    let expected = ParseResult::pure(Expr::mk_binop(
        Spanned {
            pos: 2,
            item: Binop::Add,
        },
        a.clone(),
        Expr::mk_binop(
            Spanned {
                pos: 10,
                item: Binop::Multiply,
            },
            Expr::mk_binop(
                Spanned {
                    pos: 6,
                    item: Binop::Multiply,
                },
                b.clone(),
                c.clone(),
            ),
            d.clone(),
        ),
    ));
    let actual = operator(
        a,
        &mut vec![
            (
                Spanned {
                    pos: 2,
                    item: Binop::Add,
                },
                b,
            ),
            (
                Spanned {
                    pos: 6,
                    item: Binop::Multiply,
                },
                c,
            ),
            (
                Spanned {
                    pos: 10,
                    item: Binop::Multiply,
                },
                d,
            ),
        ]
        .into_iter()
        .map(ParseResult::pure),
    );
    assert_eq!(expected, actual)
}

#[test]
fn ambiguous() {
    // 1 == 2 == 3
    let a = Spanned {
        pos: 0,
        item: Expr::Int(1),
    };
    let b = Spanned {
        pos: 5,
        item: Expr::Int(2),
    };
    let c = Spanned {
        pos: 10,
        item: Expr::Int(3),
    };
    let expected = ParseResult::ambiguous_use_of(Spanned {
        pos: 7,
        item: Binop::Eq,
    });
    let actual = operator(
        a,
        &mut vec![
            (
                Spanned {
                    pos: 2,
                    item: Binop::Eq,
                },
                b,
            ),
            (
                Spanned {
                    pos: 7,
                    item: Binop::Eq,
                },
                c,
            ),
        ]
        .into_iter()
        .map(ParseResult::pure),
    );
    assert_eq!(expected, actual)
}
