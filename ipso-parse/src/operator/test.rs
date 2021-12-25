#[cfg(test)]
use crate::{operator::operator, ParseResult};
use ipso_syntax::Spanned;
#[cfg(test)]
use ipso_syntax::{Binop, Expr};

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
        Binop::Add,
        Expr::mk_binop(
            Binop::Add,
            Expr::mk_binop(Binop::Add, a.clone(), b.clone()),
            c.clone(),
        ),
        d.clone(),
    ));
    let actual = operator(a, vec![(Binop::Add, b), (Binop::Add, c), (Binop::Add, d)]);
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
        Binop::Or,
        a.clone(),
        Expr::mk_binop(
            Binop::Or,
            b.clone(),
            Expr::mk_binop(Binop::Or, c.clone(), d.clone()),
        ),
    ));
    let actual = operator(a, vec![(Binop::Or, b), (Binop::Or, c), (Binop::Or, d)]);
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
        pos: 5,
        item: Expr::Int(2),
    };
    let c = Spanned {
        pos: 10,
        item: Expr::Int(3),
    };
    let expected = ParseResult::pure(Expr::mk_binop(
        Binop::Add,
        Expr::mk_binop(Binop::Multiply, a.clone(), b.clone()),
        c.clone(),
    ));
    let actual = operator(a, vec![(Binop::Multiply, b), (Binop::Add, c)]);
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
        Binop::Eq,
        Expr::mk_binop(
            Binop::Add,
            Expr::mk_binop(Binop::Multiply, a.clone(), b.clone()),
            c.clone(),
        ),
        d.clone(),
    ));
    let actual = operator(
        a,
        vec![(Binop::Multiply, b), (Binop::Add, c), (Binop::Eq, d)],
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
        Binop::Add,
        Expr::mk_binop(
            Binop::Add,
            Expr::mk_binop(Binop::Multiply, a.clone(), b.clone()),
            c.clone(),
        ),
        d.clone(),
    ));
    let actual = operator(
        a,
        vec![(Binop::Multiply, b), (Binop::Add, c), (Binop::Add, d)],
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
        Binop::Add,
        Expr::mk_binop(Binop::Multiply, a.clone(), b.clone()),
        Expr::mk_binop(Binop::Multiply, c.clone(), d.clone()),
    ));
    let actual = operator(
        a,
        vec![(Binop::Multiply, b), (Binop::Add, c), (Binop::Multiply, d)],
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
        pos: 5,
        item: Expr::Int(2),
    };
    let c = Spanned {
        pos: 10,
        item: Expr::Int(3),
    };
    let expected = ParseResult::pure(Expr::mk_binop(
        Binop::Add,
        a.clone(),
        Expr::mk_binop(Binop::Multiply, b.clone(), c.clone()),
    ));
    let actual = operator(a, vec![(Binop::Add, b), (Binop::Multiply, c)]);
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
        Binop::Add,
        Expr::mk_binop(
            Binop::Add,
            a.clone(),
            Expr::mk_binop(Binop::Multiply, b.clone(), c.clone()),
        ),
        d.clone(),
    ));
    let actual = operator(
        a,
        vec![(Binop::Add, b), (Binop::Multiply, c), (Binop::Add, d)],
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
        Binop::Add,
        a.clone(),
        Expr::mk_binop(
            Binop::Multiply,
            Expr::mk_binop(Binop::Multiply, b.clone(), c.clone()),
            d.clone(),
        ),
    ));
    let actual = operator(
        a,
        vec![(Binop::Add, b), (Binop::Multiply, c), (Binop::Multiply, d)],
    );
    assert_eq!(expected, actual)
}
