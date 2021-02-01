#[cfg(test)]
use crate::{core::Expr, syntax::Binop};

#[test]
fn instantiate_1() {
    let term = Expr::mk_lam(
        true,
        Expr::mk_binop(Binop::Multiply, Expr::Int(99), Expr::Var(1)),
    );
    let expected = Expr::mk_lam(
        true,
        Expr::mk_binop(Binop::Multiply, Expr::Int(99), Expr::Int(42)),
    );
    let actual = term.instantiate(&Expr::Int(42));
    assert_eq!(expected, actual)
}

#[test]
fn instantiate_2() {
    let term = Expr::mk_lam(
        false,
        Expr::mk_binop(Binop::Multiply, Expr::Int(99), Expr::Var(0)),
    );
    let expected = Expr::mk_lam(
        false,
        Expr::mk_binop(Binop::Multiply, Expr::Int(99), Expr::Int(42)),
    );
    let actual = term.instantiate(&Expr::Int(42));
    assert_eq!(expected, actual)
}

#[test]
fn map_vars_1() {
    let val = Expr::mk_lam(true, Expr::mk_lam(true, Expr::Var(1)));
    assert_eq!(val, val.map_vars(|n| n));
}
#[test]
fn map_vars_2() {
    let val = Expr::mk_lam(
        true,
        Expr::mk_lam(true, Expr::mk_app(Expr::Var(2), Expr::Var(1))),
    );
    assert_eq!(
        Expr::mk_lam(
            true,
            Expr::mk_lam(true, Expr::mk_app(Expr::Var(3), Expr::Var(1)))
        ),
        val.map_vars(|n| n + 1)
    );
}
