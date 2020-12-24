#[cfg(test)]
use super::Type;

#[test]
fn iter_vars_test_1() {
    assert_eq!(
        Type::mk_app(Type::Var(0), Type::Var(1))
            .iter_vars()
            .collect::<Vec<usize>>(),
        vec![0, 1]
    )
}

#[test]
fn iter_vars_test_2() {
    assert_eq!(
        Type::mk_app(
            Type::mk_app(Type::Var(0), Type::Var(1)),
            Type::mk_app(Type::Var(2), Type::Var(3))
        )
        .iter_vars()
        .collect::<Vec<usize>>(),
        vec![0, 1, 2, 3]
    )
}

#[test]
fn iter_vars_test_3() {
    assert_eq!(
        Type::mk_app(
            Type::mk_app(Type::Var(0), Type::Var(1)),
            Type::mk_app(Type::Name(String::from("hi")), Type::Var(3))
        )
        .iter_vars()
        .collect::<Vec<usize>>(),
        vec![0, 1, 3]
    )
}
