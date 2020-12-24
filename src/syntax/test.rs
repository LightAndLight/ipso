#[cfg(test)]
use super::Type;

#[test]
fn iter_vars_test_1() {
    assert_eq!(
        Type::mk_app(Type::Var(0), Type::Var(0))
            .iter_vars()
            .collect(),
        vec![0, 1, 2, 3]
    )
}
