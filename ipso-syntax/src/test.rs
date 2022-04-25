use crate::{is_keyword, Keyword, Type, KEYWORDS};
use quickcheck_macros::quickcheck;
use std::rc::Rc;

#[quickcheck]
fn prop_all_keywords_in_list(keyword: Keyword) {
    let keyword_string = keyword.to_string();
    assert!(
        is_keyword(keyword_string),
        "KEYWORDS is missing {:?}",
        keyword_string
    );
    assert_eq!(keyword, Keyword::from_string(keyword_string).unwrap())
}

#[test]
fn keywords_list_valid() {
    for keyword in KEYWORDS {
        assert_eq!(*keyword, Keyword::from_string(keyword).unwrap().to_string())
    }
}

#[test]
fn iter_vars_test_1() {
    assert_eq!(
        Type::mk_app(Type::Var(0), Type::Var(1))
            .iter_vars()
            .collect::<Vec<&usize>>(),
        vec![&0, &1]
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
        .collect::<Vec<&usize>>(),
        vec![&0, &1, &2, &3]
    )
}

#[test]
fn iter_vars_test_3() {
    assert_eq!(
        Type::mk_app(
            Type::mk_app(Type::Var(0), Type::Var(1)),
            Type::mk_app(Type::Name(Rc::from("hi")), Type::Var(3))
        )
        .iter_vars()
        .collect::<Vec<&usize>>(),
        vec![&0, &1, &3]
    )
}

#[test]
fn unwrap_fatarrow_1() {
    let expected = Some((&Type::Var(0), &Type::Var(1)));
    let ty = Type::mk_fatarrow(Type::Var(0), Type::Var(1));
    let actual = ty.unwrap_fatarrow();
    assert_eq!(expected, actual)
}
