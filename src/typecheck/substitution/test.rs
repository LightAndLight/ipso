#[cfg(test)]
use crate::{
    syntax::{Kind, Type},
    typecheck::{substitution::Substitution, Typechecker, UnifyTypeContext},
};

#[test]
fn subst_left_1() {
    crate::current_dir_with_tc!(|mut tc: Typechecker| {
        tc.bound_tyvars
            .insert(&vec![(String::from("r"), Kind::Row)]);
        tc.type_solutions = vec![
            (Kind::Type, None),
            (Kind::Type, Some(Type::mk_app(Type::Record, Type::Var(0)))),
        ];
        let mut subst = Substitution::new();
        let context = UnifyTypeContext {
            expected: Type::Meta(0),
            actual: Type::Int,
        };
        subst.subst_left(&mut tc, &context, 0, Type::Int).unwrap();
        tc.commit_substitutions(subst);
        let expected = vec![
            (Kind::Type, Some(Type::Int)),
            (Kind::Type, Some(Type::mk_app(Type::Record, Type::Var(0)))),
        ];
        let actual = tc.type_solutions;
        assert_eq!(expected, actual)
    })
}