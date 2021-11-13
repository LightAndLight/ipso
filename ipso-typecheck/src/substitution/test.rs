#[cfg(test)]
use crate::{substitution::Substitution, Typechecker, UnifyTypeContextRefs};
#[cfg(test)]
use ipso_core as core;
#[cfg(test)]
use ipso_syntax::kind::Kind;
#[cfg(test)]
use std::rc::Rc;

#[test]
fn subst_left_1() {
    crate::current_dir_with_tc!(|mut tc: Typechecker| {
        tc.bound_tyvars.insert(&[(Rc::from("r"), Kind::Row)]);
        tc.type_solutions = vec![
            (Kind::Type, None),
            (
                Kind::Type,
                Some(core::Type::mk_app(
                    tc.common_kinds,
                    core::Type::Record,
                    core::Type::Var(Kind::Row, 0),
                )),
            ),
        ];
        let mut subst = Substitution::new();
        let context = UnifyTypeContextRefs {
            expected: &core::Type::Meta(Kind::Type, 0),
            actual: &core::Type::Int,
        };
        subst
            .subst_left(&mut tc, &context, 0, core::Type::Int)
            .unwrap();
        tc.commit_substitutions(subst);
        let expected = vec![
            (Kind::Type, Some(core::Type::Int)),
            (
                Kind::Type,
                Some(core::Type::mk_app(
                    tc.common_kinds,
                    core::Type::Record,
                    core::Type::Var(Kind::Row, 0),
                )),
            ),
        ];
        let actual = tc.type_solutions;
        assert_eq!(expected, actual)
    })
}
