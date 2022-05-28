use super::{infer, Env, Error, ErrorHint, State};
use crate::kind_inference::unification;
use crate::BoundVars;
use ipso_core::{self as core, CommonKinds};
use ipso_syntax::{self as syntax, kind::Kind};
use std::collections::HashMap;
use std::rc::Rc;

fn with_empty_env_and_state<A>(f: &dyn Fn(Env, &mut State) -> A) -> A {
    let common_kinds = CommonKinds::default();
    let types = HashMap::new();
    let type_variables = BoundVars::new();
    let mut state = State::new();
    f(
        Env {
            common_kinds: &common_kinds,
            types: &types,
            type_variables: &type_variables,
        },
        &mut state,
    )
}

#[test]
fn infer_1() {
    with_empty_env_and_state(&|env, state| {
        let expected = Ok((core::Type::Bool, Kind::Type));
        let actual = {
            let ty = syntax::Type::Bool;
            infer(env, state, 0, &ty)
                .map_err(|error| error.with_hint(ErrorHint::WhileInferring { ty: ty.clone() }))
        };
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_2() {
    with_empty_env_and_state(&|env, state| {
        let expected = Ok((core::Type::RowNil, Kind::Row));
        let actual = {
            let ty = syntax::Type::RowNil;
            infer(env, state, 0, &ty)
                .map_err(|error| error.with_hint(ErrorHint::WhileInferring { ty: ty.clone() }))
        };
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_3() {
    with_empty_env_and_state(&|env, state| {
        let pos = 99;
        let ty =
            syntax::Type::mk_rowcons(Rc::from("x"), syntax::Type::RowNil, syntax::Type::RowNil);
        let expected = Err(Error::mismatch(pos, &Kind::Type, &Kind::Row)
            .with_hint(ErrorHint::WhileInferring { ty: ty.clone() }));
        let actual = {
            let ty: syntax::Type<Rc<str>> = ty;
            infer(env, state, pos, &ty)
                .map_err(|error| error.with_hint(ErrorHint::WhileInferring { ty: ty.clone() }))
        };
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_4() {
    with_empty_env_and_state(&|env, state| {
        let expected = Ok((
            core::Type::app(
                core::Type::mk_record_ctor(env.common_kinds),
                core::Type::mk_rowcons(Rc::from("x"), core::Type::Bool, core::Type::RowNil),
            ),
            Kind::Type,
        ));
        let actual = {
            let ty: syntax::Type<Rc<str>> = syntax::Type::mk_app(
                syntax::Type::Record,
                syntax::Type::mk_rowcons(Rc::from("x"), syntax::Type::Bool, syntax::Type::RowNil),
            );
            infer(env, state, 0, &ty)
                .map_err(|error| error.with_hint(ErrorHint::WhileInferring { ty: ty.clone() }))
        }
        .map(|(ty, kind)| (ty, state.kind_solutions.zonk(false, kind)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn occurs_1() {
    with_empty_env_and_state(&|_env, state| {
        let v1 = state.fresh_meta();
        let v2 = state.fresh_meta();
        let kind = Kind::mk_arrow(&v1, &v2);
        let expected = Err(unification::Error::occurs(
            match v1 {
                Kind::Meta(m) => m,
                _ => unreachable!(),
            },
            &kind,
        ));
        assert_eq!(
            expected,
            unification::unify(&mut state.kind_solutions, &v1, &kind),
        )
    })
}
