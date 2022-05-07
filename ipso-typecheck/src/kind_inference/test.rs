use super::{infer, Env, Error, ErrorHint, State};
use crate::kind_inference::unify_with_hint;
use crate::BoundVars;
use ipso_core::{self as core, CommonKinds};
use ipso_syntax::{self as syntax, kind::Kind};
use std::collections::HashMap;
use std::rc::Rc;

fn with_empty_env_and_ctx<A>(f: &dyn Fn(Env, &mut State) -> A) -> A {
    let common_kinds = CommonKinds::default();
    let types = HashMap::new();
    let type_variables = BoundVars::new();
    let mut ctx = State::new();
    f(
        Env {
            common_kinds: &common_kinds,
            types: &types,
            type_variables: &type_variables,
        },
        &mut ctx,
    )
}

#[test]
fn infer_1() {
    with_empty_env_and_ctx(&|env, ctx| {
        let expected = Ok((core::Type::Bool, Kind::Type));
        let actual = infer(env, ctx, &syntax::Type::Bool);
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_2() {
    with_empty_env_and_ctx(&|env, ctx| {
        let expected = Ok((core::Type::RowNil, Kind::Row));
        let actual = infer(env, ctx, &syntax::Type::RowNil);
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_3() {
    with_empty_env_and_ctx(&|env, ctx| {
        let ty =
            syntax::Type::mk_rowcons(Rc::from("x"), syntax::Type::RowNil, syntax::Type::RowNil);
        let expected = Err(Error::mismatch(&Kind::Type, &Kind::Row)
            .with_hint(ErrorHint::WhileInferring { ty: ty.clone() }));
        let actual = infer(env, ctx, &ty);
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_4() {
    with_empty_env_and_ctx(&|env, ctx| {
        let expected = Ok((
            core::Type::app(
                core::Type::mk_record_ctor(env.common_kinds),
                core::Type::mk_rowcons(Rc::from("x"), core::Type::Bool, core::Type::RowNil),
            ),
            Kind::Type,
        ));
        let actual = infer(
            env,
            ctx,
            &syntax::Type::mk_app(
                syntax::Type::Record,
                syntax::Type::mk_rowcons(Rc::from("x"), syntax::Type::Bool, syntax::Type::RowNil),
            ),
        )
        .map(|(ty, kind)| (ty, ctx.kind_solutions.zonk(false, kind)));
        assert_eq!(expected, actual)
    })
}

#[test]
fn occurs_1() {
    with_empty_env_and_ctx(&|_env, ctx| {
        let v1 = ctx.fresh_meta();
        let v2 = ctx.fresh_meta();
        let kind = Kind::mk_arrow(&v1, &v2);
        let hint = ErrorHint::WhileChecking {
            ty: syntax::Type::Unit,
            has_kind: Kind::Type,
        };
        let expected = Err(Error::occurs(
            match v1 {
                Kind::Meta(m) => m,
                _ => unreachable!(),
            },
            &kind,
        )
        .with_hint(hint.clone()));
        assert_eq!(expected, unify_with_hint(ctx, &|| hint.clone(), &v1, &kind),)
    })
}
