#[cfg(test)]
use super::{infer, InferenceContext, InferenceError, InferenceErrorHint, Solutions};
#[cfg(test)]
use crate::BoundVars;
#[cfg(test)]
use ipso_core::{self as core, CommonKinds};
#[cfg(test)]
use ipso_syntax::{self as syntax, kind::Kind};
#[cfg(test)]
use std::collections::HashMap;
use std::rc::Rc;

fn with_empty_ctx<A>(f: &dyn Fn(&mut InferenceContext) -> A) -> A {
    let common_kinds = CommonKinds::default();
    let types = HashMap::new();
    let type_variables = BoundVars::new();
    let mut kind_solutions = Solutions::new();
    let mut ctx = InferenceContext {
        common_kinds: &common_kinds,
        types: &types,
        type_variables: &type_variables,
        kind_solutions: &mut kind_solutions,
    };
    f(&mut ctx)
}

#[test]
fn infer_1() {
    with_empty_ctx(&|ctx| {
        let expected = Ok((core::Type::Bool, Kind::Type));
        let actual = infer(ctx, &syntax::Type::Bool);
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_2() {
    with_empty_ctx(&|ctx| {
        let expected = Ok((core::Type::RowNil, Kind::Row));
        let actual = infer(ctx, &syntax::Type::RowNil);
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_3() {
    with_empty_ctx(&|ctx| {
        let ty =
            syntax::Type::mk_rowcons(Rc::from("x"), syntax::Type::RowNil, syntax::Type::RowNil);
        let expected = Err(InferenceError::mismatch(&Kind::Type, &Kind::Row)
            .with_hint(InferenceErrorHint::WhileInferring { ty: ty.clone() }));
        let actual = infer(ctx, &ty);
        assert_eq!(expected, actual)
    })
}

#[test]
fn infer_4() {
    with_empty_ctx(&|ctx| {
        let expected = Ok((
            core::Type::app(
                core::Type::mk_record_ctor(ctx.common_kinds),
                core::Type::mk_rowcons(Rc::from("x"), core::Type::Bool, core::Type::RowNil),
            ),
            Kind::Type,
        ));
        let actual = infer(
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
    with_empty_ctx(&|ctx| {
        let v1 = ctx.fresh_meta();
        let v2 = ctx.fresh_meta();
        let kind = Kind::mk_arrow(&v1, &v2);
        let hint = InferenceErrorHint::WhileChecking {
            ty: syntax::Type::Unit,
            has_kind: Kind::Type,
        };
        let expected = Err(InferenceError::occurs(
            match v1 {
                Kind::Meta(m) => m,
                _ => unreachable!(),
            },
            &kind,
        )
        .with_hint(hint.clone()));
        assert_eq!(expected, ctx.unify(&|| hint.clone(), &v1, &kind),)
    })
}
