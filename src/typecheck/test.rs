use crate::syntax::Type;
use crate::typecheck::Context;
use crate::typecheck::ContextEntry;

use super::{Kind, TypeError, Typechecker};

#[test]
fn infer_kind_test_1() {
    let mut tc = Typechecker::new();
    let expected = Ok(Kind::Type);
    let actual = tc.infer_kind(&Type::Bool);
    assert_eq!(expected, actual)
}

#[test]
fn infer_kind_test_2() {
    let mut tc = Typechecker::new();
    let expected = Ok(Kind::Row);
    let actual = tc.infer_kind(&Type::RowNil);
    assert_eq!(expected, actual)
}

#[test]
fn infer_kind_test_3() {
    let mut tc = Typechecker::new();
    let expected = Err(TypeError::KindMismatch {
        pos: 0,
        expected: Kind::Type,
        actual: Kind::Row,
    });
    let actual = tc.infer_kind(&Type::mk_rowcons("x", Type::RowNil, Type::RowNil));
    assert_eq!(expected, actual)
}

#[test]
fn infer_kind_test_4() {
    let mut tc = Typechecker::new();
    let expected = Ok(Some(Kind::Type));
    let actual = tc
        .infer_kind(&Type::mk_app(
            Type::Record,
            Type::mk_rowcons("x", Type::Bool, Type::RowNil),
        ))
        .map(|kind| tc.zonk_kind(kind));
    assert_eq!(expected, actual)
}

#[test]
fn context_test_1() {
    assert_eq!(
        {
            let mut ctx = Context::new();
            ctx.insert(vec![
                (String::from("a"), Type::Unit),
                (String::from("b"), Type::Bool),
                (String::from("c"), Type::String),
            ]);
            ctx
        },
        Context(
            vec![
                (
                    String::from("a"),
                    vec![ContextEntry {
                        index: 2,
                        ty: Type::Unit
                    }]
                ),
                (
                    String::from("b"),
                    vec![ContextEntry {
                        index: 1,
                        ty: Type::Bool
                    }]
                ),
                (
                    String::from("c"),
                    vec![ContextEntry {
                        index: 0,
                        ty: Type::String
                    }]
                )
            ]
            .into_iter()
            .collect()
        )
    )
}

#[test]
#[should_panic]
fn context_test_2() {
    let mut ctx = Context::new();
    ctx.insert(vec![
        (String::from("a"), Type::Unit),
        (String::from("a"), Type::Bool),
        (String::from("c"), Type::String),
    ]);
}
