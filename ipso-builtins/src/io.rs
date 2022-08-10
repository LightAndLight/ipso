use ipso_core::{Builtin, CommonKinds, Declaration, Expr, Type, TypeSig};
use ipso_syntax::kind::Kind;
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    vec![
        // map : (a -> b) -> IO a -> IO b
        Rc::new(Declaration::Definition {
            name: Rc::from("map"),
            sig: {
                let a = Type::unsafe_mk_var(1, Kind::Type);
                let b = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![
                        // a : Type
                        (Rc::from("a"), a.kind()),
                        // b : Type
                        (Rc::from("b"), b.kind()),
                    ],
                    body: Type::arrow(
                        common_kinds,
                        Type::arrow(common_kinds, a.clone(), b.clone()),
                        Type::arrow(
                            common_kinds,
                            Type::app(Type::mk_io(common_kinds), a),
                            Type::app(Type::mk_io(common_kinds), b),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::MapIO),
        }),
        // pure : a -> IO a
        Rc::new(Declaration::Definition {
            name: Rc::from("pure"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![
                        // a : Type
                        (Rc::from("a"), a.kind()),
                    ],
                    body: Type::arrow(
                        common_kinds,
                        a.clone(),
                        Type::app(Type::mk_io(common_kinds), a),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::Pure),
        }),
        // andThen : IO a -> (a -> IO b) -> IO b
        Rc::new(Declaration::Definition {
            name: Rc::from("andThen"),
            sig: {
                let a = Type::unsafe_mk_var(1, Kind::Type);
                let b = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![
                        // a : Type
                        (Rc::from("a"), a.kind()),
                        // b : Type
                        (Rc::from("b"), a.kind()),
                    ],
                    body: Type::arrow(
                        common_kinds,
                        Type::app(Type::mk_io(common_kinds), a.clone()),
                        Type::arrow(
                            common_kinds,
                            Type::arrow(
                                common_kinds,
                                a,
                                Type::app(Type::mk_io(common_kinds), b.clone()),
                            ),
                            Type::app(Type::mk_io(common_kinds), b),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::BindIO),
        }),
    ]
}
