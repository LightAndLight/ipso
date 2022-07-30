use ipso_core::{Builtin, CommonKinds, Declaration, Expr, Type, TypeSig};
use ipso_syntax::kind::Kind;
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    vec![
        // success : IO a
        Rc::new(Declaration::Definition {
            name: String::from("success"),
            sig: {
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), Kind::Type)],
                    body: Type::app(Type::mk_io(common_kinds), Type::Var(Kind::Type, 0)),
                }
            },
            body: Expr::alloc_builtin(Builtin::ExitSuccess),
        }),
        // failure : IO a
        Rc::new(Declaration::Definition {
            name: String::from("failure"),
            sig: {
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), Kind::Type)],
                    body: Type::app(Type::mk_io(common_kinds), Type::Var(Kind::Type, 0)),
                }
            },
            body: Expr::alloc_builtin(Builtin::ExitFailure),
        }),
        // with : Int -> IO a
        Rc::new(Declaration::Definition {
            name: String::from("with"),
            sig: {
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), Kind::Type)],
                    body: Type::arrow(
                        common_kinds,
                        Type::Int,
                        Type::app(Type::mk_io(common_kinds), Type::Var(Kind::Type, 0)),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::ExitWith),
        }),
    ]
}
