use ipso_core::{Builtin, CommonKinds, Declaration, Expr, Type, TypeSig};
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    vec![
        // read : String -> IO String
        Rc::new(Declaration::Definition {
            name: Rc::from("read"),
            sig: {
                TypeSig {
                    ty_vars: vec![],
                    body: Type::arrow(
                        common_kinds,
                        Type::String,
                        Type::app(Type::mk_io(common_kinds), Type::String),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::FileRead),
        }),
        // write : String -> String -> IO ()
        Rc::new(Declaration::Definition {
            name: Rc::from("write"),
            sig: {
                TypeSig {
                    ty_vars: vec![],
                    body: Type::arrow(
                        common_kinds,
                        Type::String,
                        Type::arrow(
                            common_kinds,
                            Type::String,
                            Type::app(Type::mk_io(common_kinds), Type::Unit),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::FileWrite),
        }),
        // append : String -> String -> IO ()
        Rc::new(Declaration::Definition {
            name: Rc::from("append"),
            sig: {
                TypeSig {
                    ty_vars: vec![],
                    body: Type::arrow(
                        common_kinds,
                        Type::String,
                        Type::arrow(
                            common_kinds,
                            Type::String,
                            Type::app(Type::mk_io(common_kinds), Type::Unit),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::FileAppend),
        }),
    ]
}
