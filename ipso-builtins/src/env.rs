use ipso_core::{Builtin, CommonKinds, Declaration, Expr, Type, TypeSig};
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    vec![
        // args : IO (Array String)
        Rc::new(Declaration::Definition {
            name: Rc::from("args"),
            sig: {
                TypeSig {
                    ty_vars: vec![],
                    body: Type::app(
                        Type::mk_io(common_kinds),
                        Type::app(Type::mk_array(common_kinds), Type::String),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::EnvArgs),
        }),
        // getvar : String -> IO (| Some : String, None : () |)
        Rc::new(Declaration::Definition {
            name: Rc::from("getvar"),
            sig: {
                TypeSig {
                    ty_vars: vec![],
                    body: Type::arrow(
                        common_kinds,
                        Type::String,
                        Type::app(
                            Type::mk_io(common_kinds),
                            Type::mk_variant(
                                common_kinds,
                                vec![
                                    (Rc::from("Some"), Type::String),
                                    (Rc::from("None"), Type::Unit),
                                ],
                                None,
                            ),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::EnvGetvar),
        }),
        // setvar : String -> String -> IO ()
        Rc::new(Declaration::Definition {
            name: Rc::from("setvar"),
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
            body: Expr::alloc_builtin(Builtin::EnvSetvar),
        }),
    ]
}
