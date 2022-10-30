use ipso_core::{Builtin, CommonKinds, Declaration, Expr, Type, TypeSig};
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    vec![
        // exists : String -> IO Bool
        Rc::new(Declaration::Definition {
            name: Rc::from("exists"),
            sig: {
                TypeSig {
                    ty_vars: vec![],
                    body: Type::arrow(
                        common_kinds,
                        Type::String,
                        Type::app(Type::mk_io(common_kinds), Type::Bool),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::PathExists),
        }),
    ]
}
