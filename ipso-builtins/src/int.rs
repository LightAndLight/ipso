use ipso_core::{Builtin, CommonKinds, Declaration, Expr, Type, TypeSig};
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    vec![
        // eq : Int -> Int -> Bool
        Rc::new(Declaration::Definition {
            name: String::from("eq"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(
                    common_kinds,
                    Type::Int,
                    Type::arrow(common_kinds, Type::Int, Type::Bool),
                ),
            },
            body: Expr::alloc_builtin(Builtin::EqInt),
        }),
        // toString : Int -> String
        Rc::new(Declaration::Definition {
            name: String::from("toString"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(common_kinds, Type::Int, Type::String),
            },
            body: Expr::alloc_builtin(Builtin::IntToString),
        }),
    ]
}
