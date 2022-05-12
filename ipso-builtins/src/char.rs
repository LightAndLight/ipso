use ipso_core::{Builtin, CommonKinds, Declaration, Expr, Type, TypeSig};
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    vec![
        // eq : Char -> Char -> Bool
        Rc::new(Declaration::Definition {
            name: String::from("eq"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(
                    common_kinds,
                    Type::Char,
                    Type::arrow(common_kinds, Type::Char, Type::Bool),
                ),
            },
            body: Expr::alloc_builtin(Builtin::EqChar),
        }),
        // toString : Char -> String
        Rc::new(Declaration::Definition {
            name: String::from("toString"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(common_kinds, Type::Char, Type::String),
            },
            body: Expr::alloc_builtin(Builtin::CharToString),
        }),
    ]
}
