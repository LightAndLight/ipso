use ipso_core::{Builtin, CommonKinds, Declaration, Expr, Type, TypeSig};
use ipso_syntax::kind::Kind;
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    vec![
        // toUtf8 : String -> Bytes
        Rc::new(Declaration::Definition {
            name: String::from("toUtf8"),
            sig: TypeSig {
                ty_vars: vec![],
                body: Type::arrow(common_kinds, Type::String, Type::Bytes),
            },
            body: Expr::alloc_builtin(Builtin::ToUtf8),
        }),
        // eq : String -> String -> Bool
        Rc::new(Declaration::Definition {
            name: String::from("eq"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(
                    common_kinds,
                    Type::String,
                    Type::arrow(common_kinds, Type::String, Type::Bool),
                ),
            },
            body: Expr::alloc_builtin(Builtin::EqString),
        }),
        // filter : (Char -> Bool) -> String -> String
        Rc::new(Declaration::Definition {
            name: String::from("filter"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(
                    common_kinds,
                    Type::arrow(common_kinds, Type::Char, Type::Bool),
                    Type::arrow(common_kinds, Type::String, Type::String),
                ),
            },
            body: Expr::alloc_builtin(Builtin::FilterString),
        }),
        // split : Char -> String -> Array String
        Rc::new(Declaration::Definition {
            name: String::from("split"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(
                    common_kinds,
                    Type::Char,
                    Type::arrow(
                        common_kinds,
                        Type::String,
                        Type::app(Type::mk_array(common_kinds), Type::String),
                    ),
                ),
            },
            body: Expr::alloc_builtin(Builtin::SplitString),
        }),
        // foldl : (a -> Char -> a) -> a -> String -> a
        Rc::new(Declaration::Definition {
            name: String::from("foldl"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), a.kind())],
                    body: Type::arrow(
                        common_kinds,
                        Type::arrow(
                            common_kinds,
                            a.clone(),
                            Type::arrow(common_kinds, Type::Char, a.clone()),
                        ),
                        Type::arrow(
                            common_kinds,
                            a.clone(),
                            Type::arrow(common_kinds, Type::String, a),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::FoldlString),
        }),
    ]
}
