use ipso_core::{Builtin, CommonKinds, Declaration, Expr, Type, TypeSig};
use ipso_syntax::kind::Kind;
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    vec![
        // toUtf8 : String -> Bytes
        Rc::new(Declaration::Definition {
            name: Rc::from("toUtf8"),
            sig: TypeSig {
                ty_vars: vec![],
                body: Type::arrow(common_kinds, Type::String, Type::Bytes),
            },
            body: Expr::alloc_builtin(Builtin::ToUtf8),
        }),
        // eq : String -> String -> Bool
        Rc::new(Declaration::Definition {
            name: Rc::from("eq"),
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
            name: Rc::from("filter"),
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
        // split : String -> String -> Array String
        Rc::new(Declaration::Definition {
            name: Rc::from("split"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(
                    common_kinds,
                    Type::String,
                    Type::arrow(
                        common_kinds,
                        Type::String,
                        Type::app(Type::mk_array(common_kinds), Type::String),
                    ),
                ),
            },
            body: Expr::alloc_builtin(Builtin::StringSplit),
        }),
        // splitc : Char -> String -> Array String
        Rc::new(Declaration::Definition {
            name: Rc::from("splitc"),
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
            body: Expr::alloc_builtin(Builtin::StringSplitc),
        }),
        // join : String -> Array String -> String
        Rc::new(Declaration::Definition {
            name: Rc::from("join"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(
                    common_kinds,
                    Type::String,
                    Type::arrow(
                        common_kinds,
                        Type::app(Type::mk_array(common_kinds), Type::String),
                        Type::String,
                    ),
                ),
            },
            body: Expr::alloc_builtin(Builtin::JoinString),
        }),
        // foldl : (a -> Char -> a) -> a -> String -> a
        Rc::new(Declaration::Definition {
            name: Rc::from("foldl"),
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
        // parts : String -> String -> Array String
        Rc::new(Declaration::Definition {
            name: Rc::from("parts"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(
                    common_kinds,
                    Type::String,
                    Type::arrow(
                        common_kinds,
                        Type::String,
                        Type::app(Type::mk_array(common_kinds), Type::String),
                    ),
                ),
            },
            body: Expr::alloc_builtin(Builtin::StringParts),
        }),
        // partsc : Char -> String -> Array String
        Rc::new(Declaration::Definition {
            name: Rc::from("partsc"),
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
            body: Expr::alloc_builtin(Builtin::StringPartsc),
        }),
        // trimp : (Char -> Bool) -> String -> String
        Rc::new(Declaration::Definition {
            name: Rc::from("trimp"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(
                    common_kinds,
                    Type::arrow(common_kinds, Type::Char, Type::Bool),
                    Type::arrow(common_kinds, Type::String, Type::String),
                ),
            },
            body: Expr::alloc_builtin(Builtin::StringTrimp),
        }),
        // trimc : Char -> String -> String
        Rc::new(Declaration::Definition {
            name: Rc::from("trimc"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(
                    common_kinds,
                    Type::Char,
                    Type::arrow(common_kinds, Type::String, Type::String),
                ),
            },
            body: Expr::alloc_builtin(Builtin::StringTrimc),
        }),
        // trim : String -> String
        Rc::new(Declaration::Definition {
            name: Rc::from("trim"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(common_kinds, Type::String, Type::String),
            },
            body: Expr::alloc_builtin(Builtin::StringTrim),
        }),
    ]
}
