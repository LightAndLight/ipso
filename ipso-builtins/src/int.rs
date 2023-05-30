use ipso_core::{Builtin, CommonKinds, Declaration, Expr, Type, TypeSig};
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    let option_int = Type::mk_variant(
        common_kinds,
        vec![("Some".into(), Type::Int), ("None".into(), Type::Unit)],
        None,
    );

    vec![
        // eq : Int -> Int -> Bool
        Rc::new(Declaration::Definition {
            name: Rc::from("eq"),
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
            name: Rc::from("toString"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(common_kinds, Type::Int, Type::String),
            },
            body: Expr::alloc_builtin(Builtin::IntToString),
        }),
        // mod : Int -> Int -> Bool
        Rc::new(Declaration::Definition {
            name: Rc::from("mod"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(
                    common_kinds,
                    Type::Int,
                    Type::arrow(common_kinds, Type::Int, Type::Int),
                ),
            },
            body: Expr::alloc_builtin(Builtin::IntMod),
        }),
        // parseBin : String -> (| Some : Int, None : () |)
        Rc::new(Declaration::Definition {
            name: Rc::from("parseBin"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(common_kinds, Type::String, option_int.clone()),
            },
            body: Expr::alloc_builtin(Builtin::ParseBin),
        }),
        // parseOct : String -> (| Some : Int, None : () |)
        Rc::new(Declaration::Definition {
            name: Rc::from("parseOct"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(common_kinds, Type::String, option_int.clone()),
            },
            body: Expr::alloc_builtin(Builtin::ParseOct),
        }),
        // parseDec : String -> (| Some : Int, None : () |)
        Rc::new(Declaration::Definition {
            name: Rc::from("parseDec"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(common_kinds, Type::String, option_int.clone()),
            },
            body: Expr::alloc_builtin(Builtin::ParseDec),
        }),
        // parseHex : String -> (| Some : Int, None : () |)
        Rc::new(Declaration::Definition {
            name: Rc::from("parseHex"),
            sig: TypeSig {
                ty_vars: Vec::new(),
                body: Type::arrow(common_kinds, Type::String, option_int),
            },
            body: Expr::alloc_builtin(Builtin::ParseHex),
        }),
    ]
}
