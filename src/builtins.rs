use crate::core::{Declaration, Expr, Module, TypeSig};
use crate::syntax::{Kind, Type};
use lazy_static::lazy_static;

lazy_static! {
    pub static ref BUILTINS: Module = Module {
        decls: vec![
            // mapIO : (a -> b) -> IO a -> IO b
            Declaration::Definition {
                name: String::from("mapIO"),
                sig: TypeSig {
                    ty_vars: vec![
                        // a : Type
                        Kind::Type,
                        // b : Type
                        Kind::Type
                    ],
                    body: Type::mk_arrow(
                        Type::mk_arrow(
                            Type::Var(1),
                            Type::Var(0)
                        ),
                        Type::mk_arrow(
                            Type::mk_app(Type::IO, Type::Var(1)),
                            Type::mk_app(Type::IO, Type::Var(0))
                        )
                    )
                },
                body: Expr::Name(String::from("mapIO_impl"))
            },
            // pureIO : a -> IO a
            Declaration::Definition {
                name: String::from("pureIO"),
                sig: TypeSig {
                    ty_vars: vec![
                        // a : Type
                        Kind::Type,
                    ],
                    body: Type::mk_arrow(
                            Type::Var(0),
                            Type::mk_app(Type::IO, Type::Var(0))
                    )
                },
                body: Expr::Name(String::from("pureIO_impl"))
            },
        ]
    };
}