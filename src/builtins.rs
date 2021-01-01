use crate::core::{Builtin, Declaration, Expr, Module, TypeSig};
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
                body: Expr::Builtin(Builtin::MapIO)
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
                body: Expr::Builtin(Builtin::PureIO)
            },

            // bindIO : IO a -> (a -> IO b) -> IO b
            Declaration::Definition {
                name: String::from("bindIO"),
                sig: TypeSig {
                    ty_vars: vec![
                        // a : Type
                        Kind::Type,
                        // b : Type
                        Kind::Type,
                    ],
                    body: Type::mk_arrow(
                            Type::mk_app(Type::IO, Type::Var(1)),
                            Type::mk_arrow(
                                Type::mk_arrow(Type::Var(1), Type::mk_app(Type::IO, Type::Var(0))),
                                Type::mk_app(Type::IO, Type::Var(0))
                            )
                    )
                },
                body: Expr::Builtin(Builtin::BindIO)
            },

            // trace : a -> b -> b
            Declaration::Definition {
                name: String::from("trace"),
                sig: TypeSig {
                    ty_vars: vec![
                        // a : Type
                        Kind::Type,
                        // b : Type
                        Kind::Type,
                    ],
                    body: Type::mk_arrow(
                            Type::Var(1),
                            Type::mk_arrow(
                                Type::Var(0),
                                Type::Var(0)
                            )
                    )
                },
                body: Expr::Builtin(Builtin::Trace)
            },

            // toUtf8 : String -> Bytes
            Declaration::Definition {
                name: String::from("toUtf8"),
                sig: TypeSig {
                    ty_vars: vec![
                    ],
                    body: Type::mk_arrow(
                            Type::String,
                            Type::Bytes
                    )
                },
                body: Expr::Builtin(Builtin::ToUtf8)
            },

            // Stdout : Type
            Declaration::BuiltinType {
                name: String::from("Stdout"),
                kind: Kind::Type
            },

            // stdout : Stdout
            Declaration::Definition {
                name: String::from("stdout"),
                sig: TypeSig {
                    ty_vars: vec![
                    ],
                    body:
                            Type::Name(String::from("Stdout"))
                },
                body: Expr::Builtin(Builtin::Stdout)
            },

            // writeStdout : Stdout -> Bytes -> IO ()
            Declaration::Definition {
                name: String::from("writeStdout"),
                sig: TypeSig {
                    ty_vars: vec![
                    ],
                    body: Type::mk_arrow(
                            Type::Name(String::from("Stdout")),
                            Type::mk_arrow(
                                Type::Bytes,
                                Type::mk_app(Type::IO, Type::Unit)
                            )
                    )
                },
                body: Expr::Builtin(Builtin::WriteStdout)
            },

            // Stdin : Type
            Declaration::BuiltinType {
                name: String::from("Stdin"),
                kind: Kind::Type
            },

            // stdin : Stdin
            Declaration::Definition {
                name: String::from("stdin"),
                sig: TypeSig {
                    ty_vars: vec![
                    ],
                    body:
                            Type::Name(String::from("Stdin"))
                },
                body: Expr::Builtin(Builtin::Stdin)
            },

            // readLineStdin : Stdin -> IO String
            Declaration::Definition {
                name: String::from("readLineStdin"),
                sig: TypeSig {
                    ty_vars: vec![
                    ],
                    body: Type::mk_arrow(
                            Type::Name(String::from("Stdin")),
                            Type::mk_app(Type::IO, Type::String)
                    )
                },
                body: Expr::Builtin(Builtin::ReadLineStdin)
            },
        ]
    };
}
