use crate::core::{Builtin, Declaration, Expr, Module, TypeSig};
use crate::syntax::{Kind, Type};
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub static ref BUILTINS: Module = Module {
        module_mapping: HashMap::new(),
        decls: vec![
            // mapIO : (a -> b) -> IO a -> IO b
            Declaration::Definition {
                name: String::from("mapIO"),
                sig: TypeSig {
                    ty_vars: vec![
                        // a : Type
                        (String::from("a"), Kind::Type),
                        // b : Type
                        (String::from("b"), Kind::Type)
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
                        (String::from("a"), Kind::Type),
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
                        (String::from("a"), Kind::Type),
                        // b : Type
                        (String::from("b"), Kind::Type),
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
                        (String::from("a"), Kind::Type),
                        // b : Type
                        (String::from("b"), Kind::Type),
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

            // flushStdout : Stdout -> IO ()
            Declaration::Definition {
                name: String::from("flushStdout"),
                sig: TypeSig {
                    ty_vars: vec![
                    ],
                    body: Type::mk_arrow(
                            Type::Name(String::from("Stdout")),
                            Type::mk_app(Type::IO, Type::Unit)
                    )
                },
                body: Expr::Builtin(Builtin::FlushStdout)
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

            // eqString : String -> String -> Bool
            Declaration::Definition{
                name: String::from("eqString"),
                sig: TypeSig{
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(Type::String, Type::mk_arrow(Type::String, Type::Bool))
                },
                body: Expr::Builtin(Builtin::EqString)
            },

            // add : Int -> Int -> Int
            Declaration::Definition{
                name: String::from("add"),
                sig: TypeSig{
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(Type::Int, Type::mk_arrow(Type::Int, Type::Int))
                },
                body: Expr::Builtin(Builtin::Add)
            },

            // subtract : Int -> Int -> Int
            Declaration::Definition{
                name: String::from("subtract"),
                sig: TypeSig{
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(Type::Int, Type::mk_arrow(Type::Int, Type::Int))
                },
                body: Expr::Builtin(Builtin::Subtract)
            },

            // multiply : Int -> Int -> Int
            Declaration::Definition{
                name: String::from("multiply"),
                sig: TypeSig{
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(Type::Int, Type::mk_arrow(Type::Int, Type::Int))
                },
                body: Expr::Builtin(Builtin::Multiply)
            },

            // eqInt : Int -> Int -> Bool
            Declaration::Definition{
                name: String::from("eqInt"),
                sig: TypeSig{
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(Type::Int, Type::mk_arrow(Type::Int, Type::Bool))
                },
                body: Expr::Builtin(Builtin::EqInt)
            },

            // ltInt : Int -> Int -> Bool
            Declaration::Definition{
                name: String::from("ltInt"),
                sig: TypeSig{
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(Type::Int, Type::mk_arrow(Type::Int, Type::Bool))
                },
                body: Expr::Builtin(Builtin::LtInt)
            },

            // showInt : Int -> String
            Declaration::Definition{
                name: String::from("showInt"),
                sig: TypeSig{
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(Type::Int, Type::String)
                },
                body: Expr::Builtin(Builtin::ShowInt)
            },

            // eqArray : (a -> a -> Bool) -> Array a -> Array a -> Bool
            Declaration::Definition{
                name: String::from("eqArray"),
                sig: TypeSig{
                    ty_vars: vec![(String::from("a"), Kind::Type)],
                    body: Type::mk_arrow(
                        Type::mk_arrow(Type::Var(0), Type::mk_arrow(Type::Var(0), Type::Bool)),
                        Type::mk_arrow(
                            Type::mk_app(Type::Array, Type::Var(0)),
                            Type::mk_arrow(
                                Type::mk_app(Type::Array, Type::Var(0)),
                                Type::Bool
                            )
                        )
                    )
                },
                body: Expr::Builtin(Builtin::EqArray)
            },

            // ltArray : (a -> a -> Bool) -> Array a -> Array a -> Bool
            Declaration::Definition{
                name: String::from("ltArray"),
                sig: TypeSig{
                    ty_vars: vec![(String::from("a"), Kind::Type)],
                    body: Type::mk_arrow(
                        Type::mk_arrow(Type::Var(0), Type::mk_arrow(Type::Var(0), Type::Bool)),
                        Type::mk_arrow(
                            Type::mk_app(Type::Array, Type::Var(0)),
                            Type::mk_arrow(
                                Type::mk_app(Type::Array, Type::Var(0)),
                                Type::Bool
                            )
                        )
                    )
                },
                body: Expr::Builtin(Builtin::LtArray)
            },

            // foldlArray : (b -> a -> b) -> b -> Array a -> b
            Declaration::Definition{
                name: String::from("foldlArray"),
                sig: TypeSig{
                    ty_vars: vec![(String::from("b"), Kind::Type), (String::from("a"), Kind::Type)],
                    body: Type::mk_arrow(
                        Type::mk_arrow(Type::Var(1), Type::mk_arrow(Type::Var(0), Type::Var(1))),
                        Type::mk_arrow(
                            Type::Var(1),
                            Type::mk_arrow(
                                Type::mk_app(Type::Array, Type::Var(0)),
                                Type::Var(1)
                            )
                        )
                    )
                },
                body: Expr::Builtin(Builtin::FoldlArray)
            },

            // generateArray : Int -> (Int -> a) -> Array a
            Declaration::Definition{
                name: String::from("generateArray"),
                sig: TypeSig{
                    ty_vars: vec![(String::from("a"), Kind::Type)],
                    body: Type::mk_arrow(
                        Type::Int,
                        Type::mk_arrow(
                            Type::mk_arrow(
                                Type::Int,
                                Type::Var(0)
                            ),
                            Type::mk_app(Type::Array, Type::Var(0))
                        )
                    )
                },
                body: Expr::Builtin(Builtin::GenerateArray)
            },

            // lengthArray : Array a -> Int
            Declaration::Definition{
                name: String::from("lengthArray"),
                sig: TypeSig{
                    ty_vars: vec![(String::from("a"), Kind::Type)],
                    body: Type::mk_arrow(
                        Type::mk_app(Type::Array, Type::Var(0)),
                        Type::Int
                    )
                },
                body: Expr::Builtin(Builtin::LengthArray)
            },

            // indexArray : Int -> Array a -> a
            Declaration::Definition{
                name: String::from("indexArray"),
                sig: TypeSig{
                    ty_vars: vec![(String::from("a"), Kind::Type)],
                    body: Type::mk_arrow(
                        Type::Int,
                        Type::mk_arrow(
                            Type::mk_app(Type::Array, Type::Var(0)),
                            Type::Var(0)
                        )
                    )
                },
                body: Expr::Builtin(Builtin::IndexArray)
            },

            // sliceArray : Int -> Int -> Array a -> Array a
            Declaration::Definition{
                name: String::from("sliceArray"),
                sig: TypeSig{
                    ty_vars: vec![(String::from("a"), Kind::Type)],
                    body: Type::mk_arrow(
                        Type::Int,
                        Type::mk_arrow(
                            Type::Int,
                            Type::mk_arrow(
                                Type::mk_app(Type::Array, Type::Var(0)),
                                Type::mk_app(Type::Array, Type::Var(0))
                            )
                        )
                    )
                },
                body: Expr::Builtin(Builtin::SliceArray)
            },

            // filterString : (Char -> Bool) -> String -> String
            Declaration::Definition{
                name: String::from("filterString"),
                sig: TypeSig{
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(
                        Type::mk_arrow(Type::Char, Type::Bool),
                        Type::mk_arrow(Type::String, Type::String)
                    )
                },
                body: Expr::Builtin(Builtin::FilterString)
            },

            // eqChar : Char -> Char -> Bool
            Declaration::Definition{
                name: String::from("eqChar"),
                sig: TypeSig{
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(
                        Type::Char,
                        Type::mk_arrow(Type::Char, Type::Bool)
                    )
                },
                body: Expr::Builtin(Builtin::EqChar)
            },

            // splitString : Char -> String -> Array String
            Declaration::Definition{
                name: String::from("splitString"),
                sig: TypeSig{
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(
                        Type::Char,
                        Type::mk_arrow(Type::String, Type::mk_app(Type::Array, Type::String))
                    )
                },
                body: Expr::Builtin(Builtin::SplitString)
            },

            // foldlString : (a -> Char -> a) -> a -> String -> a
            Declaration::Definition{
                name: String::from("foldlString"),
                sig: TypeSig{
                    ty_vars: vec![(String::from("a"), Kind::Type)],
                    body: Type::mk_arrow(
                        Type::mk_arrow(Type::Var(0), Type::mk_arrow(Type::Char, Type::Var(0))),
                        Type::mk_arrow(
                            Type::Var(0),
                            Type::mk_arrow(
                                Type::String,
                                Type::Var(0)
                            )
                        )
                    )
                },
                body: Expr::Builtin(Builtin::FoldlString)
            },

            // snocArray : Array a -> a -> Array a
            Declaration::Definition{
                name: String::from("snocArray"),
                sig: TypeSig {
                    ty_vars: vec![(String::from("a"), Kind::Type)],
                    body: Type::mk_arrow(
                        Type::mk_app(Type::Array, Type::Var(0)),
                        Type::mk_arrow(
                            Type::Var(0),
                            Type::mk_app(Type::Array, Type::Var(0))
                        )
                    )
                },
                body: Expr::Builtin(Builtin::SnocArray)
            }
        ]
    };
}
