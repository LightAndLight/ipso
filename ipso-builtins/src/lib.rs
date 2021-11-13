use ipso_core::{Builtin, Declaration, Expr, Module, Type, TypeSig};
use ipso_syntax::kind::Kind;
use std::collections::HashMap;
use std::rc::Rc;

pub fn builtins() -> Module {
    let stdout_ty = Type::unsafe_mk_name(Rc::from("Stdout"), Kind::Type);
    let stdin_ty = Type::unsafe_mk_name(Rc::from("Stdin"), Kind::Type);
    let string_ty = Type::String;
    let bytes_ty = Type::Bytes;
    let io_ty = Type::IO;
    let unit_ty = Type::Unit;
    let bool_ty = Type::Bool;
    let int_ty = Type::Int;
    let char_ty = Type::Char;
    let array_ty = Type::Array;

    Module {
        module_mapping: HashMap::new(),
        decls: vec![
            // mapIO : (a -> b) -> IO a -> IO b
            Declaration::Definition {
                name: String::from("mapIO"),
                sig: {
                    let a = Type::unsafe_mk_var(1, Kind::Type);
                    let b = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![
                            // a : Type
                            (Rc::from("a"), a.get_kind()),
                            // b : Type
                            (Rc::from("b"), b.get_kind()),
                        ],
                        body: Type::mk_arrow(
                            Type::mk_arrow(a.clone(), b.clone()),
                            Type::mk_arrow(
                                Type::mk_app(io_ty.clone(), a),
                                Type::mk_app(io_ty.clone(), b),
                            ),
                        ),
                    }
                },
                body: Expr::alloc_builtin(Builtin::MapIO),
            },
            // pureIO : a -> IO a
            Declaration::Definition {
                name: String::from("pureIO"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![
                            // a : Type
                            (Rc::from("a"), a.get_kind()),
                        ],
                        body: Type::mk_arrow(a.clone(), Type::mk_app(io_ty.clone(), a)),
                    }
                },
                body: Expr::alloc_builtin(Builtin::PureIO),
            },
            // bindIO : IO a -> (a -> IO b) -> IO b
            Declaration::Definition {
                name: String::from("bindIO"),
                sig: {
                    let a = Type::unsafe_mk_var(1, Kind::Type);
                    let b = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![
                            // a : Type
                            (Rc::from("a"), a.get_kind()),
                            // b : Type
                            (Rc::from("b"), a.get_kind()),
                        ],
                        body: Type::mk_arrow(
                            Type::mk_app(io_ty.clone(), a.clone()),
                            Type::mk_arrow(
                                Type::mk_arrow(a, Type::mk_app(io_ty.clone(), b.clone())),
                                Type::mk_app(io_ty.clone(), b),
                            ),
                        ),
                    }
                },
                body: Expr::alloc_builtin(Builtin::BindIO),
            },
            // trace : a -> b -> b
            Declaration::Definition {
                name: String::from("trace"),
                sig: {
                    let a = Type::unsafe_mk_var(1, Kind::Type);
                    let b = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![
                            // a : Type
                            (Rc::from("a"), a.get_kind()),
                            // b : Type
                            (Rc::from("b"), a.get_kind()),
                        ],
                        body: Type::mk_arrow(a, Type::mk_arrow(b.clone(), b)),
                    }
                },
                body: Expr::alloc_builtin(Builtin::Trace),
            },
            // toUtf8 : String -> Bytes
            Declaration::Definition {
                name: String::from("toUtf8"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: Type::mk_arrow(string_ty.clone(), bytes_ty.clone()),
                },
                body: Expr::alloc_builtin(Builtin::ToUtf8),
            },
            // Stdout : Type
            Declaration::BuiltinType {
                name: String::from("Stdout"),
                kind: Kind::Type,
            },
            // stdout : Stdout
            Declaration::Definition {
                name: String::from("stdout"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: stdout_ty.clone(),
                },
                body: Expr::alloc_builtin(Builtin::Stdout),
            },
            // writeStdout : Stdout -> Bytes -> IO ()
            Declaration::Definition {
                name: String::from("writeStdout"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: Type::mk_arrow(
                        stdout_ty.clone(),
                        Type::mk_arrow(bytes_ty, Type::mk_app(io_ty.clone(), unit_ty.clone())),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::WriteStdout),
            },
            // flushStdout : Stdout -> IO ()
            Declaration::Definition {
                name: String::from("flushStdout"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: Type::mk_arrow(stdout_ty, Type::mk_app(io_ty.clone(), unit_ty)),
                },
                body: Expr::alloc_builtin(Builtin::FlushStdout),
            },
            // Stdin : Type
            Declaration::BuiltinType {
                name: String::from("Stdin"),
                kind: Kind::Type,
            },
            // stdin : Stdin
            Declaration::Definition {
                name: String::from("stdin"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: stdin_ty.clone(),
                },
                body: Expr::alloc_builtin(Builtin::Stdin),
            },
            // readLineStdin : Stdin -> IO String
            Declaration::Definition {
                name: String::from("readLineStdin"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: Type::mk_arrow(stdin_ty, Type::mk_app(io_ty, string_ty.clone())),
                },
                body: Expr::alloc_builtin(Builtin::ReadLineStdin),
            },
            // eqString : String -> String -> Bool
            Declaration::Definition {
                name: String::from("eqString"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(
                        string_ty.clone(),
                        Type::mk_arrow(string_ty.clone(), bool_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::EqString),
            },
            // add : Int -> Int -> Int
            Declaration::Definition {
                name: String::from("add"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(
                        int_ty.clone(),
                        Type::mk_arrow(int_ty.clone(), int_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::Add),
            },
            // subtract : Int -> Int -> Int
            Declaration::Definition {
                name: String::from("subtract"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(
                        int_ty.clone(),
                        Type::mk_arrow(int_ty.clone(), int_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::Subtract),
            },
            // multiply : Int -> Int -> Int
            Declaration::Definition {
                name: String::from("multiply"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(
                        int_ty.clone(),
                        Type::mk_arrow(int_ty.clone(), int_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::Multiply),
            },
            // eqInt : Int -> Int -> Bool
            Declaration::Definition {
                name: String::from("eqInt"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(
                        int_ty.clone(),
                        Type::mk_arrow(int_ty.clone(), bool_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::EqInt),
            },
            // ltInt : Int -> Int -> Bool
            Declaration::Definition {
                name: String::from("ltInt"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(
                        int_ty.clone(),
                        Type::mk_arrow(int_ty.clone(), bool_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::LtInt),
            },
            // showInt : Int -> String
            Declaration::Definition {
                name: String::from("showInt"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(int_ty.clone(), string_ty.clone()),
                },
                body: Expr::alloc_builtin(Builtin::ShowInt),
            },
            // eqArray : (a -> a -> Bool) -> Array a -> Array a -> Bool
            Declaration::Definition {
                name: String::from("eqArray"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![(Rc::from("a"), a.get_kind())],
                        body: Type::mk_arrow(
                            Type::mk_arrow(a.clone(), Type::mk_arrow(a.clone(), bool_ty.clone())),
                            Type::mk_arrow(
                                Type::mk_app(array_ty.clone(), a.clone()),
                                Type::mk_arrow(Type::mk_app(array_ty.clone(), a), bool_ty.clone()),
                            ),
                        ),
                    }
                },
                body: Expr::alloc_builtin(Builtin::EqArray),
            },
            // ltArray : (a -> a -> Bool) -> Array a -> Array a -> Bool
            Declaration::Definition {
                name: String::from("ltArray"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![(Rc::from("a"), a.get_kind())],
                        body: Type::mk_arrow(
                            Type::mk_arrow(a.clone(), Type::mk_arrow(a.clone(), bool_ty.clone())),
                            Type::mk_arrow(
                                Type::mk_app(array_ty.clone(), a.clone()),
                                Type::mk_arrow(Type::mk_app(array_ty.clone(), a), bool_ty.clone()),
                            ),
                        ),
                    }
                },
                body: Expr::alloc_builtin(Builtin::LtArray),
            },
            // foldlArray : (b -> a -> b) -> b -> Array a -> b
            Declaration::Definition {
                name: String::from("foldlArray"),
                sig: {
                    let b = Type::unsafe_mk_var(1, Kind::Type);
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![(Rc::from("b"), b.get_kind()), (Rc::from("a"), a.get_kind())],
                        body: Type::mk_arrow(
                            Type::mk_arrow(b.clone(), Type::mk_arrow(a.clone(), b.clone())),
                            Type::mk_arrow(
                                b.clone(),
                                Type::mk_arrow(Type::mk_app(array_ty.clone(), a), b),
                            ),
                        ),
                    }
                },
                body: Expr::alloc_builtin(Builtin::FoldlArray),
            },
            // generateArray : Int -> (Int -> a) -> Array a
            Declaration::Definition {
                name: String::from("generateArray"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![(Rc::from("a"), a.get_kind())],
                        body: Type::mk_arrow(
                            int_ty.clone(),
                            Type::mk_arrow(
                                Type::mk_arrow(int_ty.clone(), a.clone()),
                                Type::mk_app(array_ty.clone(), a),
                            ),
                        ),
                    }
                },
                body: Expr::alloc_builtin(Builtin::GenerateArray),
            },
            // lengthArray : Array a -> Int
            Declaration::Definition {
                name: String::from("lengthArray"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![(Rc::from("a"), a.get_kind())],
                        body: Type::mk_arrow(Type::mk_app(array_ty.clone(), a), int_ty.clone()),
                    }
                },
                body: Expr::alloc_builtin(Builtin::LengthArray),
            },
            // indexArray : Int -> Array a -> a
            Declaration::Definition {
                name: String::from("indexArray"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![(Rc::from("a"), a.get_kind())],
                        body: Type::mk_arrow(
                            int_ty.clone(),
                            Type::mk_arrow(Type::mk_app(array_ty.clone(), a.clone()), a),
                        ),
                    }
                },
                body: Expr::alloc_builtin(Builtin::IndexArray),
            },
            // sliceArray : Int -> Int -> Array a -> Array a
            Declaration::Definition {
                name: String::from("sliceArray"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![(Rc::from("a"), a.get_kind())],
                        body: Type::mk_arrow(
                            int_ty.clone(),
                            Type::mk_arrow(
                                int_ty,
                                Type::mk_arrow(
                                    Type::mk_app(array_ty.clone(), a.clone()),
                                    Type::mk_app(array_ty.clone(), a),
                                ),
                            ),
                        ),
                    }
                },
                body: Expr::alloc_builtin(Builtin::SliceArray),
            },
            // filterString : (Char -> Bool) -> String -> String
            Declaration::Definition {
                name: String::from("filterString"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(
                        Type::mk_arrow(char_ty.clone(), bool_ty.clone()),
                        Type::mk_arrow(string_ty.clone(), string_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::FilterString),
            },
            // eqChar : Char -> Char -> Bool
            Declaration::Definition {
                name: String::from("eqChar"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(char_ty.clone(), Type::mk_arrow(char_ty.clone(), bool_ty)),
                },
                body: Expr::alloc_builtin(Builtin::EqChar),
            },
            // splitString : Char -> String -> Array String
            Declaration::Definition {
                name: String::from("splitString"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::mk_arrow(
                        char_ty.clone(),
                        Type::mk_arrow(
                            string_ty.clone(),
                            Type::mk_app(array_ty.clone(), string_ty.clone()),
                        ),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::SplitString),
            },
            // foldlString : (a -> Char -> a) -> a -> String -> a
            Declaration::Definition {
                name: String::from("foldlString"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![(Rc::from("a"), a.get_kind())],
                        body: Type::mk_arrow(
                            Type::mk_arrow(a.clone(), Type::mk_arrow(char_ty, a.clone())),
                            Type::mk_arrow(a.clone(), Type::mk_arrow(string_ty, a)),
                        ),
                    }
                },
                body: Expr::alloc_builtin(Builtin::FoldlString),
            },
            // snocArray : Array a -> a -> Array a
            Declaration::Definition {
                name: String::from("snocArray"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![(Rc::from("a"), Kind::Type)],
                        body: Type::mk_arrow(
                            Type::mk_app(array_ty.clone(), a.clone()),
                            Type::mk_arrow(a.clone(), Type::mk_app(array_ty, a)),
                        ),
                    }
                },
                body: Expr::alloc_builtin(Builtin::SnocArray),
            },
        ],
    }
}
