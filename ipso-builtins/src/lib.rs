use ipso_core::{
    Branch, Builtin, ClassDeclaration, ClassMember, CommonKinds, Declaration, Expr, Module, Name,
    Pattern, Type, TypeSig,
};
use ipso_syntax::{kind::Kind, ModuleId};
use std::rc::Rc;

pub fn builtins(common_kinds: &CommonKinds) -> Module {
    let string_ty = Type::String;
    let bytes_ty = Type::Bytes;
    let io_ty = Type::mk_io(common_kinds);
    let unit_ty = Type::Unit;
    let bool_ty = Type::Bool;
    let int_ty = Type::Int;
    let char_ty = Type::Char;
    let array_ty = Type::mk_array(common_kinds);

    Module {
        decls: vec![
            // trace : a -> b -> b
            Declaration::Definition {
                name: String::from("trace"),
                sig: {
                    let a = Type::unsafe_mk_var(1, Kind::Type);
                    let b = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![
                            // a : Type
                            (Rc::from("a"), a.kind()),
                            // b : Type
                            (Rc::from("b"), a.kind()),
                        ],
                        body: Type::arrow(common_kinds, a, Type::arrow(common_kinds, b.clone(), b)),
                    }
                },
                body: Expr::alloc_builtin(Builtin::Trace),
            },
            Declaration::Module {
                name: String::from("io"),
                decls: vec![
                    // map : (a -> b) -> IO a -> IO b
                    Rc::new(Declaration::Definition {
                        name: String::from("map"),
                        sig: {
                            let a = Type::unsafe_mk_var(1, Kind::Type);
                            let b = Type::unsafe_mk_var(0, Kind::Type);
                            TypeSig {
                                ty_vars: vec![
                                    // a : Type
                                    (Rc::from("a"), a.kind()),
                                    // b : Type
                                    (Rc::from("b"), b.kind()),
                                ],
                                body: Type::arrow(
                                    common_kinds,
                                    Type::arrow(common_kinds, a.clone(), b.clone()),
                                    Type::arrow(
                                        common_kinds,
                                        Type::app(io_ty.clone(), a),
                                        Type::app(io_ty.clone(), b),
                                    ),
                                ),
                            }
                        },
                        body: Expr::alloc_builtin(Builtin::MapIO),
                    }),
                    // pure : a -> IO a
                    Rc::new(Declaration::Definition {
                        name: String::from("pure"),
                        sig: {
                            let a = Type::unsafe_mk_var(0, Kind::Type);
                            TypeSig {
                                ty_vars: vec![
                                    // a : Type
                                    (Rc::from("a"), a.kind()),
                                ],
                                body: Type::arrow(
                                    common_kinds,
                                    a.clone(),
                                    Type::app(io_ty.clone(), a),
                                ),
                            }
                        },
                        body: Expr::alloc_builtin(Builtin::Pure),
                    }),
                    // andThen : IO a -> (a -> IO b) -> IO b
                    Rc::new(Declaration::Definition {
                        name: String::from("andThen"),
                        sig: {
                            let a = Type::unsafe_mk_var(1, Kind::Type);
                            let b = Type::unsafe_mk_var(0, Kind::Type);
                            TypeSig {
                                ty_vars: vec![
                                    // a : Type
                                    (Rc::from("a"), a.kind()),
                                    // b : Type
                                    (Rc::from("b"), a.kind()),
                                ],
                                body: Type::arrow(
                                    common_kinds,
                                    Type::app(io_ty.clone(), a.clone()),
                                    Type::arrow(
                                        common_kinds,
                                        Type::arrow(
                                            common_kinds,
                                            a,
                                            Type::app(io_ty.clone(), b.clone()),
                                        ),
                                        Type::app(io_ty.clone(), b),
                                    ),
                                ),
                            }
                        },
                        body: Expr::alloc_builtin(Builtin::BindIO),
                    }),
                ],
            },
            // println : String -> IO ()
            Declaration::Definition {
                name: String::from("println"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: Type::arrow(
                        common_kinds,
                        string_ty.clone(),
                        Type::app(io_ty.clone(), unit_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::Println),
            },
            // print : String -> IO ()
            Declaration::Definition {
                name: String::from("print"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: Type::arrow(
                        common_kinds,
                        string_ty.clone(),
                        Type::app(io_ty.clone(), unit_ty),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::Print),
            },
            // readln : IO String
            Declaration::Definition {
                name: String::from("readln"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: Type::app(io_ty.clone(), string_ty.clone()),
                },
                body: Expr::alloc_builtin(Builtin::Readln),
            },
            Declaration::Module {
                name: String::from("string"),
                decls: vec![
                    // toUtf8 : String -> Bytes
                    Rc::new(Declaration::Definition {
                        name: String::from("toUtf8"),
                        sig: TypeSig {
                            ty_vars: vec![],
                            body: Type::arrow(common_kinds, string_ty.clone(), bytes_ty),
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
                                string_ty.clone(),
                                Type::arrow(common_kinds, string_ty.clone(), bool_ty.clone()),
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
                                Type::arrow(common_kinds, char_ty.clone(), bool_ty.clone()),
                                Type::arrow(common_kinds, string_ty.clone(), string_ty.clone()),
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
                                char_ty.clone(),
                                Type::arrow(
                                    common_kinds,
                                    string_ty.clone(),
                                    Type::app(array_ty.clone(), string_ty.clone()),
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
                                        Type::arrow(common_kinds, char_ty.clone(), a.clone()),
                                    ),
                                    Type::arrow(
                                        common_kinds,
                                        a.clone(),
                                        Type::arrow(common_kinds, string_ty.clone(), a),
                                    ),
                                ),
                            }
                        },
                        body: Expr::alloc_builtin(Builtin::FoldlString),
                    }),
                ],
            },
            Declaration::Module {
                name: String::from("int"),
                decls: vec![
                    // eq : Int -> Int -> Bool
                    Rc::new(Declaration::Definition {
                        name: String::from("eq"),
                        sig: TypeSig {
                            ty_vars: Vec::new(),
                            body: Type::arrow(
                                common_kinds,
                                int_ty.clone(),
                                Type::arrow(common_kinds, int_ty.clone(), bool_ty.clone()),
                            ),
                        },
                        body: Expr::alloc_builtin(Builtin::EqInt),
                    }),
                    // show : Int -> String
                    Rc::new(Declaration::Definition {
                        name: String::from("show"),
                        sig: TypeSig {
                            ty_vars: Vec::new(),
                            body: Type::arrow(common_kinds, int_ty.clone(), string_ty.clone()),
                        },
                        body: Expr::alloc_builtin(Builtin::ShowInt),
                    }),
                ],
            },
            Declaration::Module {
                name: String::from("array"),
                decls: vec![
                    // eq : (a -> a -> Bool) -> Array a -> Array a -> Bool
                    Rc::new(Declaration::Definition {
                        name: String::from("eq"),
                        sig: {
                            let a = Type::unsafe_mk_var(0, Kind::Type);
                            TypeSig {
                                ty_vars: vec![(Rc::from("a"), a.kind())],
                                body: Type::arrow(
                                    common_kinds,
                                    Type::arrow(
                                        common_kinds,
                                        a.clone(),
                                        Type::arrow(common_kinds, a.clone(), bool_ty.clone()),
                                    ),
                                    Type::arrow(
                                        common_kinds,
                                        Type::app(array_ty.clone(), a.clone()),
                                        Type::arrow(
                                            common_kinds,
                                            Type::app(array_ty.clone(), a),
                                            bool_ty.clone(),
                                        ),
                                    ),
                                ),
                            }
                        },
                        body: Expr::alloc_builtin(Builtin::EqArray),
                    }),
                    // foldl : (b -> a -> b) -> b -> Array a -> b
                    Rc::new(Declaration::Definition {
                        name: String::from("foldl"),
                        sig: {
                            let b = Type::unsafe_mk_var(1, Kind::Type);
                            let a = Type::unsafe_mk_var(0, Kind::Type);
                            TypeSig {
                                ty_vars: vec![(Rc::from("b"), b.kind()), (Rc::from("a"), a.kind())],
                                body: Type::arrow(
                                    common_kinds,
                                    Type::arrow(
                                        common_kinds,
                                        b.clone(),
                                        Type::arrow(common_kinds, a.clone(), b.clone()),
                                    ),
                                    Type::arrow(
                                        common_kinds,
                                        b.clone(),
                                        Type::arrow(
                                            common_kinds,
                                            Type::app(array_ty.clone(), a),
                                            b,
                                        ),
                                    ),
                                ),
                            }
                        },
                        body: Expr::alloc_builtin(Builtin::FoldlArray),
                    }),
                    // generate : Int -> (Int -> a) -> Array a
                    Rc::new(Declaration::Definition {
                        name: String::from("generate"),
                        sig: {
                            let a = Type::unsafe_mk_var(0, Kind::Type);
                            TypeSig {
                                ty_vars: vec![(Rc::from("a"), a.kind())],
                                body: Type::arrow(
                                    common_kinds,
                                    int_ty.clone(),
                                    Type::arrow(
                                        common_kinds,
                                        Type::arrow(common_kinds, int_ty.clone(), a.clone()),
                                        Type::app(array_ty.clone(), a),
                                    ),
                                ),
                            }
                        },
                        body: Expr::alloc_builtin(Builtin::GenerateArray),
                    }),
                    // length : Array a -> Int
                    Rc::new(Declaration::Definition {
                        name: String::from("length"),
                        sig: {
                            let a = Type::unsafe_mk_var(0, Kind::Type);
                            TypeSig {
                                ty_vars: vec![(Rc::from("a"), a.kind())],
                                body: Type::arrow(
                                    common_kinds,
                                    Type::app(array_ty.clone(), a),
                                    int_ty.clone(),
                                ),
                            }
                        },
                        body: Expr::alloc_builtin(Builtin::LengthArray),
                    }),
                    // index : Int -> Array a -> a
                    Rc::new(Declaration::Definition {
                        name: String::from("index"),
                        sig: {
                            let a = Type::unsafe_mk_var(0, Kind::Type);
                            TypeSig {
                                ty_vars: vec![(Rc::from("a"), a.kind())],
                                body: Type::arrow(
                                    common_kinds,
                                    int_ty.clone(),
                                    Type::arrow(
                                        common_kinds,
                                        Type::app(array_ty.clone(), a.clone()),
                                        a,
                                    ),
                                ),
                            }
                        },
                        body: Expr::alloc_builtin(Builtin::IndexArray),
                    }),
                    // slice : Int -> Int -> Array a -> Array a
                    Rc::new(Declaration::Definition {
                        name: String::from("slice"),
                        sig: {
                            let a = Type::unsafe_mk_var(0, Kind::Type);
                            TypeSig {
                                ty_vars: vec![(Rc::from("a"), a.kind())],
                                body: Type::arrow(
                                    common_kinds,
                                    int_ty.clone(),
                                    Type::arrow(
                                        common_kinds,
                                        int_ty,
                                        Type::arrow(
                                            common_kinds,
                                            Type::app(array_ty.clone(), a.clone()),
                                            Type::app(array_ty.clone(), a),
                                        ),
                                    ),
                                ),
                            }
                        },
                        body: Expr::alloc_builtin(Builtin::SliceArray),
                    }),
                    // snoc : Array a -> a -> Array a
                    Rc::new(Declaration::Definition {
                        name: String::from("snoc"),
                        sig: {
                            let a = Type::unsafe_mk_var(0, Kind::Type);
                            TypeSig {
                                ty_vars: vec![(Rc::from("a"), Kind::Type)],
                                body: Type::arrow(
                                    common_kinds,
                                    Type::app(array_ty.clone(), a.clone()),
                                    Type::arrow(
                                        common_kinds,
                                        a.clone(),
                                        Type::app(array_ty.clone(), a),
                                    ),
                                ),
                            }
                        },
                        body: Expr::alloc_builtin(Builtin::SnocArray),
                    }),
                    // flatMap : (a -> Array b) -> Array a -> Array b
                    Rc::new(Declaration::Definition {
                        name: String::from("flatMap"),
                        sig: TypeSig::new(
                            vec![(Rc::from("a"), Kind::Type), (Rc::from("b"), Kind::Type)],
                            Type::arrow(
                                common_kinds,
                                Type::arrow(
                                    common_kinds,
                                    Type::Var(Kind::Type, 1),
                                    Type::app(array_ty.clone(), Type::Var(Kind::Type, 0)),
                                ),
                                Type::arrow(
                                    common_kinds,
                                    Type::app(array_ty.clone(), Type::Var(Kind::Type, 1)),
                                    Type::app(array_ty.clone(), Type::Var(Kind::Type, 0)),
                                ),
                            ),
                        ),
                        body: Rc::new(Expr::Builtin(Builtin::FlatMap)),
                    }),
                ],
            },
            Declaration::Module {
                name: String::from("char"),
                decls: vec![
                    // eq : Char -> Char -> Bool
                    Rc::new(Declaration::Definition {
                        name: String::from("eq"),
                        sig: TypeSig {
                            ty_vars: Vec::new(),
                            body: Type::arrow(
                                common_kinds,
                                char_ty.clone(),
                                Type::arrow(common_kinds, char_ty, bool_ty.clone()),
                            ),
                        },
                        body: Expr::alloc_builtin(Builtin::EqChar),
                    }),
                ],
            },
            Declaration::Module {
                name: String::from("cmd"),
                decls: vec![
                    // run : Cmd -> IO ()
                    Rc::new(Declaration::Definition {
                        name: String::from("run"),
                        sig: {
                            TypeSig {
                                ty_vars: Vec::new(),
                                body: Type::arrow(
                                    common_kinds,
                                    Type::Cmd,
                                    Type::app(io_ty.clone(), Type::Unit),
                                ),
                            }
                        },
                        body: Expr::alloc_builtin(Builtin::Run),
                    }),
                    // lines : Cmd -> IO (Array String)
                    Rc::new(Declaration::Definition {
                        name: String::from("lines"),
                        sig: TypeSig::new(
                            vec![],
                            Type::arrow(
                                common_kinds,
                                Type::Cmd,
                                Type::app(io_ty, Type::app(array_ty.clone(), string_ty)),
                            ),
                        ),
                        body: Rc::new(Expr::Builtin(Builtin::Lines)),
                    }),
                    // show : Cmd -> String
                    Rc::new(Declaration::Definition {
                        name: String::from("show"),
                        sig: TypeSig::new(
                            vec![],
                            Type::arrow(common_kinds, Type::Cmd, Type::String),
                        ),
                        body: Rc::new(Expr::Builtin(Builtin::ShowCmd)),
                    }),
                ],
            },
            /*
            class Eq a where
              eq : a -> a -> Bool
             */
            Declaration::Class(ClassDeclaration {
                supers: vec![],
                name: Rc::from("Eq"),
                args: vec![(Rc::from("a"), Kind::Type)],
                members: vec![ClassMember {
                    name: String::from("eq"),
                    sig: TypeSig {
                        ty_vars: vec![],
                        body: Type::arrow(
                            common_kinds,
                            Type::Var(Kind::Type, 0),
                            Type::arrow(common_kinds, Type::Var(Kind::Type, 0), bool_ty),
                        ),
                    },
                }],
            }),
            /*
            instance Eq Int where
              eq = eqInt
             */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Eq"),
                    ),
                    Type::Int,
                ),
                evidence: Rc::from("Eq Int"),
            },
            Declaration::Evidence {
                name: Rc::from("Eq Int"),
                body: Rc::new(Expr::mk_record(
                    vec![(Expr::Int(0), Expr::Builtin(Builtin::EqInt))],
                    None,
                )),
            },
            /*
            instance Eq Char where
              eq = eqChar
             */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Eq"),
                    ),
                    Type::Char,
                ),
                evidence: Rc::from("Eq Char"),
            },
            Declaration::Evidence {
                name: Rc::from("Eq Char"),
                body: Rc::new(Expr::mk_record(
                    vec![(Expr::Int(0), Expr::Builtin(Builtin::EqChar))],
                    None,
                )),
            },
            /*
            instance Eq String where
              eq = eqString
             */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Eq"),
                    ),
                    Type::String,
                ),
                evidence: Rc::from("Eq String"),
            },
            Declaration::Evidence {
                name: Rc::from("Eq String"),
                body: Rc::new(Expr::mk_record(
                    vec![(Expr::Int(0), Expr::Builtin(Builtin::EqString))],
                    None,
                )),
            },
            /*
            instance Eq Bool where
              eq = eqBool
             */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Eq"),
                    ),
                    Type::Bool,
                ),
                evidence: Rc::from("Eq Bool"),
            },
            Declaration::Evidence {
                name: Rc::from("Eq Bool"),
                body: Rc::new(Expr::mk_record(
                    vec![(Expr::Int(0), Expr::Builtin(Builtin::EqBool))],
                    None,
                )),
            },
            /*
            instance Eq a => Eq (Array a) where
              eq = eqArray eq
             */
            Declaration::Instance {
                ty_vars: vec![(Rc::from("a"), Kind::Type)],
                // Eq a
                assumes: vec![Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Eq"),
                    ),
                    Type::Var(Kind::Type, 0),
                )],
                // Eq (Array a)
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Eq"),
                    ),
                    Type::app(array_ty.clone(), Type::Var(Kind::Type, 0)),
                ),
                evidence: Rc::from("Eq a => Eq (Array a)"),
            },
            Declaration::Evidence {
                name: Rc::from("Eq a => Eq (Array a)"),
                // \eqDict -> { eq = builtins.eqArray (eq eqDict) }
                body: Rc::new(Expr::mk_lam(
                    true,
                    Expr::mk_record(
                        vec![(
                            Expr::Int(0),
                            Expr::mk_app(
                                Expr::Builtin(Builtin::EqArray),
                                Expr::mk_app(Expr::Name(Name::definition("eq")), Expr::Var(0)),
                            ),
                        )],
                        None,
                    ),
                )),
            },
            /*
            class Eq a => Ord a where
              compare : a -> a -> (| Less : (), Equal : (), Greater : () |)
             */
            Declaration::Class(ClassDeclaration {
                supers: vec![Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Eq"),
                    ),
                    Type::Name(Kind::Type, Rc::from("a")),
                )],
                name: Rc::from("Ord"),
                args: vec![(Rc::from("a"), Kind::Type)],
                members: vec![ClassMember {
                    name: String::from("compare"),
                    sig: TypeSig {
                        ty_vars: vec![],
                        body: Type::arrow(
                            common_kinds,
                            Type::Var(Kind::Type, 0),
                            Type::arrow(
                                common_kinds,
                                Type::Var(Kind::Type, 0),
                                Type::mk_variant(
                                    common_kinds,
                                    vec![
                                        (Rc::from("Less"), Type::Unit),
                                        (Rc::from("Equal"), Type::Unit),
                                        (Rc::from("Greater"), Type::Unit),
                                    ],
                                    None,
                                ),
                            ),
                        ),
                    },
                }],
            }),
            /*
            instance Ord Bool where
              compare a b =
                if a
                then if b then EQ () else GT ()
                else if b then LT () else EQ ()
             */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Ord"),
                    ),
                    Type::Bool,
                ),
                evidence: Rc::from("Ord Bool"),
            },
            Declaration::Evidence {
                name: Rc::from("Ord Bool"),
                body: Rc::new(Expr::mk_record(
                    vec![
                        (
                            Expr::Int(0),
                            // dict : Eq Bool
                            Expr::mk_record(
                                vec![(Expr::Int(0), Expr::Builtin(Builtin::EqBool))],
                                None,
                            ),
                        ),
                        (
                            Expr::Int(1),
                            Expr::mk_lam(
                                true,
                                Expr::mk_lam(
                                    true,
                                    Expr::mk_ifthenelse(
                                        Expr::Var(1),
                                        Expr::mk_ifthenelse(
                                            Expr::Var(0),
                                            // EQ () : (| EQ : (), GT : (), LT : () |)
                                            Expr::mk_app(
                                                Expr::mk_variant(Expr::Int(0)),
                                                Expr::Unit,
                                            ),
                                            // GT () : (| EQ : (), GT : (), LT : () |)
                                            Expr::mk_app(
                                                Expr::mk_variant(Expr::Int(1)),
                                                Expr::Unit,
                                            ),
                                        ),
                                        Expr::mk_ifthenelse(
                                            Expr::Var(0),
                                            // LT () : (| EQ : (), GT : (), LT : () |)
                                            Expr::mk_app(
                                                Expr::mk_variant(Expr::Int(2)),
                                                Expr::Unit,
                                            ),
                                            // EQ () : (| EQ : (), GT : (), LT : () |)
                                            Expr::mk_app(
                                                Expr::mk_variant(Expr::Int(0)),
                                                Expr::Unit,
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ],
                    None,
                )),
            },
            /*
            instance Ord Char where
              compare = compareChar
             */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Ord"),
                    ),
                    Type::Char,
                ),
                evidence: Rc::from("Ord Char"),
            },
            Declaration::Evidence {
                name: Rc::from("Ord Char"),
                body: Rc::new(Expr::mk_record(
                    vec![
                        (
                            Expr::Int(0),
                            // dict : Eq Char
                            Expr::mk_record(
                                vec![(Expr::Int(0), Expr::Builtin(Builtin::EqChar))],
                                None,
                            ),
                        ),
                        (Expr::Int(1), Expr::Builtin(Builtin::CompareChar)),
                    ],
                    None,
                )),
            },
            /*
            instance Ord String where
              compare = compareString
             */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Ord"),
                    ),
                    Type::String,
                ),
                evidence: Rc::from("Ord String"),
            },
            Declaration::Evidence {
                name: Rc::from("Ord String"),
                body: Rc::new(Expr::mk_record(
                    vec![
                        (
                            Expr::Int(0),
                            // dict : Eq String
                            Expr::mk_record(
                                vec![(Expr::Int(0), Expr::Builtin(Builtin::EqString))],
                                None,
                            ),
                        ),
                        (Expr::Int(1), Expr::Builtin(Builtin::CompareString)),
                    ],
                    None,
                )),
            },
            /*
            instance Ord Int where
              compare = compareInt
             */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Ord"),
                    ),
                    Type::Int,
                ),
                evidence: Rc::from("Ord Int"),
            },
            Declaration::Evidence {
                name: Rc::from("Ord Int"),
                body: Rc::new(Expr::mk_record(
                    vec![
                        (
                            Expr::Int(0),
                            // dict : Eq Int
                            Expr::mk_record(
                                vec![(Expr::Int(0), Expr::Builtin(Builtin::EqInt))],
                                None,
                            ),
                        ),
                        (Expr::Int(1), Expr::Builtin(Builtin::CompareInt)),
                    ],
                    None,
                )),
            },
            /*
            instance Ord a => Ord (Array a) where
              compare = compareArray compare
             */
            Declaration::Instance {
                ty_vars: vec![(Rc::from("a"), Kind::Type)],
                assumes: vec![Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Ord"),
                    ),
                    Type::Var(Kind::Type, 0),
                )],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Ord"),
                    ),
                    Type::app(
                        Type::Array(Kind::mk_arrow(&Kind::Type, &Kind::Type)),
                        Type::Var(Kind::Type, 0),
                    ),
                ),
                evidence: Rc::from("Ord a => Ord (Array a)"),
            },
            Declaration::Evidence {
                name: Rc::from("Ord a => Ord (Array a)"),
                // \ordDict -> ...
                body: Rc::new(Expr::mk_lam(
                    true,
                    Expr::mk_record(
                        vec![
                            (
                                // eqDict
                                Expr::Int(0),
                                // { eq = builtins.eqArray (eq ordDict.eqDict) }
                                Expr::mk_record(
                                    vec![(
                                        Expr::Int(0),
                                        Expr::mk_app(
                                            Expr::Builtin(Builtin::EqArray),
                                            Expr::mk_app(
                                                Expr::Name(Name::definition("eq")),
                                                Expr::mk_project(Expr::Var(0), Expr::Int(0)),
                                            ),
                                        ),
                                    )],
                                    None,
                                ),
                            ),
                            (
                                // compare
                                Expr::Int(1),
                                // compareArray (compare ordDict)
                                Expr::mk_app(
                                    Expr::Builtin(Builtin::CompareArray),
                                    Expr::mk_app(
                                        Expr::Name(Name::definition("compare")),
                                        Expr::Var(0),
                                    ),
                                ),
                            ),
                        ],
                        None,
                    ),
                )),
            },
            {
                let a = Type::Var(Kind::Type, 0);
                Declaration::Definition {
                    name: String::from("neq"),
                    sig: TypeSig::new(
                        vec![(Rc::from("a"), Kind::Type)],
                        Type::mk_fatarrow(
                            common_kinds,
                            Type::app(
                                Type::Name(
                                    Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                                    Rc::from("Eq"),
                                ),
                                a.clone(),
                            ),
                            Type::arrow(
                                common_kinds,
                                a.clone(),
                                Type::arrow(common_kinds, a, Type::Bool),
                            ),
                        ),
                    ),
                    // \eqDict a b -> if eq eqDict a b then false else true
                    body: Rc::new(Expr::mk_lam(
                        true,
                        Expr::mk_lam(
                            true,
                            Expr::mk_lam(
                                true,
                                Expr::mk_ifthenelse(
                                    Expr::mk_app(
                                        Expr::mk_app(
                                            Expr::mk_app(
                                                Expr::Name(Name::definition("eq")),
                                                Expr::Var(2),
                                            ),
                                            Expr::Var(1),
                                        ),
                                        Expr::Var(0),
                                    ),
                                    Expr::False,
                                    Expr::True,
                                ),
                            ),
                        ),
                    )),
                }
            },
            {
                let a = Type::Var(Kind::Type, 0);
                Declaration::Definition {
                    name: String::from("lt"),
                    // Ord a => a -> a -> Bool
                    sig: TypeSig::new(
                        vec![(Rc::from("a"), Kind::Type)],
                        Type::mk_fatarrow(
                            common_kinds,
                            Type::app(
                                Type::Name(
                                    Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                                    Rc::from("Ord"),
                                ),
                                a.clone(),
                            ),
                            Type::arrow(
                                common_kinds,
                                a.clone(),
                                Type::arrow(common_kinds, a, Type::Bool),
                            ),
                        ),
                    ),
                    /*
                    \ordDict a b ->
                      case compare ordDict a b of
                        Less () -> True
                        _ -> False
                    */
                    body: Rc::new(Expr::mk_lam(
                        true,
                        Expr::mk_lam(
                            true,
                            Expr::mk_lam(
                                true,
                                Expr::mk_case(
                                    Expr::mk_app(
                                        Expr::mk_app(
                                            Expr::mk_app(
                                                Expr::Name(Name::definition("compare")),
                                                Expr::Var(2),
                                            ),
                                            Expr::Var(1),
                                        ),
                                        Expr::Var(0),
                                    ),
                                    vec![
                                        Branch {
                                            // Less () : (| Equal : (), Greater : (), Less : () |)
                                            pattern: Pattern::Variant {
                                                tag: Rc::new(Expr::Int(2)),
                                            },
                                            body: Expr::True,
                                        },
                                        Branch {
                                            pattern: Pattern::Wildcard,
                                            body: Expr::False,
                                        },
                                    ],
                                ),
                            ),
                        ),
                    )),
                }
            },
            {
                let a = Type::Var(Kind::Type, 0);
                Declaration::Definition {
                    name: String::from("lte"),
                    // Ord a => a -> a -> Bool
                    sig: TypeSig::new(
                        vec![(Rc::from("a"), Kind::Type)],
                        Type::mk_fatarrow(
                            common_kinds,
                            Type::app(
                                Type::Name(
                                    Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                                    Rc::from("Ord"),
                                ),
                                a.clone(),
                            ),
                            Type::arrow(
                                common_kinds,
                                a.clone(),
                                Type::arrow(common_kinds, a, Type::Bool),
                            ),
                        ),
                    ),
                    /*
                    \ordDict a b ->
                      case compare ordDict a b of
                        Greater () -> False
                        _ -> True
                    */
                    body: Rc::new(Expr::mk_lam(
                        true,
                        Expr::mk_lam(
                            true,
                            Expr::mk_lam(
                                true,
                                Expr::mk_case(
                                    Expr::mk_app(
                                        Expr::mk_app(
                                            Expr::mk_app(
                                                Expr::Name(Name::definition("compare")),
                                                Expr::Var(2),
                                            ),
                                            Expr::Var(1),
                                        ),
                                        Expr::Var(0),
                                    ),
                                    vec![
                                        Branch {
                                            // Greater () : (| Equal : (), Greater : (), Less : () |)
                                            pattern: Pattern::Variant {
                                                tag: Rc::new(Expr::Int(1)),
                                            },
                                            body: Expr::False,
                                        },
                                        Branch {
                                            pattern: Pattern::Wildcard,
                                            body: Expr::True,
                                        },
                                    ],
                                ),
                            ),
                        ),
                    )),
                }
            },
            {
                let a = Type::Var(Kind::Type, 0);
                Declaration::Definition {
                    name: String::from("gt"),
                    // Ord a => a -> a -> Bool
                    sig: TypeSig::new(
                        vec![(Rc::from("a"), Kind::Type)],
                        Type::mk_fatarrow(
                            common_kinds,
                            Type::app(
                                Type::Name(
                                    Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                                    Rc::from("Ord"),
                                ),
                                a.clone(),
                            ),
                            Type::arrow(
                                common_kinds,
                                a.clone(),
                                Type::arrow(common_kinds, a, Type::Bool),
                            ),
                        ),
                    ),
                    /*
                    \ordDict a b ->
                      case compare ordDict a b of
                        Greater () -> True
                        _ -> False
                    */
                    body: Rc::new(Expr::mk_lam(
                        true,
                        Expr::mk_lam(
                            true,
                            Expr::mk_lam(
                                true,
                                Expr::mk_case(
                                    Expr::mk_app(
                                        Expr::mk_app(
                                            Expr::mk_app(
                                                Expr::Name(Name::definition("compare")),
                                                Expr::Var(2),
                                            ),
                                            Expr::Var(1),
                                        ),
                                        Expr::Var(0),
                                    ),
                                    vec![
                                        Branch {
                                            // Greater () : (| Equal : (), Greater : (), Less : () |)
                                            pattern: Pattern::Variant {
                                                tag: Rc::new(Expr::Int(1)),
                                            },
                                            body: Expr::True,
                                        },
                                        Branch {
                                            pattern: Pattern::Wildcard,
                                            body: Expr::False,
                                        },
                                    ],
                                ),
                            ),
                        ),
                    )),
                }
            },
            {
                let a = Type::Var(Kind::Type, 0);
                Declaration::Definition {
                    name: String::from("gte"),
                    // Ord a => a -> a -> Bool
                    sig: TypeSig::new(
                        vec![(Rc::from("a"), Kind::Type)],
                        Type::mk_fatarrow(
                            common_kinds,
                            Type::app(
                                Type::Name(
                                    Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                                    Rc::from("Ord"),
                                ),
                                a.clone(),
                            ),
                            Type::arrow(
                                common_kinds,
                                a.clone(),
                                Type::arrow(common_kinds, a, Type::Bool),
                            ),
                        ),
                    ),
                    /*
                    \ordDict a b ->
                      case compare ordDict a b of
                        Less () -> False
                        _ -> True
                    */
                    body: Rc::new(Expr::mk_lam(
                        true,
                        Expr::mk_lam(
                            true,
                            Expr::mk_lam(
                                true,
                                Expr::mk_case(
                                    Expr::mk_app(
                                        Expr::mk_app(
                                            Expr::mk_app(
                                                Expr::Name(Name::definition("compare")),
                                                Expr::Var(2),
                                            ),
                                            Expr::Var(1),
                                        ),
                                        Expr::Var(0),
                                    ),
                                    vec![
                                        Branch {
                                            // Less () : (| Equal : (), Greater : (), Less : () |)
                                            pattern: Pattern::Variant {
                                                tag: Rc::new(Expr::Int(2)),
                                            },
                                            body: Expr::False,
                                        },
                                        Branch {
                                            pattern: Pattern::Wildcard,
                                            body: Expr::True,
                                        },
                                    ],
                                ),
                            ),
                        ),
                    )),
                }
            },
            /*
            class ToArgs a where
              toArgs : a -> Array String
            */
            Declaration::Class(ClassDeclaration {
                supers: vec![],
                name: Rc::from("ToArgs"),
                args: vec![(Rc::from("a"), Kind::Type)],
                members: vec![ClassMember {
                    name: String::from("toArgs"),
                    sig: TypeSig::new(
                        vec![],
                        Type::arrow(
                            common_kinds,
                            Type::Var(Kind::Type, 0),
                            Type::app(array_ty.clone(), Type::String),
                        ),
                    ),
                }],
            }),
            /*
            instance ToArgs String where
              toArgs x = [x]
            */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("ToArgs"),
                    ),
                    Type::String,
                ),
                evidence: Rc::from("ToArgs String"),
            },
            Declaration::Evidence {
                name: Rc::from("ToArgs String"),
                body: Rc::new(Expr::mk_record(
                    vec![(
                        // toArgs
                        Expr::Int(0),
                        // \x -> [x]
                        Expr::mk_lam(true, Expr::Array(vec![Expr::Var(0)])),
                    )],
                    None,
                )),
            },
            /*
            instance ToArgs a => ToArgs (Array a) where
              toArgs = array.flatMap toArgs
            */
            {
                let to_args_ty = Type::Name(
                    Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                    Rc::from("ToArgs"),
                );
                Declaration::Instance {
                    ty_vars: vec![(Rc::from("a"), Kind::Type)],
                    assumes: vec![Type::app(to_args_ty.clone(), Type::Var(Kind::Type, 0))],
                    head: Type::app(to_args_ty, Type::app(array_ty, Type::Var(Kind::Type, 0))),
                    evidence: Rc::from("ToArgs a => ToArgs (Array a)"),
                }
            },
            Declaration::Evidence {
                name: Rc::from("ToArgs a => ToArgs (Array a)"),
                // \toArgsDict -> ...
                body: Rc::new(Expr::mk_lam(
                    true,
                    Expr::mk_record(
                        vec![(
                            // toArgs
                            Expr::Int(0),
                            // array.flatMap (toArgs toArgsDict)
                            Expr::mk_app(
                                Expr::Module {
                                    // TODO: this should be a distinguished value of "self"
                                    id: ModuleId::new(0),
                                    path: vec![String::from("array")],
                                    item: Name::definition("flatMap"),
                                },
                                Expr::mk_app(Expr::Name(Name::definition("toArgs")), Expr::Var(0)),
                            ),
                        )],
                        None,
                    ),
                )),
            },
        ],
    }
}
