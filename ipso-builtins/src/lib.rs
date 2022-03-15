use ipso_core::{
    Builtin, ClassDeclaration, ClassMember, CommonKinds, Declaration, Expr, InstanceMember, Module,
    Type, TypeSig,
};
use ipso_syntax::kind::Kind;
use std::collections::HashMap;
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
            },
            // pure : a -> IO a
            Declaration::Definition {
                name: String::from("pure"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![
                            // a : Type
                            (Rc::from("a"), a.kind()),
                        ],
                        body: Type::arrow(common_kinds, a.clone(), Type::app(io_ty.clone(), a)),
                    }
                },
                body: Expr::alloc_builtin(Builtin::Pure),
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
                            (Rc::from("a"), a.kind()),
                            // b : Type
                            (Rc::from("b"), a.kind()),
                        ],
                        body: Type::arrow(
                            common_kinds,
                            Type::app(io_ty.clone(), a.clone()),
                            Type::arrow(
                                common_kinds,
                                Type::arrow(common_kinds, a, Type::app(io_ty.clone(), b.clone())),
                                Type::app(io_ty.clone(), b),
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
                            (Rc::from("a"), a.kind()),
                            // b : Type
                            (Rc::from("b"), a.kind()),
                        ],
                        body: Type::arrow(common_kinds, a, Type::arrow(common_kinds, b.clone(), b)),
                    }
                },
                body: Expr::alloc_builtin(Builtin::Trace),
            },
            // toUtf8 : String -> Bytes
            Declaration::Definition {
                name: String::from("toUtf8"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: Type::arrow(common_kinds, string_ty.clone(), bytes_ty),
                },
                body: Expr::alloc_builtin(Builtin::ToUtf8),
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
            // eqString : String -> String -> Bool
            Declaration::Definition {
                name: String::from("eqString"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::arrow(
                        common_kinds,
                        string_ty.clone(),
                        Type::arrow(common_kinds, string_ty.clone(), bool_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::EqString),
            },
            // eqInt : Int -> Int -> Bool
            Declaration::Definition {
                name: String::from("eqInt"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::arrow(
                        common_kinds,
                        int_ty.clone(),
                        Type::arrow(common_kinds, int_ty.clone(), bool_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::EqInt),
            },
            // ltInt : Int -> Int -> Bool
            Declaration::Definition {
                name: String::from("ltInt"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::arrow(
                        common_kinds,
                        int_ty.clone(),
                        Type::arrow(common_kinds, int_ty.clone(), bool_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::LtInt),
            },
            // showInt : Int -> String
            Declaration::Definition {
                name: String::from("showInt"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::arrow(common_kinds, int_ty.clone(), string_ty.clone()),
                },
                body: Expr::alloc_builtin(Builtin::ShowInt),
            },
            // eqArray : (a -> a -> Bool) -> Array a -> Array a -> Bool
            Declaration::Definition {
                name: String::from("eqArray"),
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
            },
            // ltArray : (a -> a -> Bool) -> Array a -> Array a -> Bool
            Declaration::Definition {
                name: String::from("ltArray"),
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
                body: Expr::alloc_builtin(Builtin::LtArray),
            },
            // foldlArray : (b -> a -> b) -> b -> Array a -> b
            Declaration::Definition {
                name: String::from("foldlArray"),
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
                                Type::arrow(common_kinds, Type::app(array_ty.clone(), a), b),
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
            },
            // lengthArray : Array a -> Int
            Declaration::Definition {
                name: String::from("lengthArray"),
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
            },
            // indexArray : Int -> Array a -> a
            Declaration::Definition {
                name: String::from("indexArray"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![(Rc::from("a"), a.kind())],
                        body: Type::arrow(
                            common_kinds,
                            int_ty.clone(),
                            Type::arrow(common_kinds, Type::app(array_ty.clone(), a.clone()), a),
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
            },
            // filterString : (Char -> Bool) -> String -> String
            Declaration::Definition {
                name: String::from("filterString"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::arrow(
                        common_kinds,
                        Type::arrow(common_kinds, char_ty.clone(), bool_ty.clone()),
                        Type::arrow(common_kinds, string_ty.clone(), string_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::FilterString),
            },
            // eqChar : Char -> Char -> Bool
            Declaration::Definition {
                name: String::from("eqChar"),
                sig: TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::arrow(
                        common_kinds,
                        char_ty.clone(),
                        Type::arrow(common_kinds, char_ty.clone(), bool_ty.clone()),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::EqChar),
            },
            // splitString : Char -> String -> Array String
            Declaration::Definition {
                name: String::from("splitString"),
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
            },
            // foldlString : (a -> Char -> a) -> a -> String -> a
            Declaration::Definition {
                name: String::from("foldlString"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Type);
                    TypeSig {
                        ty_vars: vec![(Rc::from("a"), a.kind())],
                        body: Type::arrow(
                            common_kinds,
                            Type::arrow(
                                common_kinds,
                                a.clone(),
                                Type::arrow(common_kinds, char_ty, a.clone()),
                            ),
                            Type::arrow(
                                common_kinds,
                                a.clone(),
                                Type::arrow(common_kinds, string_ty, a),
                            ),
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
                        body: Type::arrow(
                            common_kinds,
                            Type::app(array_ty.clone(), a.clone()),
                            Type::arrow(common_kinds, a.clone(), Type::app(array_ty.clone(), a)),
                        ),
                    }
                },
                body: Expr::alloc_builtin(Builtin::SnocArray),
            },
            // run : Cmd -> IO ()
            Declaration::Definition {
                name: String::from("run"),
                sig: {
                    TypeSig {
                        ty_vars: Vec::new(),
                        body: Type::arrow(common_kinds, Type::Cmd, Type::app(io_ty, Type::Unit)),
                    }
                },
                body: Expr::alloc_builtin(Builtin::Run),
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
                superclass_constructors: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Eq"),
                    ),
                    Type::Int,
                ),
                members: vec![InstanceMember {
                    name: String::from("eq"),
                    body: Expr::Builtin(Builtin::EqInt),
                }],
            },
            /*
            instance Eq Char where
              eq = eqChar
             */
            Declaration::Instance {
                ty_vars: vec![],
                superclass_constructors: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Eq"),
                    ),
                    Type::Char,
                ),
                members: vec![InstanceMember {
                    name: String::from("eq"),
                    body: Expr::Builtin(Builtin::EqChar),
                }],
            },
            /*
            instance Eq String where
              eq = eqString
             */
            Declaration::Instance {
                ty_vars: vec![],
                superclass_constructors: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Eq"),
                    ),
                    Type::String,
                ),
                members: vec![InstanceMember {
                    name: String::from("eq"),
                    body: Expr::Builtin(Builtin::EqString),
                }],
            },
            /*
            instance Eq Bool where
              eq = eqBool
             */
            Declaration::Instance {
                ty_vars: vec![],
                superclass_constructors: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Eq"),
                    ),
                    Type::Bool,
                ),
                members: vec![InstanceMember {
                    name: String::from("eq"),
                    body: Expr::Builtin(Builtin::EqBool),
                }],
            },
            /*
            instance Eq a where Eq (Array a) where
              eq = eqArray eq
             */
            Declaration::Instance {
                ty_vars: vec![(Rc::from("a"), Kind::Type)],
                superclass_constructors: vec![],
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
                    Type::app(array_ty, Type::Var(Kind::Type, 0)),
                ),
                members: vec![InstanceMember {
                    name: String::from("eq"),
                    body: Expr::mk_lam(
                        true,
                        Expr::mk_app(
                            Expr::Builtin(Builtin::EqArray),
                            Expr::mk_app(Expr::Name(String::from("eq")), Expr::Var(0)),
                        ),
                    ),
                }],
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
                superclass_constructors: vec![
                    // dict : Eq Bool
                    Expr::mk_record(vec![(Expr::Int(0), Expr::Builtin(Builtin::EqBool))], None),
                ],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Ord"),
                    ),
                    Type::Bool,
                ),
                members: vec![InstanceMember {
                    name: String::from("compare"),
                    body: Expr::mk_lam(
                        true,
                        Expr::mk_lam(
                            true,
                            Expr::mk_ifthenelse(
                                Expr::Var(1),
                                Expr::mk_ifthenelse(
                                    Expr::Var(0),
                                    // EQ () : (| EQ : (), GT : (), LT : () |)
                                    Expr::mk_app(Expr::mk_variant(Expr::Int(0)), Expr::Unit),
                                    // GT () : (| EQ : (), GT : (), LT : () |)
                                    Expr::mk_app(Expr::mk_variant(Expr::Int(1)), Expr::Unit),
                                ),
                                Expr::mk_ifthenelse(
                                    Expr::Var(0),
                                    // LT () : (| EQ : (), GT : (), LT : () |)
                                    Expr::mk_app(Expr::mk_variant(Expr::Int(2)), Expr::Unit),
                                    // EQ () : (| EQ : (), GT : (), LT : () |)
                                    Expr::mk_app(Expr::mk_variant(Expr::Int(0)), Expr::Unit),
                                ),
                            ),
                        ),
                    ),
                }],
            },
            /*
            instance Ord Char where
              compare = compareChar
             */
            Declaration::Instance {
                ty_vars: vec![],
                superclass_constructors: vec![
                    // dict : Eq Char
                    Expr::mk_record(vec![(Expr::Int(0), Expr::Builtin(Builtin::EqChar))], None),
                ],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Ord"),
                    ),
                    Type::Char,
                ),
                members: vec![InstanceMember {
                    name: String::from("compare"),
                    body: Expr::Builtin(Builtin::CompareChar),
                }],
            },
        ],
    }
}
