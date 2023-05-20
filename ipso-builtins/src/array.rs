use ipso_core::{Binop, Builtin, CommonKinds, Declaration, Expr, Name, Type, TypeSig};
use ipso_syntax::{kind::Kind, ModuleRef};
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    vec![
        // eq : (a -> a -> Bool) -> Array a -> Array a -> Bool
        Rc::new(Declaration::Definition {
            name: Rc::from("eq"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), a.kind())],
                    body: Type::arrow(
                        common_kinds,
                        Type::arrow(
                            common_kinds,
                            a.clone(),
                            Type::arrow(common_kinds, a.clone(), Type::Bool),
                        ),
                        Type::arrow(
                            common_kinds,
                            Type::app(Type::mk_array(common_kinds), a.clone()),
                            Type::arrow(
                                common_kinds,
                                Type::app(Type::mk_array(common_kinds), a),
                                Type::Bool,
                            ),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::EqArray),
        }),
        // foldl : (b -> a -> b) -> b -> Array a -> b
        Rc::new(Declaration::Definition {
            name: Rc::from("foldl"),
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
                                Type::app(Type::mk_array(common_kinds), a),
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
            name: Rc::from("generate"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), a.kind())],
                    body: Type::arrow(
                        common_kinds,
                        Type::Int,
                        Type::arrow(
                            common_kinds,
                            Type::arrow(common_kinds, Type::Int, a.clone()),
                            Type::app(Type::mk_array(common_kinds), a),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::GenerateArray),
        }),
        // length : Array a -> Int
        Rc::new(Declaration::Definition {
            name: Rc::from("length"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), a.kind())],
                    body: Type::arrow(
                        common_kinds,
                        Type::app(Type::mk_array(common_kinds), a),
                        Type::Int,
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::LengthArray),
        }),
        // index : Int -> Array a -> a
        Rc::new(Declaration::Definition {
            name: Rc::from("index"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), a.kind())],
                    body: Type::arrow(
                        common_kinds,
                        Type::Int,
                        Type::arrow(
                            common_kinds,
                            Type::app(Type::mk_array(common_kinds), a.clone()),
                            a,
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::IndexArray),
        }),
        // slice : Int -> Int -> Array a -> Array a
        Rc::new(Declaration::Definition {
            name: Rc::from("slice"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), a.kind())],
                    body: Type::arrow(
                        common_kinds,
                        Type::Int,
                        Type::arrow(
                            common_kinds,
                            Type::Int,
                            Type::arrow(
                                common_kinds,
                                Type::app(Type::mk_array(common_kinds), a.clone()),
                                Type::app(Type::mk_array(common_kinds), a),
                            ),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::SliceArray),
        }),
        // snoc : Array a -> a -> Array a
        Rc::new(Declaration::Definition {
            name: Rc::from("snoc"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), Kind::Type)],
                    body: Type::arrow(
                        common_kinds,
                        Type::app(Type::mk_array(common_kinds), a.clone()),
                        Type::arrow(
                            common_kinds,
                            a.clone(),
                            Type::app(Type::mk_array(common_kinds), a),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::SnocArray),
        }),
        // map : (a -> b) -> Array a -> Array b
        Rc::new(Declaration::Definition {
            name: Rc::from("map"),
            sig: TypeSig::new(
                vec![(Rc::from("a"), Kind::Type), (Rc::from("b"), Kind::Type)],
                Type::arrow(
                    common_kinds,
                    Type::arrow(
                        common_kinds,
                        Type::Var(Kind::Type, 1),
                        Type::Var(Kind::Type, 0),
                    ),
                    Type::arrow(
                        common_kinds,
                        Type::app(Type::mk_array(common_kinds), Type::Var(Kind::Type, 1)),
                        Type::app(Type::mk_array(common_kinds), Type::Var(Kind::Type, 0)),
                    ),
                ),
            ),
            body: Rc::new(Expr::Builtin(Builtin::MapArray)),
        }),
        // flatMap : (a -> Array b) -> Array a -> Array b
        Rc::new(Declaration::Definition {
            name: Rc::from("flatMap"),
            sig: TypeSig::new(
                vec![(Rc::from("a"), Kind::Type), (Rc::from("b"), Kind::Type)],
                Type::arrow(
                    common_kinds,
                    Type::arrow(
                        common_kinds,
                        Type::Var(Kind::Type, 1),
                        Type::app(Type::mk_array(common_kinds), Type::Var(Kind::Type, 0)),
                    ),
                    Type::arrow(
                        common_kinds,
                        Type::app(Type::mk_array(common_kinds), Type::Var(Kind::Type, 1)),
                        Type::app(Type::mk_array(common_kinds), Type::Var(Kind::Type, 0)),
                    ),
                ),
            ),
            body: Rc::new(Expr::Builtin(Builtin::FlatMap)),
        }),
        {
            let s = Type::Var(Kind::Type, 1);
            let a = Type::Var(Kind::Type, 0);
            // unfoldr : s -> (s -> (| Step : { value : a, next : s }, Skip : { next : s }, Done : () |)) -> Array a
            Rc::new(Declaration::Definition {
                name: Rc::from("unfoldr"),
                sig: TypeSig {
                    ty_vars: vec![(Rc::from("s"), a.kind()), (Rc::from("a"), a.kind())],
                    body: Type::arrow(
                        common_kinds,
                        s.clone(),
                        Type::arrow(
                            common_kinds,
                            Type::arrow(
                                common_kinds,
                                s.clone(),
                                Type::mk_variant(
                                    common_kinds,
                                    vec![
                                        (
                                            Rc::from("Step"),
                                            Type::mk_record(
                                                common_kinds,
                                                vec![
                                                    (Rc::from("value"), a.clone()),
                                                    (Rc::from("next"), s.clone()),
                                                ],
                                                None,
                                            ),
                                        ),
                                        (
                                            Rc::from("Skip"),
                                            Type::mk_record(
                                                common_kinds,
                                                vec![(Rc::from("next"), s)],
                                                None,
                                            ),
                                        ),
                                        (Rc::from("Done"), Type::Unit),
                                    ],
                                    None,
                                ),
                            ),
                            Type::app(Type::mk_array(common_kinds), a),
                        ),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::ArrayUnfoldr),
            })
        },
        // sum : Array Int -> Int
        Rc::new(Declaration::Definition {
            name: Rc::from("sum"),
            sig: {
                TypeSig {
                    ty_vars: vec![],
                    body: Type::arrow(
                        common_kinds,
                        Type::app(Type::mk_array(common_kinds), Type::Int),
                        Type::Int,
                    ),
                }
            },
            // foldl (\a b -> a + b) 0
            body: Rc::new(Expr::mk_app(
                Expr::mk_app(
                    Expr::Module {
                        id: ModuleRef::This,
                        path: vec![],
                        item: Name::definition("foldl"),
                    },
                    Expr::mk_lam(
                        true,
                        Expr::mk_lam(true, Expr::mk_binop(Binop::Add, Expr::Var(1), Expr::Var(0))),
                    ),
                ),
                Expr::Int(0),
            )),
        }),
        // any : (a -> Bool) -> Array a -> Bool
        Rc::new(Declaration::Definition {
            name: Rc::from("any"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), Kind::Type)],
                    body: Type::arrow(
                        common_kinds,
                        Type::arrow(common_kinds, a.clone(), Type::Bool),
                        Type::arrow(
                            common_kinds,
                            Type::app(Type::mk_array(common_kinds), a),
                            Type::Bool,
                        ),
                    ),
                }
            },
            // \f -> foldl (\a b -> a || f b) false
            body: Rc::new(Expr::mk_lam(
                true,
                Expr::mk_app(
                    Expr::mk_app(
                        Expr::Module {
                            id: ModuleRef::This,
                            path: vec![],
                            item: Name::definition("foldl"),
                        },
                        Expr::mk_lam(
                            true,
                            Expr::mk_lam(
                                true,
                                Expr::mk_binop(
                                    Binop::Or,
                                    Expr::Var(1),
                                    Expr::mk_app(Expr::Var(2), Expr::Var(0)),
                                ),
                            ),
                        ),
                    ),
                    Expr::False,
                ),
            )),
        }),
        // each_ : Array a -> (a -> IO ()) -> IO ()
        Rc::new(Declaration::Definition {
            name: Rc::from("each_"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), a.kind())],
                    body: Type::arrow(
                        common_kinds,
                        // Array a
                        Type::app(Type::mk_array(common_kinds), a.clone()),
                        Type::arrow(
                            common_kinds,
                            // a -> IO ()
                            Type::arrow(
                                common_kinds,
                                a,
                                Type::app(Type::mk_io(common_kinds), Type::Unit),
                            ),
                            // IO ()
                            Type::app(Type::mk_io(common_kinds), Type::Unit),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::ArrayEach_),
        }),
        // filter : (a -> Bool) -> Array a -> Array a
        Rc::new(Declaration::Definition {
            name: Rc::from("filter"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), a.kind())],
                    body: Type::arrow(
                        common_kinds,
                        Type::arrow(common_kinds, a.clone(), Type::Bool),
                        Type::arrow(
                            common_kinds,
                            Type::app(Type::mk_array(common_kinds), a.clone()),
                            Type::app(Type::mk_array(common_kinds), a),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::ArrayFilter),
        }),
        // filterMap : (a -> (| Some : b, None : () |)) -> Array a -> Array b
        Rc::new(Declaration::Definition {
            name: Rc::from("filterMap"),
            sig: {
                let a = Type::unsafe_mk_var(1, Kind::Type);
                let b = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), a.kind()), (Rc::from("b"), b.kind())],
                    body: Type::arrow(
                        common_kinds,
                        Type::arrow(
                            common_kinds,
                            a.clone(),
                            Type::mk_variant(
                                common_kinds,
                                vec![
                                    (Rc::from("Some"), b.clone()),
                                    (Rc::from("None"), Type::Unit),
                                ],
                                None,
                            ),
                        ),
                        Type::arrow(
                            common_kinds,
                            Type::app(Type::mk_array(common_kinds), a),
                            Type::app(Type::mk_array(common_kinds), b),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::ArrayFilterMap),
        }),
        // find : (a -> Bool) -> Array a -> (| Some : a, None : () |)
        Rc::new(Declaration::Definition {
            name: Rc::from("find"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), a.kind())],
                    body: Type::arrow(
                        common_kinds,
                        Type::arrow(common_kinds, a.clone(), Type::Bool),
                        Type::arrow(
                            common_kinds,
                            Type::app(Type::mk_array(common_kinds), a.clone()),
                            Type::mk_variant(
                                common_kinds,
                                vec![(Rc::from("Some"), a), (Rc::from("None"), Type::Unit)],
                                None,
                            ),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::ArrayFind),
        }),
        // findMap : (a -> (| Some : b, None : () |)) -> Array a -> (| Some : b, None : () |)
        Rc::new(Declaration::Definition {
            name: Rc::from("findMap"),
            sig: {
                let a = Type::unsafe_mk_var(1, Kind::Type);
                let b = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), a.kind()), (Rc::from("b"), b.kind())],
                    body: Type::arrow(
                        common_kinds,
                        Type::arrow(
                            common_kinds,
                            a.clone(),
                            Type::mk_variant(
                                common_kinds,
                                vec![
                                    (Rc::from("Some"), b.clone()),
                                    (Rc::from("None"), Type::Unit),
                                ],
                                None,
                            ),
                        ),
                        Type::arrow(
                            common_kinds,
                            Type::app(Type::mk_array(common_kinds), a),
                            Type::mk_variant(
                                common_kinds,
                                vec![(Rc::from("Some"), b), (Rc::from("None"), Type::Unit)],
                                None,
                            ),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::ArrayFindMap),
        }),
        // swap : Int -> Int -> Array a -> Array a
        Rc::new(Declaration::Definition {
            name: Rc::from("swap"),
            sig: {
                let a = Type::unsafe_mk_var(0, Kind::Type);
                TypeSig {
                    ty_vars: vec![(Rc::from("a"), a.kind())],
                    body: Type::arrow(
                        common_kinds,
                        Type::Int,
                        Type::arrow(
                            common_kinds,
                            Type::Int,
                            Type::arrow(
                                common_kinds,
                                Type::app(Type::mk_array(common_kinds), a.clone()),
                                Type::app(Type::mk_array(common_kinds), a),
                            ),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::ArraySwap),
        }),
    ]
}
