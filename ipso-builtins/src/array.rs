use ipso_core::{Builtin, CommonKinds, Declaration, Expr, Type, TypeSig};
use ipso_syntax::kind::Kind;
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    vec![
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
            name: String::from("generate"),
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
            name: String::from("length"),
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
            name: String::from("index"),
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
            name: String::from("slice"),
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
            name: String::from("snoc"),
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
            name: String::from("map"),
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
            name: String::from("flatMap"),
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
    ]
}
