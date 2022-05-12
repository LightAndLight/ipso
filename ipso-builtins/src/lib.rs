pub mod array;
pub mod char;
pub mod cmd;
pub mod int;
pub mod io;
pub mod string;

use ipso_core::{
    Branch, Builtin, ClassDeclaration, ClassMember, CommonKinds, Declaration, Expr, Module, Name,
    Pattern, StringPart, Type, TypeSig,
};
use ipso_syntax::{kind::Kind, ModuleRef};
use std::rc::Rc;

pub fn builtins(common_kinds: &CommonKinds) -> Module {
    let io_ty = Type::mk_io(common_kinds);
    let array_ty = Type::mk_array(common_kinds);

    Module {
        decls: vec![
            Declaration::Module {
                name: String::from("io"),
                decls: io::decls(common_kinds),
            },
            Declaration::Module {
                name: String::from("string"),
                decls: string::decls(common_kinds),
            },
            Declaration::Module {
                name: String::from("int"),
                decls: int::decls(common_kinds),
            },
            Declaration::Module {
                name: String::from("array"),
                decls: array::decls(common_kinds),
            },
            Declaration::Module {
                name: String::from("char"),
                decls: char::decls(common_kinds),
            },
            Declaration::Module {
                name: String::from("cmd"),
                decls: cmd::decls(common_kinds),
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
            // println : String -> IO ()
            Declaration::Definition {
                name: String::from("println"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: Type::arrow(
                        common_kinds,
                        Type::String,
                        Type::app(io_ty.clone(), Type::Unit),
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
                        Type::String,
                        Type::app(io_ty.clone(), Type::Unit),
                    ),
                },
                body: Expr::alloc_builtin(Builtin::Print),
            },
            // readln : IO String
            Declaration::Definition {
                name: String::from("readln"),
                sig: TypeSig {
                    ty_vars: vec![],
                    body: Type::app(io_ty, Type::String),
                },
                body: Expr::alloc_builtin(Builtin::Readln),
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
                            Type::arrow(common_kinds, Type::Var(Kind::Type, 0), Type::Bool),
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
                                    id: ModuleRef::This,
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
            /*
            class Debug a where
              Debug : a -> String
            */
            Declaration::Class(ClassDeclaration {
                supers: vec![],
                name: Rc::from("Debug"),
                args: vec![(Rc::from("a"), Kind::Type)],
                members: vec![ClassMember {
                    name: String::from("debug"),
                    sig: TypeSig::new(
                        vec![],
                        Type::arrow(common_kinds, Type::Var(Kind::Type, 0), Type::String),
                    ),
                }],
            }),
            /*
            instance Debug () where
              debug x = "()"
            */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Debug"),
                    ),
                    Type::Unit,
                ),
                evidence: Rc::from("Debug ()"),
            },
            Declaration::Evidence {
                name: Rc::from("Debug ()"),
                body: Rc::new(Expr::mk_record(
                    vec![(
                        // debug
                        Expr::Int(0),
                        // \x -> "()"
                        Expr::mk_lam(true, Expr::String(vec![StringPart::from("()")])),
                    )],
                    None,
                )),
            },
            /*
            instance Debug Int where
              debug = int.show
            */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Debug"),
                    ),
                    Type::Int,
                ),
                evidence: Rc::from("Debug Int"),
            },
            Declaration::Evidence {
                name: Rc::from("Debug Int"),
                body: Rc::new(Expr::mk_record(
                    vec![(
                        // debug
                        Expr::Int(0),
                        // int.show
                        Expr::Module {
                            id: ModuleRef::This,
                            path: vec![String::from("int")],
                            item: Name::definition("show"),
                        },
                    )],
                    None,
                )),
            },
            /*
            instance Debug Char where
              debug = char.toString
            */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Debug"),
                    ),
                    Type::Char,
                ),
                evidence: Rc::from("Debug Char"),
            },
            Declaration::Evidence {
                name: Rc::from("Debug Char"),
                body: Rc::new(Expr::mk_record(
                    vec![(
                        // debug
                        Expr::Int(0),
                        // char.toString
                        Expr::Module {
                            id: ModuleRef::This,
                            path: vec![String::from("char")],
                            item: Name::definition("toString"),
                        },
                    )],
                    None,
                )),
            },
            /*
            instance Debug String where
              debug = <debugString>
            */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Debug"),
                    ),
                    Type::String,
                ),
                evidence: Rc::from("Debug String"),
            },
            Declaration::Evidence {
                name: Rc::from("Debug String"),
                body: Rc::new(Expr::mk_record(
                    vec![(
                        // debug
                        Expr::Int(0),
                        Expr::Builtin(Builtin::DebugString),
                    )],
                    None,
                )),
            },
            /*
            instance Debug Bool where
              debug b = if b then "true" else "false"
            */
            Declaration::Instance {
                ty_vars: vec![],
                assumes: vec![],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Debug"),
                    ),
                    Type::Bool,
                ),
                evidence: Rc::from("Debug Bool"),
            },
            Declaration::Evidence {
                name: Rc::from("Debug Bool"),
                body: Rc::new(Expr::mk_record(
                    vec![(
                        // debug
                        Expr::Int(0),
                        // \b -> if b then "true" else "false"
                        Expr::mk_lam(
                            true,
                            Expr::mk_ifthenelse(
                                Expr::Var(0),
                                Expr::String(vec![StringPart::from("true")]),
                                Expr::String(vec![StringPart::from("false")]),
                            ),
                        ),
                    )],
                    None,
                )),
            },
            /*
            debugRecordFields :
              DebugRecordFields a =>
              Record a ->
              Array { field : String, value : String }
            */
            Declaration::Definition {
                name: String::from("debugRecordFields"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Row);
                    TypeSig {
                        ty_vars: vec![
                            // a : Type
                            (Rc::from("a"), a.kind()),
                        ],
                        body: Type::mk_fatarrow(
                            common_kinds,
                            Type::app(Type::DebugRecordFields, a.clone()),
                            Type::arrow(
                                common_kinds,
                                Type::app(Type::mk_record_ctor(common_kinds), a),
                                Type::app(
                                    Type::mk_array(common_kinds),
                                    Type::mk_record(
                                        common_kinds,
                                        vec![
                                            (Rc::from("field"), Type::String),
                                            (Rc::from("value"), Type::String),
                                        ],
                                        None,
                                    ),
                                ),
                            ),
                        ),
                    }
                },
                /*
                The evidence for DebugRecordFields is a function
                of type `Record a -> Array { field : String, value : String }`

                `\x -> x` takes that function and returns it.
                */
                body: Rc::new(Expr::mk_lam(true, Expr::Var(0))),
            },
            /*
            instance DebugRecordFields a => Debug { a } where
              debug record =
                let fields = debugRecordFields record in
                if fields == []
                then "{}"
                else "{ ${string.join ", " (array.map (\field -> "${field.field} = ${field.value}") fields)} }"
            */
            Declaration::Instance {
                ty_vars: vec![(Rc::from("a"), Kind::Row)],
                assumes: vec![Type::app(Type::DebugRecordFields, Type::Var(Kind::Row, 0))],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Debug"),
                    ),
                    // { a }
                    Type::app(Type::mk_record_ctor(common_kinds), Type::Var(Kind::Row, 0)),
                ),
                evidence: Rc::from("Debug Record"),
            },
            Declaration::Evidence {
                name: Rc::from("Debug Record"),
                // \debugRecordFields -> { debug = ... }
                body: Rc::new(Expr::mk_lam(
                    true,
                    Expr::mk_record(
                        vec![(
                            // debug
                            Expr::Int(0),
                            /*
                            \record ->
                              let fields = debugRecordFields record in
                               if fields == []
                               then "{}"
                               else "{ ${string.join ", " (array.map (\field -> "${field.field} = ${field.value}") fields)} }"
                            */
                            Expr::mk_lam(
                                true,
                                Expr::mk_let(
                                    Expr::mk_app(Expr::Var(1), Expr::Var(0)),
                                    Expr::mk_ifthenelse(
                                        // eqArray eqString fields []
                                        Expr::mk_app(
                                            // eqArray eqString fields
                                            Expr::mk_app(
                                                // eqArray eqString
                                                Expr::mk_app(
                                                    Expr::Builtin(Builtin::EqArray),
                                                    Expr::Builtin(Builtin::EqString),
                                                ),
                                                // fields
                                                Expr::Var(0),
                                            ),
                                            Expr::Array(vec![]),
                                        ),
                                        Expr::String(vec![StringPart::from("{}")]),
                                        Expr::String(vec![
                                            StringPart::from("{ "),
                                            StringPart::Expr(
                                                // string.join ", " (array.map (\field -> "${field.field} = ${field.value}") fields)
                                                Expr::mk_app(
                                                    // string.join ", "
                                                    Expr::mk_app(
                                                        Expr::Module {
                                                            id: ModuleRef::This,
                                                            path: vec![String::from("string")],
                                                            item: Name::definition("join"),
                                                        },
                                                        Expr::String(vec![StringPart::from(", ")]),
                                                    ),
                                                    // array.map (\field -> "${field.field} = ${field.value}") fields
                                                    Expr::mk_app(
                                                        // array.map (\field -> "${field.field} = ${field.value}")
                                                        Expr::mk_app(
                                                            Expr::Module {
                                                                id: ModuleRef::This,
                                                                path: vec![String::from("array")],
                                                                item: Name::definition("map"),
                                                            },
                                                            // \field -> "${field.field} = ${field.value}"
                                                            Expr::mk_lam(
                                                                true,
                                                                Expr::String(vec![
                                                                    StringPart::Expr(
                                                                        Expr::mk_project(
                                                                            Expr::Var(0),
                                                                            // .field
                                                                            Expr::Int(0),
                                                                        ),
                                                                    ),
                                                                    StringPart::from(" = "),
                                                                    StringPart::Expr(
                                                                        Expr::mk_project(
                                                                            Expr::Var(0),
                                                                            // .value
                                                                            Expr::Int(1),
                                                                        ),
                                                                    ),
                                                                ]),
                                                            ),
                                                        ),
                                                        // fields
                                                        Expr::Var(0),
                                                    ),
                                                ),
                                            ),
                                            StringPart::from(" }"),
                                        ]),
                                    ),
                                ),
                            ),
                        )],
                        None,
                    ),
                )),
            },
            /*
            debugVariantCtor :
              DebugVariantCtor a =>
              Variant a ->
              { ctor : String, value : String }
            */
            Declaration::Definition {
                name: String::from("debugVariantCtor"),
                sig: {
                    let a = Type::unsafe_mk_var(0, Kind::Row);
                    TypeSig {
                        ty_vars: vec![
                            // a : Type
                            (Rc::from("a"), a.kind()),
                        ],
                        body: Type::mk_fatarrow(
                            common_kinds,
                            Type::app(Type::DebugVariantCtor, a.clone()),
                            Type::arrow(
                                common_kinds,
                                Type::app(Type::mk_variant_ctor(common_kinds), a),
                                Type::app(
                                    Type::mk_array(common_kinds),
                                    Type::mk_record(
                                        common_kinds,
                                        vec![
                                            (Rc::from("ctor"), Type::String),
                                            (Rc::from("value"), Type::String),
                                        ],
                                        None,
                                    ),
                                ),
                            ),
                        ),
                    }
                },
                /*
                The evidence for DebugVariantCtor is a function
                of type `Variant a -> { ctor : String, value : String }`

                `\x -> x` takes that function and returns it.
                */
                body: Rc::new(Expr::mk_lam(true, Expr::Var(0))),
            },
            /*
            instance DebugVariantCtor a => Debug (| a |) where
              debug variant = let result = debugVariantCtor variant in "${result.ctor} ${result.value}"
            */
            Declaration::Instance {
                ty_vars: vec![(Rc::from("a"), Kind::Row)],
                assumes: vec![Type::app(Type::DebugVariantCtor, Type::Var(Kind::Row, 0))],
                head: Type::app(
                    Type::Name(
                        Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                        Rc::from("Debug"),
                    ),
                    // (| a |)
                    Type::app(Type::mk_variant_ctor(common_kinds), Type::Var(Kind::Row, 0)),
                ),
                evidence: Rc::from("Debug Variant"),
            },
            Declaration::Evidence {
                name: Rc::from("Debug Variant"),
                // \debugVariantCtor -> { debug = ... }
                body: Rc::new(Expr::mk_lam(
                    true,
                    Expr::mk_record(
                        vec![(
                            // debug
                            Expr::Int(0),
                            /*
                            \variant -> let result = debugVariantCtor variant in "${result.ctor} ${result.value}"
                            */
                            Expr::mk_lam(
                                true,
                                Expr::mk_let(
                                    Expr::mk_app(Expr::Var(1), Expr::Var(0)),
                                    Expr::String(vec![
                                        StringPart::Expr(Expr::mk_project(
                                            Expr::Var(0),
                                            Expr::Int(0),
                                        )),
                                        StringPart::from(" "),
                                        StringPart::Expr(Expr::mk_project(
                                            Expr::Var(0),
                                            Expr::Int(1),
                                        )),
                                    ]),
                                ),
                            ),
                        )],
                        None,
                    ),
                )),
            },
            /*
            instance Debug a => Debug (Array a) where
              debug x = string.join ", " <| array.map debug x
            */
            {
                let debug = Type::Name(
                    Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                    Rc::from("Debug"),
                );
                let a = Type::Var(Kind::Type, 0);
                
                Declaration::Instance {
                    ty_vars: vec![(Rc::from("a"), Kind::Type)],
                    assumes: vec![
                        // Debug a
                        Type::app(debug.clone(), a.clone()),
                    ],
                    head: 
                    // Debug (Array a)
                    Type::app(
                        debug,
                        Type::app(Type::mk_array(common_kinds), a),
                    ),
                    evidence: Rc::from("Debug Array"),
                }
            },
            Declaration::Evidence {
                name: Rc::from("Debug Array"),
                
                // \debugDict -> { debug = ... }
                body: Rc::new(Expr::mk_lam(true, Expr::mk_record(
                    vec![(
                        // debug
                        Expr::Int(0),
                        // \x -> "[${string.join ", " (array.map (debug debugDict) x)}]"
                        Expr::mk_lam(
                            true,
                            Expr::String(
                                vec![
                                    StringPart::from("["),
                                    StringPart::Expr(
                                        Expr::mk_app(
                                            Expr::mk_app(
                                                Expr::Module {
                                                    id: ModuleRef::This,
                                                    path: vec![String::from("string")],
                                                    item: Name::definition("join"),
                                                },
                                                Expr::String(vec![StringPart::from(", ")]),
                                            ),
                                            Expr::mk_app(
                                                Expr::mk_app(
                                                    Expr::Module {
                                                        id: ModuleRef::This,
                                                        path: vec![String::from("array")],
                                                        item: Name::definition("map"),
                                                    },
                                                    Expr::mk_app(Expr::Name(Name::definition("debug")), Expr::Var(1)),
                                                ),
                                                Expr::Var(0),
                                            ),
                                        ),
                                    ),
                                    StringPart::from("]"),
                                ]
                            )
                        ),
                    )],
                    None,
                ))),
            },
        ],
    }
}
