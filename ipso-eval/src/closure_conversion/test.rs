use std::rc::Rc;

use ipso_core::Binop;

use crate::closure_conversion::{self, convert};

#[test]
fn convert_1() {
    let expected = {
        use closure_conversion::Expr;

        Expr::Lam {
            env: vec![],
            arg: true,
            body: Rc::new(Expr::Var(0)),
        }
    };

    let actual = convert(&{
        use ipso_core::Expr;

        Expr::Lam {
            arg: true,
            body: Rc::new(Expr::Var(0)),
        }
    });

    assert_eq!(expected, actual)
}

#[test]
fn convert_2() {
    let expected = {
        use closure_conversion::Expr;

        Expr::Lam {
            env: vec![],
            arg: true,
            body: Rc::new(Expr::Lam {
                env: vec![0],
                arg: false,
                body: Rc::new(Expr::Var(0)),
            }),
        }
    };

    let actual = convert(&{
        use ipso_core::Expr;

        Expr::Lam {
            arg: true,
            body: Rc::new(Expr::Lam {
                arg: false,
                body: Rc::new(Expr::Var(0)),
            }),
        }
    });

    assert_eq!(expected, actual)
}

#[test]
fn convert_3() {
    let expected = {
        use closure_conversion::Expr;

        Expr::Lam {
            env: vec![],
            arg: true,
            body: Rc::new(Expr::Lam {
                env: vec![0],
                arg: true,
                body: Rc::new(Expr::Var(1)),
            }),
        }
    };

    let actual = convert(&{
        use ipso_core::Expr;

        Expr::Lam {
            arg: true,
            body: Rc::new(Expr::Lam {
                arg: true,
                body: Rc::new(Expr::Var(1)),
            }),
        }
    });

    assert_eq!(expected, actual)
}

#[test]
fn convert_4() {
    let expected = {
        use closure_conversion::Expr;

        Expr::Lam {
            env: vec![],
            arg: true,
            body: Rc::new(Expr::Lam {
                env: vec![0],
                arg: true,
                body: Rc::new(Expr::Binop(
                    Binop::Add,
                    Rc::new(Expr::Var(1)),
                    Rc::new(Expr::Var(0)),
                )),
            }),
        }
    };

    let actual = convert(&{
        use ipso_core::Expr;

        Expr::Lam {
            arg: true,
            body: Rc::new(Expr::Lam {
                arg: true,
                body: Rc::new(Expr::Binop(
                    Binop::Add,
                    Rc::new(Expr::Var(1)),
                    Rc::new(Expr::Var(0)),
                )),
            }),
        }
    });

    assert_eq!(expected, actual)
}

#[test]
fn convert_5() {
    let expected = {
        use closure_conversion::Expr;

        Expr::Lam {
            env: vec![],
            arg: true,
            body: Rc::new(Expr::Lam {
                env: vec![0],
                arg: true,
                body: Rc::new(Expr::Lam {
                    env: vec![1, 0],
                    arg: true,
                    body: Rc::new(Expr::Binop(
                        Binop::Add,
                        Rc::new(Expr::Var(2)),
                        Rc::new(Expr::Var(1)),
                    )),
                }),
            }),
        }
    };

    let actual = convert(&{
        use ipso_core::Expr;

        Expr::Lam {
            arg: true,
            body: Rc::new(Expr::Lam {
                arg: true,
                body: Rc::new(Expr::Lam {
                    arg: true,
                    body: Rc::new(Expr::Binop(
                        Binop::Add,
                        Rc::new(Expr::Var(2)),
                        Rc::new(Expr::Var(1)),
                    )),
                }),
            }),
        }
    });

    assert_eq!(expected, actual)
}

#[test]
fn convert_6() {
    let expected = {
        use closure_conversion::Expr;

        Expr::Lam {
            env: vec![],
            arg: true,
            body: Rc::new(Expr::Lam {
                env: vec![0],
                arg: true,
                body: Rc::new(Expr::Lam {
                    env: vec![1],
                    arg: true,
                    body: Rc::new(Expr::Binop(
                        Binop::Add,
                        Rc::new(Expr::Var(1)),
                        Rc::new(Expr::Var(0)),
                    )),
                }),
            }),
        }
    };

    let actual = convert(&{
        use ipso_core::Expr;

        Expr::Lam {
            arg: true,
            body: Rc::new(Expr::Lam {
                arg: true,
                body: Rc::new(Expr::Lam {
                    arg: true,
                    body: Rc::new(Expr::Binop(
                        Binop::Add,
                        Rc::new(Expr::Var(2)),
                        Rc::new(Expr::Var(0)),
                    )),
                }),
            }),
        }
    });

    assert_eq!(expected, actual)
}

#[test]
fn convert_8() {
    let expected = {
        use closure_conversion::Expr;

        Expr::Lam {
            env: vec![],
            arg: true,
            body: Rc::new(Expr::Lam {
                env: vec![],
                arg: true,
                body: Rc::new(Expr::Lam {
                    env: vec![0],
                    arg: true,
                    body: Rc::new(Expr::Binop(
                        Binop::Add,
                        Rc::new(Expr::Var(1)),
                        Rc::new(Expr::Var(0)),
                    )),
                }),
            }),
        }
    };

    let actual = convert(&{
        use ipso_core::Expr;

        Expr::Lam {
            arg: true,
            body: Rc::new(Expr::Lam {
                arg: true,
                body: Rc::new(Expr::Lam {
                    arg: true,
                    body: Rc::new(Expr::Binop(
                        Binop::Add,
                        Rc::new(Expr::Var(1)),
                        Rc::new(Expr::Var(0)),
                    )),
                }),
            }),
        }
    });

    assert_eq!(expected, actual)
}

#[test]
fn convert_9() {
    let expected = {
        use closure_conversion::{Branch, Expr, Pattern};

        // \val ->
        Expr::Lam {
            env: vec![],
            arg: true,
            // \def ->
            body: Rc::new(Expr::Lam {
                env: vec![0],
                arg: true,
                // case val of
                body: Rc::new(Expr::Case(
                    Rc::new(Expr::Var(1)),
                    vec![
                        // A x -> x
                        Branch {
                            pattern: Pattern::Variant {
                                tag: Rc::new(Expr::Int(0)),
                            },
                            body: Expr::Var(0),
                        },
                        // r -> def
                        Branch {
                            pattern: Pattern::Name,
                            body: Expr::Var(1),
                        },
                    ],
                )),
            }),
        }
    };

    let actual = convert(&{
        use ipso_core::{Branch, Expr, Pattern};

        // \val ->
        Expr::Lam {
            arg: true,
            // \def ->
            body: Rc::new(Expr::Lam {
                arg: true,
                // case val of
                body: Rc::new(Expr::Case(
                    Rc::new(Expr::Var(1)),
                    vec![
                        // A x -> x
                        Branch {
                            pattern: Pattern::Variant {
                                tag: Rc::new(Expr::Int(0)),
                            },
                            body: Expr::Var(0),
                        },
                        // r -> def
                        Branch {
                            pattern: Pattern::Name,
                            body: Expr::Var(1),
                        },
                    ],
                )),
            }),
        }
    });

    assert_eq!(expected, actual)
}

#[test]
fn convert_10() {
    let expected = {
        use closure_conversion::{Branch, Expr, Pattern, StringPart};

        // \offset ->
        Expr::Lam {
            env: vec![],
            arg: true,
            // \val ->
            body: Rc::new(Expr::Lam {
                env: vec![0],
                arg: true,
                // case val of
                body: Rc::new(Expr::Case(
                    Rc::new(Expr::Var(1)),
                    vec![
                        // A x -> A x
                        Branch {
                            pattern: Pattern::Variant {
                                tag: Rc::new(Expr::Var(1)),
                            },
                            body: Expr::App(Rc::new(Expr::Var(2)), Rc::new(Expr::Var(0))),
                        },
                        // r -> A "hello"
                        Branch {
                            pattern: Pattern::Name,
                            body: Expr::App(
                                Rc::new(Expr::Var(2)),
                                Rc::new(Expr::String(vec![StringPart::String(String::from(
                                    "hello",
                                ))])),
                            ),
                        },
                    ],
                )),
            }),
        }
    };

    let actual = convert(&{
        use ipso_core::{Branch, Expr, Pattern, StringPart};

        // \offset ->
        Expr::Lam {
            arg: true,
            // \val ->
            body: Rc::new(Expr::Lam {
                arg: true,
                // case val of
                body: Rc::new(Expr::Case(
                    Rc::new(Expr::Var(1)),
                    vec![
                        // A x -> A x
                        Branch {
                            pattern: Pattern::Variant {
                                tag: Rc::new(Expr::Var(1)),
                            },
                            body: Expr::App(Rc::new(Expr::Var(2)), Rc::new(Expr::Var(0))),
                        },
                        // r -> A "hello"
                        Branch {
                            pattern: Pattern::Name,
                            body: Expr::App(
                                Rc::new(Expr::Var(2)),
                                Rc::new(Expr::String(vec![StringPart::from("hello")])),
                            ),
                        },
                    ],
                )),
            }),
        }
    });

    assert_eq!(expected, actual)
}

#[test]
fn convert_11() {
    let expected = {
        use closure_conversion::Expr;

        // \val ->
        Expr::Lam {
            env: vec![],
            arg: true,
            // let
            body: Rc::new(Expr::Let {
                // other = ()
                value: Rc::new(Expr::Unit),
                // in val
                rest: Rc::new(Expr::Var(1)),
            }),
        }
    };

    let actual = convert(&{
        use ipso_core::Expr;

        // \val ->
        Expr::Lam {
            arg: true,
            // let
            body: Rc::new(Expr::Let {
                // other = ()
                value: Rc::new(Expr::Unit),
                // in val
                rest: Rc::new(Expr::Var(1)),
            }),
        }
    });

    assert_eq!(expected, actual)
}

#[test]
fn convert_12() {
    let expected = {
        use closure_conversion::{Expr, StringPart};
        use ipso_core::Name;
        use ipso_syntax::{ModuleId, ModuleRef};

        // \dict ->
        Expr::Lam {
            env: vec![],
            arg: true,
            // {
            body: Rc::new(Expr::Record(vec![(
                Expr::Int(0),
                // a = \arg ->
                Expr::Lam {
                    env: vec![0],
                    arg: true,
                    // array.foldl (\acc el -> "${acc} ${a dict el}") "A Array" arg
                    body: Rc::new(Expr::App(
                        Rc::new(Expr::App(
                            Rc::new(Expr::App(
                                Rc::new(Expr::Module {
                                    id: ModuleRef::Id(ModuleId::new(0)),
                                    path: vec![String::from("array")],
                                    item: Name::definition("foldl"),
                                }),
                                Rc::new(Expr::Lam {
                                    env: vec![1],
                                    arg: true,
                                    body: Rc::new(Expr::Lam {
                                        env: vec![1, 0],
                                        arg: true,
                                        body: Rc::new(Expr::String(vec![
                                            StringPart::Expr(Expr::Var(1)),
                                            StringPart::String(String::from(" ")),
                                            StringPart::Expr(Expr::App(
                                                Rc::new(Expr::App(
                                                    Rc::new(Expr::Name(Name::definition("a"))),
                                                    Rc::new(Expr::Var(2)),
                                                )),
                                                Rc::new(Expr::Var(0)),
                                            )),
                                        ])),
                                    }),
                                }),
                            )),
                            Rc::new(Expr::String(vec![StringPart::String(String::from(
                                "A Array",
                            ))])),
                        )),
                        Rc::new(Expr::Var(0)),
                    )),
                },
            )])),
        }
    };

    let actual = {
        use ipso_core::{Expr, Name, StringPart};
        use ipso_syntax::{ModuleId, ModuleRef};

        // \dict ->
        closure_conversion::convert(&Expr::Lam {
            arg: true,
            // {
            body: Rc::new(Expr::Record(vec![(
                // a = \arg ->
                Expr::Int(0),
                Expr::Lam {
                    arg: true,
                    // array.foldl (\acc el -> "${acc} ${a dict el}") "A Array" arg
                    body: Rc::new(Expr::App(
                        Rc::new(Expr::App(
                            Rc::new(Expr::App(
                                Rc::new(Expr::Module {
                                    id: ModuleRef::Id(ModuleId::new(0)),
                                    path: vec![String::from("array")],
                                    item: Name::definition("foldl"),
                                }),
                                // \acc ->
                                Rc::new(Expr::Lam {
                                    arg: true,
                                    // \el ->
                                    body: Rc::new(Expr::Lam {
                                        arg: true,
                                        // "${acc} ${a dict el}"
                                        body: Rc::new(Expr::String(vec![
                                            StringPart::Expr(Expr::Var(1)),
                                            StringPart::String(String::from(" ")),
                                            StringPart::Expr(Expr::App(
                                                Rc::new(Expr::App(
                                                    Rc::new(Expr::Name(Name::definition("a"))),
                                                    Rc::new(Expr::Var(3)),
                                                )),
                                                Rc::new(Expr::Var(0)),
                                            )),
                                        ])),
                                    }),
                                }),
                            )),
                            Rc::new(Expr::String(vec![StringPart::String(String::from(
                                "A Array",
                            ))])),
                        )),
                        Rc::new(Expr::Var(0)),
                    )),
                },
            )])),
        })
    };

    assert_eq!(expected, actual)
}

#[test]
fn convert_13() {
    let expected = {
        use closure_conversion::{Expr, StringPart};

        // let a = "a" in
        Expr::Let {
            value: Rc::new(Expr::String(vec![StringPart::from("a")])),
            // let b = "b" in
            rest: Rc::new(Expr::Let {
                value: Rc::new(Expr::String(vec![StringPart::from("b")])),
                // let c = b in
                rest: Rc::new(Expr::Let {
                    value: Rc::new(Expr::Var(0)),
                    // a
                    rest: Rc::new(Expr::Var(2)),
                }),
            }),
        }
    };

    let actual = convert(&{
        use ipso_core::{Expr, StringPart};

        // let a = "a" in
        Expr::Let {
            value: Rc::new(Expr::String(vec![StringPart::from("a")])),
            // let b = "b" in
            rest: Rc::new(Expr::Let {
                value: Rc::new(Expr::String(vec![StringPart::from("b")])),
                // let c = b in
                rest: Rc::new(Expr::Let {
                    value: Rc::new(Expr::Var(0)),
                    // a
                    rest: Rc::new(Expr::Var(2)),
                }),
            }),
        }
    });

    assert_eq!(expected, actual)
}
