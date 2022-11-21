use pretty_assertions::assert_eq;
use std::rc::Rc;

use crate::{Branch, Expr, Pattern, Spanned};

use super::desugar_expr;

#[test]
fn desugar_nested_patterns_1() {
    let x = Expr::mk_var("x");
    let ok: Rc<str> = Rc::from("Ok");
    let err_ctor: Rc<str> = Rc::from("Err");
    let err: Rc<str> = Rc::from("err");
    let fresh_0 = "desugar#0";

    /*
    case x of
      Ok desugar#0 ->
        case desugar#0 of
          _ -> 1
      Err err -> 2
    */
    let expected = Ok(Spanned {
        pos: 0,
        // case x of
        item: Expr::mk_case(
            Spanned {
                pos: 0,
                item: x.clone(),
            },
            vec![
                Branch {
                    // Ok desugar#0
                    pattern: Spanned {
                        pos: 0,
                        item: Pattern::Variant {
                            name: ok.clone(),
                            arg: Spanned {
                                pos: 0,
                                item: Box::new(Pattern::Name(Spanned {
                                    pos: 0,
                                    item: Rc::from(fresh_0),
                                })),
                            },
                        },
                    },
                    // ->
                    body: Spanned {
                        pos: 0,
                        // case desugar#0 of
                        item: Expr::mk_case(
                            Spanned {
                                pos: 0,
                                item: Expr::mk_var(fresh_0),
                            },
                            // _ -> 1
                            vec![Branch {
                                pattern: Spanned {
                                    pos: 0,
                                    item: Pattern::Wildcard,
                                },
                                body: Spanned {
                                    pos: 0,
                                    item: Expr::Int(1),
                                },
                            }],
                        ),
                    },
                },
                Branch {
                    // Err err
                    pattern: Spanned {
                        pos: 0,
                        item: Pattern::Variant {
                            name: err_ctor.clone(),
                            arg: Spanned {
                                pos: 0,
                                item: Box::new(Pattern::Name(Spanned {
                                    pos: 0,
                                    item: err.clone(),
                                })),
                            },
                        },
                    },
                    // -> 2
                    body: Spanned {
                        pos: 0,
                        item: Expr::Int(2),
                    },
                },
            ],
        ),
    });

    /*
    case x of
      Ok _ -> 1
      Err err -> 2
    */
    let expr = Expr::mk_case(
        Spanned { pos: 0, item: x },
        vec![
            Branch {
                // Ok _
                pattern: Spanned {
                    pos: 0,
                    item: Pattern::Variant {
                        name: ok,
                        arg: Spanned {
                            pos: 0,
                            item: Box::new(Pattern::Wildcard),
                        },
                    },
                },
                // -> 1
                body: Spanned {
                    pos: 0,
                    item: Expr::Int(1),
                },
            },
            Branch {
                // Err err
                pattern: Spanned {
                    pos: 0,
                    item: Pattern::Variant {
                        name: err_ctor,
                        arg: Spanned {
                            pos: 0,
                            item: Box::new(Pattern::Name(Spanned { pos: 0, item: err })),
                        },
                    },
                },
                // -> 2
                body: Spanned {
                    pos: 0,
                    item: Expr::Int(2),
                },
            },
        ],
    );
    let actual = desugar_expr(
        &ipso_diagnostic::Source::Interactive {
            label: String::from("test"),
        },
        Spanned { pos: 0, item: expr },
    );

    assert_eq!(expected, actual)
}
