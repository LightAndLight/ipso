#[cfg(test)]
use super::{expr_app, expr_case, string};
#[cfg(test)]
use crate::ParseError;
#[cfg(test)]
use crate::{keep_left, map2, Parser};
#[cfg(test)]
use ipso_diagnostic::Source;
#[cfg(test)]
use ipso_lex::{
    token::{self, Relation},
    Lexer,
};
#[cfg(test)]
use ipso_syntax::{Branch, Expr, Keyword, Pattern, Spanned, StringPart};

#[cfg(test)]
macro_rules! parse_test {
    ($input:expr, $function:ident, $output:expr) => {{
        assert_eq!($output, {
            let input = String::from($input);
            let mut parser = Parser::new(
                Source::Interactive {
                    label: String::from("(parser)"),
                },
                Lexer::new(&input),
            );
            let result = keep_left!($function(&mut parser), parser.eof());
            parser.into_parse_error(result.result)
        })
    }};
}

#[test]
fn parse_app_1() {
    parse_test!(
        "a b c",
        expr_app,
        Ok(Expr::mk_app(
            Expr::mk_app(
                Spanned {
                    pos: 0,
                    item: Expr::mk_var("a")
                },
                Spanned {
                    pos: 2,
                    item: Expr::mk_var("b")
                }
            ),
            Spanned {
                pos: 4,
                item: Expr::mk_var("c")
            }
        ))
    )
}

#[test]
fn parse_app_2() {
    parse_test!(
        "pure ()",
        expr_app,
        Ok(Expr::mk_app(
            Spanned {
                pos: 0,
                item: Expr::mk_var("pure")
            },
            Spanned {
                pos: 5,
                item: Expr::Unit
            }
        ))
    )
}

#[test]
fn parse_case_1() {
    parse_test!(
        /*
        case x of
          a -> b
        */
        "case x of\n  a -> b",
        expr_case,
        Ok(Spanned {
            pos: 0,
            item: Expr::mk_case(
                Spanned {
                    pos: 5,
                    item: Expr::mk_var("x")
                },
                vec![Branch {
                    pattern: Spanned {
                        pos: 12,
                        item: Pattern::Name(Spanned {
                            pos: 12,
                            item: String::from("a")
                        })
                    },
                    body: Spanned {
                        pos: 17,
                        item: Expr::mk_var("b")
                    }
                }]
            )
        })
    )
}

#[test]
fn parse_case_2() {
    parse_test!(
        "case x of\n  a -> b\n  c -> d",
        expr_case,
        Ok(Spanned {
            pos: 0,
            item: Expr::mk_case(
                Spanned {
                    pos: 5,
                    item: Expr::mk_var("x")
                },
                vec![
                    Branch {
                        pattern: Spanned {
                            pos: 12,
                            item: Pattern::Name(Spanned {
                                pos: 12,
                                item: String::from("a")
                            })
                        },
                        body: Spanned {
                            pos: 17,
                            item: Expr::mk_var("b")
                        }
                    },
                    Branch {
                        pattern: Spanned {
                            pos: 21,
                            item: Pattern::Name(Spanned {
                                pos: 21,
                                item: String::from("c")
                            })
                        },
                        body: Spanned {
                            pos: 26,
                            item: Expr::mk_var("d")
                        }
                    }
                ]
            )
        })
    )
}

#[test]
fn parse_case_3() {
    parse_test!(
        /*
        case x of
          a ->
            b
          c -> d
        */
        "case x of\n  a ->\n    b\n  c -> d",
        expr_case,
        Ok(Spanned {
            pos: 0,
            item: Expr::mk_case(
                Spanned {
                    pos: 5,
                    item: Expr::mk_var("x")
                },
                vec![
                    Branch {
                        pattern: Spanned {
                            pos: 12,
                            item: Pattern::Name(Spanned {
                                pos: 12,
                                item: String::from("a")
                            })
                        },
                        body: Spanned {
                            pos: 21,
                            item: Expr::mk_var("b")
                        }
                    },
                    Branch {
                        pattern: Spanned {
                            pos: 25,
                            item: Pattern::Name(Spanned {
                                pos: 25,
                                item: String::from("c")
                            })
                        },
                        body: Spanned {
                            pos: 30,
                            item: Expr::mk_var("d")
                        }
                    }
                ]
            )
        })
    )
}

#[test]
fn parse_case_4() {
    parse_test!(
        /*
        case x of
          a -> b
           c -> d
         */
        "case x of\n  a -> b\n   c -> d",
        expr_case,
        Err(ParseError::Unexpected {
            source: Source::Interactive {
                label: String::from("(parser)"),
            },
            pos: 24,
            expecting: vec![
                token::Name::LAngle,
                token::Name::LParen,
                token::Name::LBrace,
                token::Name::LBracket,
                token::Name::Dot,
                token::Name::Ctor,
                token::Name::Keyword(Keyword::True),
                token::Name::Keyword(Keyword::False),
                token::Name::Ident,
                token::Name::DoubleQuote,
                token::Name::SingleQuote,
                token::Name::Int,
                token::Name::Indent(Relation::Eq, 2),
                token::Name::Comment,
                token::Name::Eof,
            ]
            .into_iter()
            .collect()
        })
    )
}

#[test]
fn parse_case_5() {
    parse_test!(
        /*
        case x of
          a -> b
         c -> d
         */
        "case x of\n  a -> b\n c -> d",
        expr_case,
        Err(ParseError::Unexpected {
            source: Source::Interactive {
                label: String::from("(parser)"),
            },
            pos: 20,
            expecting: vec![
                token::Name::Indent(Relation::Eq, 2),
                token::Name::Indent(Relation::Gt, 2),
                token::Name::Comment,
                token::Name::Eof
            ]
            .into_iter()
            .collect()
        })
    )
}

#[test]
fn parse_string_1() {
    parse_test!(
        "\"hello\"",
        string,
        Ok(vec![StringPart::String(String::from("hello"))])
    )
}

#[test]
fn parse_string_2() {
    parse_test!(
        "\"hello $name\"",
        string,
        Ok(vec![
            StringPart::String(String::from("hello ")),
            StringPart::Expr(Spanned {
                pos: 8,
                item: Expr::Var(String::from("name"))
            })
        ])
    )
}

#[test]
fn parse_string_3() {
    parse_test!(
        "\"a ${b c} d\"",
        string,
        Ok(vec![
            StringPart::String(String::from("a ")),
            StringPart::Expr(Expr::mk_app(
                Spanned {
                    pos: 5,
                    item: Expr::Var(String::from("b"))
                },
                Spanned {
                    pos: 7,
                    item: Expr::Var(String::from("c"))
                }
            )),
            StringPart::String(String::from(" d")),
        ])
    )
}
