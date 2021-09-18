#[cfg(test)]
use crate::syntax::{Spanned, StringPart};
#[cfg(test)]
use crate::{
    diagnostic::InputLocation,
    keep_left,
    lex::{Lexer, TokenType},
    map2,
    syntax::{Branch, Declaration, Expr, Names, Pattern, Type},
};
#[cfg(test)]
use std::rc::Rc;

#[cfg(test)]
use super::{ParseError, ParseResult, Parser};

#[cfg(test)]
macro_rules! parse_test {
    ($input:expr, $function:ident, $output:expr) => {
        assert_eq!(
            {
                let tokens = {
                    let input = String::from($input);
                    let lexer = Lexer::new(&input);
                    lexer.tokenize()
                };
                let mut parser = Parser::new(
                    InputLocation::Interactive {
                        label: String::from("(parser)"),
                    },
                    tokens,
                );
                let result = keep_left!(parser.$function(), parser.eof());
                parser.into_parse_error(result.result)
            },
            $output
        )
    };
}

#[test]
fn parse_ident_1() {
    parse_test!("hello", ident, Ok(Rc::from("hello")))
}

#[test]
fn parse_ident_2() {
    parse_test!(
        "import",
        ident,
        Err(ParseError::Unexpected {
            location: InputLocation::Interactive {
                label: String::from("(parser)"),
            },
            pos: 0,
            expecting: vec![TokenType::Ident(Rc::from(""))].into_iter().collect()
        })
    )
}

#[test]
fn parse_import_1() {
    parse_test!(
        "import yes",
        import,
        Ok(Declaration::Import {
            module: Spanned {
                pos: 7,
                item: String::from("yes")
            },
            name: None
        })
    )
}

#[test]
fn parse_import_as_1() {
    parse_test!(
        "import yes as no",
        import,
        Ok(Declaration::Import {
            module: Spanned {
                pos: 7,
                item: String::from("yes")
            },
            name: Some(Spanned {
                pos: 14,
                item: String::from("no")
            })
        })
    )
}

#[test]
fn parse_import_as_2() {
    parse_test!(
        "import yes\n as no",
        import,
        Ok(Declaration::Import {
            module: Spanned {
                pos: 7,
                item: String::from("yes")
            },
            name: Some(Spanned {
                pos: 15,
                item: String::from("no")
            })
        })
    )
}

#[test]
fn parse_import_as_3() {
    parse_test!(
        /*
        import yes
        as no
         */
        "import yes\nas no",
        import,
        Err(ParseError::Unexpected {
            location: InputLocation::Interactive {
                label: String::from("(parser)"),
            },
            pos: 10,
            expecting: vec![
                TokenType::Space,
                TokenType::Ident(Rc::from("as")),
                TokenType::Comment { length: 0 }
            ]
            .into_iter()
            .collect()
        })
    )
}

#[test]
fn parse_definition_1() {
    parse_test!(
        "x : Int\nx = 1",
        definition,
        Ok(Declaration::Definition {
            name: String::from("x"),
            ty: Type::Int,
            args: Vec::new(),
            body: Spanned {
                pos: 12,
                item: Expr::Int(1)
            }
        })
    )
}

#[test]
fn parse_definition_2() {
    parse_test!(
        "x : Int\nx =\n  1",
        definition,
        Ok(Declaration::Definition {
            name: String::from("x"),
            ty: Type::Int,
            args: Vec::new(),
            body: Spanned {
                pos: 14,
                item: Expr::Int(1)
            }
        })
    )
}

#[test]
fn parse_definition_3() {
    parse_test!(
        /*
        x : Int
        x =
        1
         */
        "x : Int\nx =\n1",
        definition,
        Err(ParseError::Unexpected {
            location: InputLocation::Interactive {
                label: String::from("(parser)"),
            },
            pos: 11,
            expecting: vec![
                TokenType::Int {
                    value: 0,
                    length: 0
                },
                TokenType::Ident(Rc::from("")),
                TokenType::Ident(Rc::from("case")),
                TokenType::Ident(Rc::from("true")),
                TokenType::Ident(Rc::from("false")),
                TokenType::Ident(Rc::from("if")),
                TokenType::Ctor,
                TokenType::Space,
                TokenType::LAngle,
                TokenType::LParen,
                TokenType::LBrace,
                TokenType::LBracket,
                TokenType::DoubleQuote,
                TokenType::SingleQuote,
                TokenType::Backslash,
                TokenType::Comment { length: 0 },
            ]
            .into_iter()
            .collect()
        })
    )
}

#[test]
fn parse_definition_4() {
    parse_test!(
        "x : Int\nx y _ = 1",
        definition,
        Ok(Declaration::Definition {
            name: String::from("x"),
            ty: Type::Int,
            args: vec![
                Pattern::Name(Spanned {
                    pos: 10,
                    item: String::from("y")
                }),
                Pattern::Wildcard
            ],
            body: Spanned {
                pos: 16,
                item: Expr::Int(1)
            }
        })
    )
}

#[test]
fn parse_definition_5() {
    parse_test!(
        "main : IO ()\nmain = pure ()",
        definition,
        Ok(Declaration::Definition {
            name: String::from("main"),
            ty: Type::mk_app(Type::IO, Type::Unit),
            args: vec![],
            body: Expr::mk_app(
                Spanned {
                    pos: 20,
                    item: Expr::mk_var("pure")
                },
                Spanned {
                    pos: 25,
                    item: Expr::Unit
                }
            )
        })
    )
}

#[test]
fn parse_type_alias_1() {
    parse_test!(
        "type Hello = ()",
        type_alias,
        Ok(Declaration::TypeAlias {
            name: String::from("Hello"),
            args: vec![],
            body: Type::Unit
        })
    )
}

#[test]
fn parse_type_alias_2() {
    parse_test!(
        "type Ap a b = a b",
        type_alias,
        Ok(Declaration::TypeAlias {
            name: String::from("Ap"),
            args: vec![String::from("a"), String::from("b")],
            body: Type::mk_app(Type::Var(Rc::from("a")), Type::Var(Rc::from("b")))
        })
    )
}

#[test]
fn parse_from_import_1() {
    parse_test!(
        "from asdf import *",
        from_import,
        Ok(Declaration::FromImport {
            module: Spanned {
                pos: 5,
                item: String::from("asdf")
            },
            names: Names::All
        })
    )
}

#[test]
fn parse_from_import_2() {
    parse_test!(
        "from asdf import b, c, d",
        from_import,
        Ok(Declaration::FromImport {
            module: Spanned {
                pos: 5,
                item: String::from("asdf")
            },
            names: Names::Names(vec![
                String::from("b"),
                String::from("c"),
                String::from("d")
            ])
        })
    )
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
fn parse_type_1() {
    parse_test!("Int", type_, Ok(Type::Int))
}

#[test]
fn parse_type_2() {
    parse_test!(
        "Int -> Bool",
        type_,
        Ok(Type::mk_app(
            Type::mk_app(Type::Arrow, Type::Int),
            Type::Bool
        ))
    )
}

#[test]
fn parse_type_2_1() {
    parse_test!(
        "Int -> Bool -> Int",
        type_,
        Ok(Type::mk_arrow(
            Type::Int,
            Type::mk_arrow(Type::Bool, Type::Int)
        ))
    )
}

#[test]
fn parse_type_3() {
    parse_test!(
        "Eq a => a -> a -> Bool",
        type_,
        Ok(Type::mk_fatarrow(
            Type::mk_app(Type::mk_name("Eq"), Type::Var(Rc::from("a"))),
            Type::mk_arrow(
                Type::Var(Rc::from("a")),
                Type::mk_arrow(Type::Var(Rc::from("a")), Type::Bool)
            ),
        ))
    )
}

#[test]
fn parse_type_3_1() {
    parse_test!(
        "Eq a => a",
        type_,
        Ok(Type::mk_fatarrow(
            Type::mk_app(Type::mk_name("Eq"), Type::Var(Rc::from("a"))),
            Type::Var(Rc::from("a")),
        ))
    )
}

#[test]
fn parse_type_4() {
    parse_test!(
        "Eq a => F => a -> Bool",
        type_,
        Ok(Type::mk_fatarrow(
            Type::mk_app(Type::mk_name("Eq"), Type::Var(Rc::from("a"))),
            Type::mk_fatarrow(
                Type::mk_name("F"),
                Type::mk_arrow(Type::Var(Rc::from("a")), Type::Bool)
            )
        ))
    )
}

#[test]
fn parse_type_5() {
    parse_test!("IO ()", type_, Ok(Type::mk_app(Type::IO, Type::Unit)))
}

#[test]
fn parse_pattern_1() {
    parse_test!(
        "a",
        pattern,
        Ok(Pattern::Name(Spanned {
            pos: 0,
            item: String::from("a")
        }))
    )
}

#[test]
fn parse_pattern_2() {
    parse_test!("_", pattern, Ok(Pattern::Wildcard))
}

#[test]
fn parse_case_1() {
    parse_test!(
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
            location: InputLocation::Interactive {
                label: String::from("(parser)"),
            },
            pos: 24,
            expecting: vec![
                TokenType::Space,
                TokenType::Indent(2),
                TokenType::Dedent,
                TokenType::LAngle,
                TokenType::LParen,
                TokenType::LBrace,
                TokenType::LBracket,
                TokenType::Dot,
                TokenType::Ctor,
                TokenType::Ident(Rc::from("true")),
                TokenType::Ident(Rc::from("false")),
                TokenType::Ident(Rc::from("")),
                TokenType::DoubleQuote,
                TokenType::SingleQuote,
                TokenType::Int {
                    value: 0,
                    length: 0
                },
                TokenType::Comment { length: 0 }
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
            location: InputLocation::Interactive {
                label: String::from("(parser)"),
            },
            pos: 18,
            expecting: vec![
                TokenType::Ctor,
                TokenType::Ident(Rc::from("")),
                TokenType::Ident(Rc::from("false")),
                TokenType::Ident(Rc::from("true")),
                TokenType::Int {
                    value: 0,
                    length: 0
                },
                TokenType::DoubleQuote,
                TokenType::SingleQuote,
                TokenType::LBrace,
                TokenType::LParen,
                TokenType::LBracket,
                TokenType::LAngle,
                TokenType::Dot,
                TokenType::Indent(2),
                TokenType::Dedent,
                TokenType::Space,
                TokenType::Comment { length: 0 }
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
