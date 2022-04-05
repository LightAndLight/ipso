#[cfg(test)]
use super::{definition, from_import, import, type_alias};
#[cfg(test)]
use crate::{keep_left, map2, ParseError, Parser};
#[cfg(test)]
use ipso_diagnostic::Source;
#[cfg(test)]
use ipso_lex::{
    token::{self, Relation},
    Lexer,
};
#[cfg(test)]
use ipso_syntax::{r#type::Type, Declaration, Expr, Names, Pattern, Spanned};
#[cfg(test)]
use std::rc::Rc;

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
fn parse_import_1() {
    parse_test!(
        "import yes",
        import,
        Ok(Declaration::Import {
            resolved: None,
            module: Spanned {
                pos: 7,
                item: String::from("yes")
            },
            as_name: None
        })
    )
}

#[test]
fn parse_import_as_1() {
    parse_test!(
        "import yes as no",
        import,
        Ok(Declaration::Import {
            resolved: None,
            module: Spanned {
                pos: 7,
                item: String::from("yes")
            },
            as_name: Some(Spanned {
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
            resolved: None,
            module: Spanned {
                pos: 7,
                item: String::from("yes")
            },
            as_name: Some(Spanned {
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
            source: Source::Interactive {
                label: String::from("(parser)"),
            },
            pos: 11,
            expecting: vec![
                token::Name::Indent(Relation::Gt, 0),
                token::Name::Comment,
                token::Name::Eof
            ]
            .into_iter()
            .collect()
        })
    )
}

#[test]
fn parse_definition_1() {
    parse_test!(
        /*
        x : Int
        x = 1
        */
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
            source: Source::Interactive {
                label: String::from("(parser)"),
            },
            pos: 12,
            expecting: vec![token::Name::Indent(Relation::Gt, 0), token::Name::Comment]
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
                Spanned {
                    pos: 10,
                    item: Pattern::Name(Spanned {
                        pos: 10,
                        item: String::from("y")
                    })
                },
                Spanned {
                    pos: 12,
                    item: Pattern::Wildcard
                }
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
            resolved: None,
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
            resolved: None,
            module: Spanned {
                pos: 5,
                item: String::from("asdf")
            },
            names: Names::Names(vec![
                Spanned {
                    pos: 17,
                    item: String::from("b")
                },
                Spanned {
                    pos: 20,
                    item: String::from("c")
                },
                Spanned {
                    pos: 23,
                    item: String::from("d")
                }
            ])
        })
    )
}
