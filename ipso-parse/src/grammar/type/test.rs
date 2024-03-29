use super::type_;
use crate::{keep_left, map2, Parser};
use ipso_diagnostic::Source;
use ipso_lex::Lexer;
use ipso_syntax::{Spanned, Type};
use std::rc::Rc;

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
fn parse_type_1() {
    parse_test!("Int", type_, Ok(Type::Int))
}

#[test]
fn parse_type_2() {
    parse_test!(
        "Int -> Bool",
        type_,
        Ok(Type::Function(
            Rc::new(Spanned {
                pos: 0,
                item: Type::Int
            }),
            Rc::new(Spanned {
                pos: 7,
                item: Type::Bool
            })
        ))
    )
}

#[test]
fn parse_type_2_1() {
    parse_test!(
        "Int -> Bool -> Int",
        type_,
        Ok(Type::Function(
            Rc::new(Spanned {
                pos: 0,
                item: Type::Int
            }),
            Rc::new(Spanned {
                pos: 7,
                item: Type::Function(
                    Rc::new(Spanned {
                        pos: 7,
                        item: Type::Bool
                    }),
                    Rc::new(Spanned {
                        pos: 15,
                        item: Type::Int
                    })
                )
            })
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
            Type::Function(
                Rc::new(Spanned {
                    pos: 8,
                    item: Type::Var(Rc::from("a"))
                }),
                Rc::new(Spanned {
                    pos: 13,
                    item: Type::Function(
                        Rc::new(Spanned {
                            pos: 13,
                            item: Type::Var(Rc::from("a"))
                        }),
                        Rc::new(Spanned {
                            pos: 18,
                            item: Type::Bool
                        })
                    )
                })
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
                Type::Function(
                    Rc::new(Spanned {
                        pos: 13,
                        item: Type::Var(Rc::from("a"))
                    }),
                    Rc::new(Spanned {
                        pos: 18,
                        item: Type::Bool
                    })
                )
            )
        ))
    )
}

#[test]
fn parse_type_5() {
    parse_test!("IO ()", type_, Ok(Type::mk_app(Type::IO, Type::Unit)))
}
