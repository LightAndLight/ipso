use super::{Error, Parser};
use crate::{keep_left, map2};
use ipso_diagnostic::Source;
use ipso_lex::{token, Lexer};
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
            let result = keep_left!(parser.$function(), parser.eof());
            parser.into_parse_error(result.result)
        })
    }};
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
        Err(Error::Unexpected {
            source: Source::Interactive {
                label: String::from("(parser)"),
            },
            pos: 0,
            expecting: vec![token::Name::Ident].into_iter().collect()
        })
    )
}
