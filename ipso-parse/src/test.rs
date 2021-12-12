#[cfg(test)]
use super::{ParseError, Parser};
#[cfg(test)]
use crate::{keep_left, map2};
#[cfg(test)]
use ipso_diagnostic::Source;
#[cfg(test)]
use ipso_lex::{token, Lexer};
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
        Err(ParseError::Unexpected {
            source: Source::Interactive {
                label: String::from("(parser)"),
            },
            pos: 0,
            expecting: vec![token::Name::Ident].into_iter().collect()
        })
    )
}
