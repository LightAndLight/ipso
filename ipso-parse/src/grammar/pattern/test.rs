use super::pattern;
use crate::{keep_left, map2, Parser};
use ipso_diagnostic::Source;
use ipso_lex::Lexer;
use ipso_syntax::{Pattern, Spanned};
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
fn parse_pattern_1() {
    parse_test!(
        "a",
        pattern,
        Ok(Pattern::Name(Spanned {
            pos: 0,
            item: Rc::from("a")
        }))
    )
}

#[test]
fn parse_pattern_2() {
    parse_test!("_", pattern, Ok(Pattern::Wildcard))
}
