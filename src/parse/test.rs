use std::collections::HashSet;

use crate::{
    keep_left,
    lex::{Lexer, TokenType},
    map2,
    syntax::{Declaration, Module},
};

use super::{parse_string, ParseError, Parser};

macro_rules! parse_test {
    ($input:expr, $function:ident, $output:expr) => {
        assert_eq!(
            {
                let tokens = {
                    let input = String::from($input);
                    let mut lexer = Lexer::new(&input);
                    lexer.tokenize()
                };
                let mut parser = Parser::new(tokens);
                keep_left!(parser.$function(), parser.eof())
            },
            $output
        )
    };
}

#[test]
fn parse_import_1() {
    parse_test!(
        "import yes as no",
        import,
        Ok(Declaration::Import {
            module: String::from("yes"),
            name: Some(String::from("no"))
        })
    )
}

#[test]
fn parse_import_2() {
    parse_test!(
        "import yes\n as no",
        import,
        Ok(Declaration::Import {
            module: String::from("yes"),
            name: Some(String::from("no"))
        })
    )
}

#[test]
fn parse_import_3() {
    parse_test!(
        "import yes\nas no",
        import,
        Err(ParseError::Unexpected {
            pos: 10,
            expecting: vec![TokenType::Space, TokenType::Ident(String::from("as"))]
                .into_iter()
                .collect()
        })
    )
}
