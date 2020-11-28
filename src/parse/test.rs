#[cfg(test)]
use crate::{
    keep_left,
    lex::{Lexer, TokenType},
    map2,
    syntax::Declaration,
};

#[cfg(test)]
use super::{ParseError, Parser};

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
                let mut parser = Parser::new(tokens);
                keep_left!(parser.$function(), parser.eof())
            },
            $output
        )
    };
}

#[test]
fn parse_ident_1() {
    parse_test!("hello", ident, Ok(String::from("hello")))
}

#[test]
fn parse_ident_2() {
    parse_test!(
        "import",
        ident,
        Err(ParseError::Unexpected {
            pos: 0,
            expecting: vec![TokenType::Ident(String::from(""))]
                .into_iter()
                .collect()
        })
    )
}

#[test]
fn parse_import_1() {
    parse_test!(
        "import yes",
        import,
        Ok(Declaration::Import {
            module: String::from("yes"),
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
            module: String::from("yes"),
            name: Some(String::from("no"))
        })
    )
}

#[test]
fn parse_import_as_2() {
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
fn parse_import_as_3() {
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
