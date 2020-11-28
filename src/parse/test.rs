#[cfg(test)]
use crate::{
    keep_left,
    lex::{Lexer, TokenType},
    map2,
    syntax::Declaration,
    syntax::Expr,
    syntax::{Pattern, Type},
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

#[test]
fn parse_definition_1() {
    parse_test!(
        "x : Int\nx = 1",
        definition,
        Ok(Declaration::Definition {
            name: String::from("x"),
            ty: Type::Int,
            args: Vec::new(),
            body: Expr::Int(1)
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
            body: Expr::Int(1)
        })
    )
}

#[test]
fn parse_definition_3() {
    parse_test!(
        "x : Int\nx =\n1",
        definition,
        Err(ParseError::Unexpected {
            pos: 11,
            expecting: vec![
                TokenType::Int {
                    value: 0,
                    length: 0
                },
                TokenType::Space
            ]
            .into_iter()
            .collect()
        })
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
fn parse_type_3() {
    parse_test!(
        "Eq a => a -> a -> Bool",
        type_,
        Ok(Type::mk_fatarrow(
            Type::mk_app(Type::mk_name("Eq"), Type::mk_name("a")),
            Type::mk_arrow(
                Type::mk_name("a"),
                Type::mk_arrow(Type::mk_name("a"), Type::Bool)
            ),
        ))
    )
}

#[test]
fn parse_type_4() {
    parse_test!(
        "Eq a => F => a -> Bool",
        type_,
        Ok(Type::mk_fatarrow(
            Type::mk_app(Type::mk_name("Eq"), Type::mk_name("a")),
            Type::mk_fatarrow(
                Type::mk_name("F"),
                Type::mk_arrow(Type::mk_name("a"), Type::Bool)
            )
        ))
    )
}

#[test]
fn parse_pattern_1() {
    parse_test!("a", pattern, Ok(Pattern::Name(String::from("a"))))
}

#[test]
fn parse_pattern_2() {
    parse_test!("_", pattern, Ok(Pattern::Wildcard))
}
