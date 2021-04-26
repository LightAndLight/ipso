#[cfg(test)]
use super::{Lexer, Token, TokenType};

#[test]
fn lex_int_1() {
    assert_eq!(
        {
            let input = String::from("923");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![Token {
            token_type: TokenType::Int {
                value: 923,
                length: 3
            },
            pos: 0
        },]
    )
}

#[test]
fn lex_int_2() {
    assert_eq!(
        {
            let input = String::from("00923");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![Token {
            token_type: TokenType::Int {
                value: 923,
                length: 5
            },
            pos: 0
        },]
    )
}

#[test]
fn lex_import() {
    assert_eq!(
        {
            let input = String::from("import yes as no");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![
            Token {
                token_type: TokenType::Ident(String::from("import")),
                pos: 0
            },
            Token {
                token_type: TokenType::Space,
                pos: 6
            },
            Token {
                token_type: TokenType::Ident(String::from("yes")),
                pos: 7
            },
            Token {
                token_type: TokenType::Space,
                pos: 10
            },
            Token {
                token_type: TokenType::Ident(String::from("as")),
                pos: 11
            },
            Token {
                token_type: TokenType::Space,
                pos: 13
            },
            Token {
                token_type: TokenType::Ident(String::from("no")),
                pos: 14
            }
        ]
    )
}

#[test]
fn lex_definition_1() {
    assert_eq!(
        {
            let input = String::from("x : Int\nx = 1");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![
            Token {
                token_type: TokenType::Ident(String::from("x")),
                pos: 0
            },
            Token {
                token_type: TokenType::Space,
                pos: 1
            },
            Token {
                token_type: TokenType::Colon,
                pos: 2
            },
            Token {
                token_type: TokenType::Space,
                pos: 3
            },
            Token {
                token_type: TokenType::Ident(String::from("Int")),
                pos: 4
            },
            Token {
                token_type: TokenType::Indent(0),
                pos: 7
            },
            Token {
                token_type: TokenType::Ident(String::from("x")),
                pos: 8
            },
            Token {
                token_type: TokenType::Space,
                pos: 9
            },
            Token {
                token_type: TokenType::Equals,
                pos: 10
            },
            Token {
                token_type: TokenType::Space,
                pos: 11
            },
            Token {
                token_type: TokenType::Int {
                    value: 1,
                    length: 1
                },
                pos: 12
            }
        ]
    )
}

#[test]
fn lex_definition_2() {
    assert_eq!(
        {
            let input = String::from("x : Int\nx = ~");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![
            Token {
                token_type: TokenType::Ident(String::from("x")),
                pos: 0
            },
            Token {
                token_type: TokenType::Space,
                pos: 1
            },
            Token {
                token_type: TokenType::Colon,
                pos: 2
            },
            Token {
                token_type: TokenType::Space,
                pos: 3
            },
            Token {
                token_type: TokenType::Ident(String::from("Int")),
                pos: 4
            },
            Token {
                token_type: TokenType::Indent(0),
                pos: 7
            },
            Token {
                token_type: TokenType::Ident(String::from("x")),
                pos: 8
            },
            Token {
                token_type: TokenType::Space,
                pos: 9
            },
            Token {
                token_type: TokenType::Equals,
                pos: 10
            },
            Token {
                token_type: TokenType::Space,
                pos: 11
            },
            Token {
                token_type: TokenType::Unexpected('~'),
                pos: 12
            }
        ]
    )
}

#[test]
fn lex_case_1() {
    assert_eq!(
        {
            let input = String::from("case x of\n  a -> b");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![
            Token {
                token_type: TokenType::Ident(String::from("case")),
                pos: 0
            },
            Token {
                token_type: TokenType::Space,
                pos: 4
            },
            Token {
                token_type: TokenType::Ident(String::from("x")),
                pos: 5
            },
            Token {
                token_type: TokenType::Space,
                pos: 6
            },
            Token {
                token_type: TokenType::Ident(String::from("of")),
                pos: 7
            },
            Token {
                token_type: TokenType::Indent(2),
                pos: 9
            },
            Token {
                token_type: TokenType::Ident(String::from("a")),
                pos: 12
            },
            Token {
                token_type: TokenType::Space,
                pos: 13
            },
            Token {
                token_type: TokenType::Arrow,
                pos: 14
            },
            Token {
                token_type: TokenType::Space,
                pos: 16
            },
            Token {
                token_type: TokenType::Ident(String::from("b")),
                pos: 17
            },
        ]
    )
}

#[test]
fn lex_ann_1() {
    assert_eq!(
        {
            let input = String::from("main : IO ~");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![
            Token {
                token_type: TokenType::Ident(String::from("main")),
                pos: 0
            },
            Token {
                token_type: TokenType::Space,
                pos: 4
            },
            Token {
                token_type: TokenType::Colon,
                pos: 5
            },
            Token {
                token_type: TokenType::Space,
                pos: 6
            },
            Token {
                token_type: TokenType::Ident(String::from("IO")),
                pos: 7
            },
            Token {
                token_type: TokenType::Space,
                pos: 9
            },
            Token {
                token_type: TokenType::Unexpected('~'),
                pos: 10
            },
        ]
    )
}

#[test]
fn lex_string_1() {
    let input = String::from("\"hello\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 0,
        },
        Token {
            token_type: TokenType::String {
                value: String::from("hello"),
                length: 5,
            },
            pos: 1,
        },
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 6,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}

#[test]
fn lex_string_2() {
    let input = String::from("\"x $y z\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 0,
        },
        Token {
            token_type: TokenType::String {
                value: String::from("x "),
                length: 2,
            },
            pos: 1,
        },
        Token {
            token_type: TokenType::Dollar,
            pos: 3,
        },
        Token {
            token_type: TokenType::Ident(String::from("y")),
            pos: 4,
        },
        Token {
            token_type: TokenType::String {
                value: String::from(" z"),
                length: 2,
            },
            pos: 5,
        },
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 7,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}

#[test]
fn lex_string_3() {
    let input = String::from("\"x $yy z\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 0,
        },
        Token {
            token_type: TokenType::String {
                value: String::from("x "),
                length: 2,
            },
            pos: 1,
        },
        Token {
            token_type: TokenType::Dollar,
            pos: 3,
        },
        Token {
            token_type: TokenType::Ident(String::from("yy")),
            pos: 4,
        },
        Token {
            token_type: TokenType::String {
                value: String::from(" z"),
                length: 2,
            },
            pos: 6,
        },
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 8,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}

#[test]
fn lex_string_4() {
    let input = String::from("\"x ${yy} z\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 0,
        },
        Token {
            token_type: TokenType::String {
                value: String::from("x "),
                length: 2,
            },
            pos: 1,
        },
        Token {
            token_type: TokenType::DollarLBrace,
            pos: 3,
        },
        Token {
            token_type: TokenType::Ident(String::from("yy")),
            pos: 5,
        },
        Token {
            token_type: TokenType::RBrace,
            pos: 7,
        },
        Token {
            token_type: TokenType::String {
                value: String::from(" z"),
                length: 2,
            },
            pos: 8,
        },
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 10,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}

#[test]
fn lex_string_5() {
    let input = String::from("\"x ${a + b} z\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 0,
        },
        Token {
            token_type: TokenType::String {
                value: String::from("x "),
                length: 2,
            },
            pos: 1,
        },
        Token {
            token_type: TokenType::DollarLBrace,
            pos: 3,
        },
        Token {
            token_type: TokenType::Ident(String::from("a")),
            pos: 5,
        },
        Token {
            token_type: TokenType::Space,
            pos: 6,
        },
        Token {
            token_type: TokenType::Plus,
            pos: 7,
        },
        Token {
            token_type: TokenType::Space,
            pos: 8,
        },
        Token {
            token_type: TokenType::Ident(String::from("b")),
            pos: 9,
        },
        Token {
            token_type: TokenType::RBrace,
            pos: 10,
        },
        Token {
            token_type: TokenType::String {
                value: String::from(" z"),
                length: 2,
            },
            pos: 11,
        },
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 13,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}

#[test]
fn lex_string_6() {
    let input = String::from("\"hello $name\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 0,
        },
        Token {
            token_type: TokenType::String {
                value: String::from("hello "),
                length: 6,
            },
            pos: 1,
        },
        Token {
            token_type: TokenType::Dollar,
            pos: 7,
        },
        Token {
            token_type: TokenType::Ident(String::from("name")),
            pos: 8,
        },
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 12,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}

#[test]
fn lex_string_7() {
    let input = String::from("\"hello\\n\\n\\nworld\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 0,
        },
        Token {
            token_type: TokenType::String {
                value: String::from("hello\n\n\nworld"),
                length: 16,
            },
            pos: 1,
        },
        Token {
            token_type: TokenType::DoubleQuote,
            pos: 17,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}
