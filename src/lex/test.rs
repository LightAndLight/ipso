#[cfg(test)]
use std::rc::Rc;

#[cfg(test)]
use super::Lexer;

#[cfg(test)]
use crate::token::{self, Token};

#[test]
fn lex_int_1() {
    assert_eq!(
        {
            let input = Rc::from("923");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![Token {
            data: token::Data::Int {
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
            let input = Rc::from("00923");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![Token {
            data: token::Data::Int {
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
            let input = Rc::from("import yes as no");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![
            Token {
                data: token::Data::Ident(Rc::from("import")),
                pos: 0
            },
            Token {
                data: token::Data::Space,
                pos: 6
            },
            Token {
                data: token::Data::Ident(Rc::from("yes")),
                pos: 7
            },
            Token {
                data: token::Data::Space,
                pos: 10
            },
            Token {
                data: token::Data::Ident(Rc::from("as")),
                pos: 11
            },
            Token {
                data: token::Data::Space,
                pos: 13
            },
            Token {
                data: token::Data::Ident(Rc::from("no")),
                pos: 14
            }
        ]
    )
}

#[test]
fn lex_definition_1() {
    assert_eq!(
        {
            let input = Rc::from("x : Int\nx = 1");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![
            Token {
                data: token::Data::Ident(Rc::from("x")),
                pos: 0
            },
            Token {
                data: token::Data::Space,
                pos: 1
            },
            Token {
                data: token::Data::Colon,
                pos: 2
            },
            Token {
                data: token::Data::Space,
                pos: 3
            },
            Token {
                data: token::Data::Ident(Rc::from("Int")),
                pos: 4
            },
            Token {
                data: token::Data::Indent(0),
                pos: 7
            },
            Token {
                data: token::Data::Ident(Rc::from("x")),
                pos: 8
            },
            Token {
                data: token::Data::Space,
                pos: 9
            },
            Token {
                data: token::Data::Equals,
                pos: 10
            },
            Token {
                data: token::Data::Space,
                pos: 11
            },
            Token {
                data: token::Data::Int {
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
            let input = Rc::from("x : Int\nx = ~");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![
            Token {
                data: token::Data::Ident(Rc::from("x")),
                pos: 0
            },
            Token {
                data: token::Data::Space,
                pos: 1
            },
            Token {
                data: token::Data::Colon,
                pos: 2
            },
            Token {
                data: token::Data::Space,
                pos: 3
            },
            Token {
                data: token::Data::Ident(Rc::from("Int")),
                pos: 4
            },
            Token {
                data: token::Data::Indent(0),
                pos: 7
            },
            Token {
                data: token::Data::Ident(Rc::from("x")),
                pos: 8
            },
            Token {
                data: token::Data::Space,
                pos: 9
            },
            Token {
                data: token::Data::Equals,
                pos: 10
            },
            Token {
                data: token::Data::Space,
                pos: 11
            },
            Token {
                data: token::Data::Unexpected('~'),
                pos: 12
            }
        ]
    )
}

#[test]
fn lex_case_1() {
    assert_eq!(
        {
            let input = Rc::from("case x of\n  a -> b");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![
            Token {
                data: token::Data::Ident(Rc::from("case")),
                pos: 0
            },
            Token {
                data: token::Data::Space,
                pos: 4
            },
            Token {
                data: token::Data::Ident(Rc::from("x")),
                pos: 5
            },
            Token {
                data: token::Data::Space,
                pos: 6
            },
            Token {
                data: token::Data::Ident(Rc::from("of")),
                pos: 7
            },
            Token {
                data: token::Data::Indent(2),
                pos: 9
            },
            Token {
                data: token::Data::Ident(Rc::from("a")),
                pos: 12
            },
            Token {
                data: token::Data::Space,
                pos: 13
            },
            Token {
                data: token::Data::Arrow,
                pos: 14
            },
            Token {
                data: token::Data::Space,
                pos: 16
            },
            Token {
                data: token::Data::Ident(Rc::from("b")),
                pos: 17
            },
        ]
    )
}

#[test]
fn lex_ann_1() {
    assert_eq!(
        {
            let input = Rc::from("main : IO ~");
            let lexer = Lexer::new(&input);
            lexer.tokenize()
        },
        vec![
            Token {
                data: token::Data::Ident(Rc::from("main")),
                pos: 0
            },
            Token {
                data: token::Data::Space,
                pos: 4
            },
            Token {
                data: token::Data::Colon,
                pos: 5
            },
            Token {
                data: token::Data::Space,
                pos: 6
            },
            Token {
                data: token::Data::Ident(Rc::from("IO")),
                pos: 7
            },
            Token {
                data: token::Data::Space,
                pos: 9
            },
            Token {
                data: token::Data::Unexpected('~'),
                pos: 10
            },
        ]
    )
}

#[test]
fn lex_string_1() {
    let input = Rc::from("\"hello\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            data: token::Data::DoubleQuote,
            pos: 0,
        },
        Token {
            data: token::Data::String {
                value: String::from("hello"),
                length: 5,
            },
            pos: 1,
        },
        Token {
            data: token::Data::DoubleQuote,
            pos: 6,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}

#[test]
fn lex_string_2() {
    let input = Rc::from("\"x $y z\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            data: token::Data::DoubleQuote,
            pos: 0,
        },
        Token {
            data: token::Data::String {
                value: String::from("x "),
                length: 2,
            },
            pos: 1,
        },
        Token {
            data: token::Data::Dollar,
            pos: 3,
        },
        Token {
            data: token::Data::Ident(Rc::from("y")),
            pos: 4,
        },
        Token {
            data: token::Data::String {
                value: String::from(" z"),
                length: 2,
            },
            pos: 5,
        },
        Token {
            data: token::Data::DoubleQuote,
            pos: 7,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}

#[test]
fn lex_string_3() {
    let input = Rc::from("\"x $yy z\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            data: token::Data::DoubleQuote,
            pos: 0,
        },
        Token {
            data: token::Data::String {
                value: String::from("x "),
                length: 2,
            },
            pos: 1,
        },
        Token {
            data: token::Data::Dollar,
            pos: 3,
        },
        Token {
            data: token::Data::Ident(Rc::from("yy")),
            pos: 4,
        },
        Token {
            data: token::Data::String {
                value: String::from(" z"),
                length: 2,
            },
            pos: 6,
        },
        Token {
            data: token::Data::DoubleQuote,
            pos: 8,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}

#[test]
fn lex_string_4() {
    let input = Rc::from("\"x ${yy} z\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            data: token::Data::DoubleQuote,
            pos: 0,
        },
        Token {
            data: token::Data::String {
                value: String::from("x "),
                length: 2,
            },
            pos: 1,
        },
        Token {
            data: token::Data::DollarLBrace,
            pos: 3,
        },
        Token {
            data: token::Data::Ident(Rc::from("yy")),
            pos: 5,
        },
        Token {
            data: token::Data::RBrace,
            pos: 7,
        },
        Token {
            data: token::Data::String {
                value: String::from(" z"),
                length: 2,
            },
            pos: 8,
        },
        Token {
            data: token::Data::DoubleQuote,
            pos: 10,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}

#[test]
fn lex_string_5() {
    let input = Rc::from("\"x ${a + b} z\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            data: token::Data::DoubleQuote,
            pos: 0,
        },
        Token {
            data: token::Data::String {
                value: String::from("x "),
                length: 2,
            },
            pos: 1,
        },
        Token {
            data: token::Data::DollarLBrace,
            pos: 3,
        },
        Token {
            data: token::Data::Ident(Rc::from("a")),
            pos: 5,
        },
        Token {
            data: token::Data::Space,
            pos: 6,
        },
        Token {
            data: token::Data::Plus,
            pos: 7,
        },
        Token {
            data: token::Data::Space,
            pos: 8,
        },
        Token {
            data: token::Data::Ident(Rc::from("b")),
            pos: 9,
        },
        Token {
            data: token::Data::RBrace,
            pos: 10,
        },
        Token {
            data: token::Data::String {
                value: String::from(" z"),
                length: 2,
            },
            pos: 11,
        },
        Token {
            data: token::Data::DoubleQuote,
            pos: 13,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}

#[test]
fn lex_string_6() {
    let input = Rc::from("\"hello $name\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            data: token::Data::DoubleQuote,
            pos: 0,
        },
        Token {
            data: token::Data::String {
                value: String::from("hello "),
                length: 6,
            },
            pos: 1,
        },
        Token {
            data: token::Data::Dollar,
            pos: 7,
        },
        Token {
            data: token::Data::Ident(Rc::from("name")),
            pos: 8,
        },
        Token {
            data: token::Data::DoubleQuote,
            pos: 12,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}

#[test]
fn lex_string_7() {
    let input = Rc::from("\"hello\\n\\n\\nworld\"");
    let lexer = Lexer::new(&input);
    let expected = vec![
        Token {
            data: token::Data::DoubleQuote,
            pos: 0,
        },
        Token {
            data: token::Data::String {
                value: String::from("hello\n\n\nworld"),
                length: 16,
            },
            pos: 1,
        },
        Token {
            data: token::Data::DoubleQuote,
            pos: 17,
        },
    ];
    let actual = lexer.tokenize();
    assert_eq!(expected, actual)
}
