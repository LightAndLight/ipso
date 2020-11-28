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
