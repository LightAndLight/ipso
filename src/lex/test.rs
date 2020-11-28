#[cfg(test)]
use super::{Lexer, Token, TokenType};

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
