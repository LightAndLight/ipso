use std::str::Chars;

pub struct Span {
    pub pos: usize,
    pub length: usize,
}

#[derive(PartialEq, Eq)]
pub enum TokenType {
    Ident(String),
    Ctor(String),

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,

    Arrow,
    FatArrow,

    Dot,

    Import,
    From,
    As,
    Asterisk,

    If,
    Then,
    Else,

    True,
    False,

    Equals,
    Type,
    Colon,

    Comma,

    Case,
    Of,
    Underscore,

    Hyphen,
    Plus,
    Slash,

    Indent,
    Dedent,
}

pub struct Token {
    pub tokenType: TokenType,
    pub span: Span,
}

pub struct Lexer<'input> {
    pos: usize,
    current: Option<char>,
    input: Chars<'input>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &String) -> Lexer {
        let mut input = input.chars();
        Lexer {
            pos: 0,
            current: input.next(),
            input,
        }
    }

    fn consume(&mut self) {
        match self.input.next() {
            None => {}
            Some(c) => {
                self.pos += 1;
                self.current = Some(c);
            }
        }
    }

    fn next(&mut self) -> Option<Token> {
        let pos = self.pos;
        match self.current {
            None => None,
            Some(c) => match c {
                '{' => {
                    self.consume();
                    Some(Token {
                        tokenType: TokenType::LBrace,
                        span: Span { pos, length: 1 },
                    })
                }
                '}' => {
                    self.consume();
                    Some(Token {
                        tokenType: TokenType::RBrace,
                        span: Span { pos, length: 1 },
                    })
                }
                '(' => {
                    self.consume();
                    Some(Token {
                        tokenType: TokenType::LParen,
                        span: Span { pos, length: 1 },
                    })
                }
                ')' => {
                    self.consume();
                    Some(Token {
                        tokenType: TokenType::RParen,
                        span: Span { pos, length: 1 },
                    })
                }
                '[' => {
                    self.consume();
                    Some(Token {
                        tokenType: TokenType::LBracket,
                        span: Span { pos, length: 1 },
                    })
                }
                ']' => {
                    self.consume();
                    Some(Token {
                        tokenType: TokenType::RBracket,
                        span: Span { pos, length: 1 },
                    })
                }
                ',' => {
                    self.consume();
                    Some(Token {
                        tokenType: TokenType::Comma,
                        span: Span { pos, length: 1 },
                    })
                }
                ':' => {
                    self.consume();
                    Some(Token {
                        tokenType: TokenType::Colon,
                        span: Span { pos, length: 1 },
                    })
                }
                '.' => {
                    self.consume();
                    Some(Token {
                        tokenType: TokenType::Dot,
                        span: Span { pos, length: 1 },
                    })
                }
                '=' => {
                    self.consume();
                    match self.current {
                        Some('>') => {
                            self.consume();

                            Some(Token {
                                tokenType: TokenType::FatArrow,
                                span: Span { pos, length: 2 },
                            })
                        }
                        _ => Some(Token {
                            tokenType: TokenType::Equals,
                            span: Span { pos, length: 1 },
                        }),
                    }
                }
                '-' => {
                    self.consume();
                    match self.current {
                        Some('>') => {
                            self.consume();

                            Some(Token {
                                tokenType: TokenType::Arrow,
                                span: Span { pos, length: 2 },
                            })
                        }
                        _ => Some(Token {
                            tokenType: TokenType::Hyphen,
                            span: Span { pos, length: 1 },
                        }),
                    }
                }
                '+' => {
                    self.consume();
                    Some(Token {
                        tokenType: TokenType::Plus,
                        span: Span { pos, length: 1 },
                    })
                }
                '/' => {
                    self.consume();
                    Some(Token {
                        tokenType: TokenType::Slash,
                        span: Span { pos, length: 1 },
                    })
                }
            },
        }
    }

    pub fn tokenize(self) -> Vec<Token> {
        todo!()
    }
}
