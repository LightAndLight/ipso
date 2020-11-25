use std::str::Chars;

pub struct Span {
    pub pos: usize,
    pub length: usize,
}

#[derive(PartialEq, Eq)]
pub enum TokenType {
    Unexpected(char),

    Ident(String),
    Ctor(String),

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,

    Backslash,
    Arrow,
    FatArrow,

    Dot,

    Asterisk,

    Equals,
    Colon,

    Comma,

    Underscore,

    Hyphen,
    Plus,
    Slash,

    Indent(usize),
}

pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

pub struct Lexer<'input> {
    pos: usize,
    current: Option<char>,
    input: Chars<'input>,
}

fn is_ctor_start(c: char) -> bool {
    'A' <= c && c <= 'Z'
}

fn is_ident_start(c: char) -> bool {
    'a' <= c && c <= 'z'
}

fn is_ident_continue(c: char) -> bool {
    'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || '0' <= c && c <= '9' || c == '_'
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
                '\n' => {
                    self.consume();
                    let mut depth = 0;
                    loop {
                        match self.current {
                            Some(c) if c == ' ' => {
                                self.consume();
                                depth += 1;
                            }
                            _ => break,
                        }
                    }
                    Some(Token {
                        token_type: TokenType::Indent(depth),
                        span: Span {
                            pos,
                            length: depth + 1,
                        },
                    })
                }
                '{' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::LBrace,
                        span: Span { pos, length: 1 },
                    })
                }
                '}' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::RBrace,
                        span: Span { pos, length: 1 },
                    })
                }
                '(' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::LParen,
                        span: Span { pos, length: 1 },
                    })
                }
                ')' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::RParen,
                        span: Span { pos, length: 1 },
                    })
                }
                '[' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::LBracket,
                        span: Span { pos, length: 1 },
                    })
                }
                ']' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::RBracket,
                        span: Span { pos, length: 1 },
                    })
                }
                ',' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Comma,
                        span: Span { pos, length: 1 },
                    })
                }
                ':' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Colon,
                        span: Span { pos, length: 1 },
                    })
                }
                '.' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Dot,
                        span: Span { pos, length: 1 },
                    })
                }
                '=' => {
                    self.consume();
                    match self.current {
                        Some('>') => {
                            self.consume();

                            Some(Token {
                                token_type: TokenType::FatArrow,
                                span: Span { pos, length: 2 },
                            })
                        }
                        _ => Some(Token {
                            token_type: TokenType::Equals,
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
                                token_type: TokenType::Arrow,
                                span: Span { pos, length: 2 },
                            })
                        }
                        _ => Some(Token {
                            token_type: TokenType::Hyphen,
                            span: Span { pos, length: 1 },
                        }),
                    }
                }
                '+' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Plus,
                        span: Span { pos, length: 1 },
                    })
                }
                '/' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Slash,
                        span: Span { pos, length: 1 },
                    })
                }
                '\\' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Backslash,
                        span: Span { pos, length: 1 },
                    })
                }
                '*' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Asterisk,
                        span: Span { pos, length: 1 },
                    })
                }
                '_' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Underscore,
                        span: Span { pos, length: 1 },
                    })
                }
                _ if is_ident_start(c) => {
                    self.consume();
                    let mut ident = String::new();
                    ident.push(c);
                    let mut length: usize = 1;
                    loop {
                        match self.current {
                            Some(c) if is_ident_continue(c) => {
                                self.consume();
                                ident.push(c);
                                length += 1;
                            }
                            _ => break,
                        }
                    }
                    Some(Token {
                        token_type: TokenType::Ident(ident),
                        span: Span { pos, length },
                    })
                }
                _ if is_ctor_start(c) => {
                    self.consume();
                    let mut ctor = String::new();
                    ctor.push(c);
                    let mut length: usize = 1;
                    loop {
                        match self.current {
                            Some(c) if is_ident_continue(c) => {
                                self.consume();
                                ctor.push(c);
                                length += 1;
                            }
                            _ => break,
                        }
                    }
                    Some(Token {
                        token_type: TokenType::Ctor(ctor),
                        span: Span { pos, length },
                    })
                }
                _ => Some(Token {
                    token_type: TokenType::Unexpected(c),
                    span: Span { pos, length: 1 },
                }),
            },
        }
    }

    pub fn tokenize(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next() {
            tokens.push(token);
        }
        tokens
    }
}
