mod test;

use std::str::Chars;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
    Space,
}

impl TokenType {
    pub fn length(&self) -> usize {
        match self {
            TokenType::Unexpected(_) => 1,
            TokenType::Ident(s) => s.len(),
            TokenType::Ctor(s) => s.len(),
            TokenType::LBrace => 1,
            TokenType::RBrace => 1,
            TokenType::LParen => 1,
            TokenType::RParen => 1,
            TokenType::LBracket => 1,
            TokenType::RBracket => 1,
            TokenType::Backslash => 1,
            TokenType::Arrow => 2,
            TokenType::FatArrow => 2,
            TokenType::Dot => 1,
            TokenType::Asterisk => 1,
            TokenType::Equals => 1,
            TokenType::Colon => 1,
            TokenType::Comma => 1,
            TokenType::Underscore => 1,
            TokenType::Hyphen => 1,
            TokenType::Plus => 1,
            TokenType::Slash => 1,
            TokenType::Indent(n) => n + 1,
            TokenType::Space => 1,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: usize,
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
        self.current = self.input.next();
        self.pos += 1;
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
                        pos,
                    })
                }
                ' ' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Space,
                        pos,
                    })
                }
                '{' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::LBrace,
                        pos,
                    })
                }
                '}' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::RBrace,
                        pos,
                    })
                }
                '(' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::LParen,
                        pos,
                    })
                }
                ')' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::RParen,
                        pos,
                    })
                }
                '[' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::LBracket,
                        pos,
                    })
                }
                ']' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::RBracket,
                        pos,
                    })
                }
                ',' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Comma,
                        pos,
                    })
                }
                ':' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Colon,
                        pos,
                    })
                }
                '.' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Dot,
                        pos,
                    })
                }
                '=' => {
                    self.consume();
                    match self.current {
                        Some('>') => {
                            self.consume();

                            Some(Token {
                                token_type: TokenType::FatArrow,
                                pos,
                            })
                        }
                        _ => Some(Token {
                            token_type: TokenType::Equals,
                            pos,
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
                                pos,
                            })
                        }
                        _ => Some(Token {
                            token_type: TokenType::Hyphen,
                            pos,
                        }),
                    }
                }
                '+' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Plus,
                        pos,
                    })
                }
                '/' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Slash,
                        pos,
                    })
                }
                '\\' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Backslash,
                        pos,
                    })
                }
                '*' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Asterisk,
                        pos,
                    })
                }
                '_' => {
                    self.consume();
                    Some(Token {
                        token_type: TokenType::Underscore,
                        pos,
                    })
                }
                _ if is_ident_start(c) => {
                    self.consume();
                    let mut ident = String::new();
                    ident.push(c);
                    loop {
                        match self.current {
                            Some(c) if is_ident_continue(c) => {
                                self.consume();
                                ident.push(c);
                            }
                            _ => break,
                        }
                    }
                    Some(Token {
                        token_type: TokenType::Ident(ident),
                        pos,
                    })
                }
                _ if is_ctor_start(c) => {
                    self.consume();
                    let mut ctor = String::new();
                    ctor.push(c);
                    loop {
                        match self.current {
                            Some(c) if is_ident_continue(c) => {
                                self.consume();
                                ctor.push(c);
                            }
                            _ => break,
                        }
                    }
                    Some(Token {
                        token_type: TokenType::Ctor(ctor),
                        pos,
                    })
                }
                _ => Some(Token {
                    token_type: TokenType::Unexpected(c),
                    pos,
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
