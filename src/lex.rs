mod test;

use std::str::Chars;

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum TokenType {
    Unexpected(char),

    Ctor,
    Ident(String),
    Int { value: usize, length: usize },

    DoubleQuote,
    Dollar,
    DollarLBrace,
    String(String),

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LAngle,
    RAngle,

    Backslash,
    Arrow,
    FatArrow,

    Dot,

    Asterisk,

    Equals,
    Colon,

    Comma,
    Pipe,

    Underscore,

    Hyphen,
    Plus,
    Slash,

    Indent(usize),
    Dedent,
    Space,
}

impl TokenType {
    pub fn render(&self) -> String {
        match self {
            TokenType::Unexpected(_) => String::from("unexpected"),
            TokenType::Ident(s) => {
                if s.len() == 0 {
                    String::from("identifier")
                } else {
                    format!("\"{}\"", s)
                }
            }
            TokenType::Int { value, length } => {
                if *length == 0 {
                    String::from("integer")
                } else {
                    format!("\"{}\"", value)
                }
            }
            TokenType::DoubleQuote => String::from("'\"'"),
            TokenType::Dollar => String::from("'$'"),
            TokenType::DollarLBrace => String::from("'${'"),
            TokenType::String(s) => format!("{:?}", s),
            TokenType::LBrace => String::from("'{'"),
            TokenType::RBrace => String::from("'}'"),
            TokenType::LParen => String::from("'('"),
            TokenType::RParen => String::from("')'"),
            TokenType::LBracket => String::from("'['"),
            TokenType::RBracket => String::from("']'"),
            TokenType::Backslash => String::from("'\\'"),
            TokenType::Arrow => String::from("'->'"),
            TokenType::FatArrow => String::from("'=>'"),
            TokenType::Dot => String::from("'.'"),
            TokenType::Asterisk => String::from("'*'"),
            TokenType::Equals => String::from("'='"),
            TokenType::Colon => String::from("':'"),
            TokenType::Comma => String::from("','"),
            TokenType::Underscore => String::from("'_'"),
            TokenType::Hyphen => String::from("'-'"),
            TokenType::Plus => String::from("'+'"),
            TokenType::Slash => String::from("'/'"),
            TokenType::Indent(n) => {
                if *n == 0 {
                    String::from("newline")
                } else {
                    format!("indent ({})", n)
                }
            }
            TokenType::Dedent => String::from("dedent"),
            TokenType::Space => String::from("space"),
            TokenType::Ctor => String::from("constructor"),
            TokenType::Pipe => String::from("'|'"),
            TokenType::LAngle => String::from("'<'"),
            TokenType::RAngle => String::from("'>'"),
        }
    }

    pub fn length(&self) -> usize {
        match self {
            TokenType::Unexpected(_) => 1,
            TokenType::Ident(s) => s.len(),
            TokenType::Int { value: _, length } => *length,
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
            TokenType::DoubleQuote => 1,
            TokenType::Dollar => 1,
            TokenType::DollarLBrace => 2,
            TokenType::String(s) => s.len(),
            TokenType::Pipe => 1,
            TokenType::LAngle => 1,
            TokenType::RAngle => 1,

            TokenType::Dedent => panic!("TokenType::Dedent.len()"),
            TokenType::Ctor => panic!("TokenType::Ctor.len()"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: usize,
}

enum Mode {
    String,
    Ident,
    Normal,
}

pub struct Lexer<'input> {
    pos: usize,
    current: Option<char>,
    input: Chars<'input>,
    mode: Vec<Mode>,
}

fn is_ident_start(c: char) -> bool {
    'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_'
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
            mode: vec![Mode::Normal],
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
            Some(c) => match &self.mode[self.mode.len() - 1] {
                Mode::Ident => {
                    if is_ident_start(c) {
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
                        let _ = self.mode.pop().unwrap();
                        Some(Token {
                            token_type: TokenType::Ident(ident),
                            pos,
                        })
                    } else {
                        Some(Token {
                            token_type: TokenType::Unexpected(c),
                            pos,
                        })
                    }
                }
                Mode::String => match c {
                    '"' => {
                        self.consume();
                        let _ = self.mode.pop().unwrap();
                        Some(Token {
                            token_type: TokenType::DoubleQuote,
                            pos,
                        })
                    }
                    '$' => {
                        self.consume();

                        match self.current {
                            Some(c) if c == '{' => {
                                self.consume();
                                self.mode.push(Mode::Normal);
                                Some(Token {
                                    token_type: TokenType::DollarLBrace,
                                    pos,
                                })
                            }
                            _ => {
                                self.mode.push(Mode::Ident);
                                Some(Token {
                                    token_type: TokenType::Dollar,
                                    pos,
                                })
                            }
                        }
                    }
                    c => {
                        self.consume();

                        let mut str = String::new();
                        str.push(c);

                        loop {
                            match self.current {
                                None => {
                                    break;
                                }
                                Some(c) => match c {
                                    '$' | '"' => {
                                        break;
                                    }
                                    '\\' => {
                                        self.consume();
                                        match self.current {
                                            None => {
                                                return Some(Token {
                                                    token_type: TokenType::Unexpected('\\'),
                                                    pos: self.pos,
                                                })
                                            }
                                            Some(c) => match c {
                                                '$' | '"' => {
                                                    str.push(c);
                                                }
                                                _ => {
                                                    return Some(Token {
                                                        token_type: TokenType::Unexpected('\\'),
                                                        pos: self.pos,
                                                    });
                                                }
                                            },
                                        }
                                    }
                                    c => {
                                        self.consume();
                                        str.push(c);
                                    }
                                },
                            }
                        }

                        Some(Token {
                            token_type: TokenType::String(str),
                            pos,
                        })
                    }
                },
                Mode::Normal => match c {
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
                    '"' => {
                        self.consume();
                        self.mode.push(Mode::String);
                        Some(Token {
                            token_type: TokenType::DoubleQuote,
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
                        self.mode.push(Mode::Normal);
                        Some(Token {
                            token_type: TokenType::LBrace,
                            pos,
                        })
                    }
                    '}' => {
                        self.consume();
                        let _ = self.mode.pop().unwrap();
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
                    '<' => {
                        self.consume();
                        Some(Token {
                            token_type: TokenType::LAngle,
                            pos,
                        })
                    }
                    '>' => {
                        self.consume();
                        Some(Token {
                            token_type: TokenType::RAngle,
                            pos,
                        })
                    }
                    '|' => {
                        self.consume();
                        Some(Token {
                            token_type: TokenType::Pipe,
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
                    _ if c.is_digit(10) => {
                        self.consume();
                        let mut length = 1;
                        let mut value: usize = c.to_digit(10).unwrap() as usize;
                        while let Some(n) = self.current.and_then(|cur| cur.to_digit(10)) {
                            self.consume();
                            value *= 10;
                            value += n as usize;
                            length += 1;
                        }
                        Some(Token {
                            token_type: TokenType::Int { value, length },
                            pos,
                        })
                    }
                    _ => {
                        self.consume();
                        Some(Token {
                            token_type: TokenType::Unexpected(c),
                            pos,
                        })
                    }
                },
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
