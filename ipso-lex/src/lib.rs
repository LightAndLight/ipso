#[cfg(test)]
mod test;

pub mod token;

use std::{rc::Rc, str::Chars};
use token::Token;

enum Mode {
    String,
    Char,
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
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || ('0'..='9').contains(&c) || c == '_'
}

impl<'input> Lexer<'input> {
    pub fn new(input: &str) -> Lexer {
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
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

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
                            data: token::Data::Ident(Rc::from(ident)),
                            pos,
                        })
                    } else {
                        Some(Token {
                            data: token::Data::Unexpected(c),
                            pos,
                        })
                    }
                }
                Mode::String => match c {
                    '"' => {
                        self.consume();
                        let _ = self.mode.pop().unwrap();
                        Some(Token {
                            data: token::Data::DoubleQuote,
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
                                    data: token::Data::DollarLBrace,
                                    pos,
                                })
                            }
                            _ => {
                                self.mode.push(Mode::Ident);
                                Some(Token {
                                    data: token::Data::Dollar,
                                    pos,
                                })
                            }
                        }
                    }
                    _ => {
                        let mut str = String::new();
                        let mut textual_len: usize = 0;

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
                                        textual_len += 1;
                                        self.consume();
                                        match self.current {
                                            None => {
                                                return Some(Token {
                                                    data: token::Data::Unexpected('\\'),
                                                    pos: self.pos,
                                                })
                                            }
                                            Some(c) => match c {
                                                '$' | '"' => {
                                                    textual_len += 1;
                                                    self.consume();
                                                    str.push(c);
                                                }
                                                'n' => {
                                                    textual_len += 1;
                                                    self.consume();
                                                    str.push('\n');
                                                }
                                                't' => {
                                                    textual_len += 1;
                                                    self.consume();
                                                    str.push('\t');
                                                }
                                                _ => {
                                                    return Some(Token {
                                                        data: token::Data::Unexpected('\\'),
                                                        pos: self.pos,
                                                    });
                                                }
                                            },
                                        }
                                    }
                                    c => {
                                        textual_len += 1;
                                        self.consume();
                                        str.push(c);
                                    }
                                },
                            }
                        }

                        Some(Token {
                            data: token::Data::String {
                                value: str,
                                length: textual_len,
                            },
                            pos,
                        })
                    }
                },
                Mode::Char => match c {
                    '\'' => {
                        self.consume();
                        let _ = self.mode.pop().unwrap();
                        Some(Token {
                            data: token::Data::SingleQuote,
                            pos,
                        })
                    }
                    _ => {
                        let char;
                        let mut textual_len: usize = 0;

                        match self.current {
                            None => {
                                char = None;
                            }
                            Some(c) => match c {
                                '\\' => {
                                    textual_len += 1;
                                    self.consume();
                                    match self.current {
                                        None => {
                                            return Some(Token {
                                                data: token::Data::Unexpected('\\'),
                                                pos: self.pos,
                                            })
                                        }
                                        Some(c) => match c {
                                            '\'' | '\\' => {
                                                textual_len += 1;
                                                self.consume();
                                                char = Some(c);
                                            }
                                            'n' => {
                                                textual_len += 1;
                                                self.consume();
                                                char = Some('\n');
                                            }
                                            't' => {
                                                textual_len += 1;
                                                self.consume();
                                                char = Some('\t');
                                            }
                                            _ => {
                                                return Some(Token {
                                                    data: token::Data::Unexpected('\\'),
                                                    pos: self.pos,
                                                });
                                            }
                                        },
                                    }
                                }
                                c => {
                                    textual_len += 1;
                                    self.consume();
                                    char = Some(c);
                                }
                            },
                        }

                        char.map(|value| Token {
                            data: token::Data::Char {
                                value,
                                length: textual_len,
                            },
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
                            data: token::Data::Indent(depth),
                            pos,
                        })
                    }
                    '#' => {
                        self.consume();
                        let mut textual_length = 1;

                        match self.current {
                            None => return None,
                            Some(c) => {
                                let mut c = c;
                                while c != '\n' {
                                    self.consume();
                                    textual_length += 1;
                                    match self.current {
                                        None => return None,
                                        Some(new_c) => {
                                            c = new_c;
                                        }
                                    }
                                }
                            }
                        }

                        Some(Token {
                            data: token::Data::Comment {
                                length: textual_length,
                            },
                            pos,
                        })
                    }
                    '"' => {
                        self.consume();
                        self.mode.push(Mode::String);
                        Some(Token {
                            data: token::Data::DoubleQuote,
                            pos,
                        })
                    }
                    '\'' => {
                        self.consume();
                        self.mode.push(Mode::Char);
                        Some(Token {
                            data: token::Data::SingleQuote,
                            pos,
                        })
                    }
                    ' ' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Space,
                            pos,
                        })
                    }
                    '{' => {
                        self.consume();
                        self.mode.push(Mode::Normal);
                        Some(Token {
                            data: token::Data::LBrace,
                            pos,
                        })
                    }
                    '}' => {
                        self.consume();
                        let _ = self.mode.pop().unwrap();
                        Some(Token {
                            data: token::Data::RBrace,
                            pos,
                        })
                    }
                    '(' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::LParen,
                            pos,
                        })
                    }
                    ')' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::RParen,
                            pos,
                        })
                    }
                    '[' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::LBracket,
                            pos,
                        })
                    }
                    ']' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::RBracket,
                            pos,
                        })
                    }
                    '<' => {
                        self.consume();
                        if let Some('-') = self.current {
                            self.consume();
                            Some(Token {
                                data: token::Data::LeftArrow,
                                pos,
                            })
                        } else {
                            Some(Token {
                                data: token::Data::LAngle,
                                pos,
                            })
                        }
                    }
                    '>' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::RAngle,
                            pos,
                        })
                    }
                    '|' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Pipe,
                            pos,
                        })
                    }
                    ',' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Comma,
                            pos,
                        })
                    }
                    ':' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Colon,
                            pos,
                        })
                    }
                    '.' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Dot,
                            pos,
                        })
                    }
                    '=' => {
                        self.consume();
                        match self.current {
                            Some('>') => {
                                self.consume();

                                Some(Token {
                                    data: token::Data::FatArrow,
                                    pos,
                                })
                            }
                            _ => Some(Token {
                                data: token::Data::Equals,
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
                                    data: token::Data::Arrow,
                                    pos,
                                })
                            }
                            _ => Some(Token {
                                data: token::Data::Hyphen,
                                pos,
                            }),
                        }
                    }
                    '+' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Plus,
                            pos,
                        })
                    }
                    '/' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Slash,
                            pos,
                        })
                    }
                    '\\' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Backslash,
                            pos,
                        })
                    }
                    '*' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Asterisk,
                            pos,
                        })
                    }
                    '_' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Underscore,
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
                            data: token::Data::Ident(Rc::from(ident)),
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
                            data: token::Data::Int { value, length },
                            pos,
                        })
                    }
                    _ => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Unexpected(c),
                            pos,
                        })
                    }
                },
            },
        }
    }
}
