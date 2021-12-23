#[cfg(test)]
mod test;

pub mod token;

use std::{fmt::Write, rc::Rc, str::Chars};
use token::Token;

enum Mode {
    String,
    Char,
    Ident,
    Normal,
    Cmd,
}

pub struct Lexer<'input> {
    pos: usize,
    column: usize,
    current: Option<char>,
    input: Chars<'input>,
    mode: Vec<Mode>,
    is_eof: bool,
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
            column: 0,
            current: input.next(),
            input,
            mode: vec![Mode::Normal],
            is_eof: false,
        }
    }

    fn consume(&mut self) {
        self.current = self.input.next();
        self.pos += 1;
        self.column += 1;
    }

    fn consume_newline(&mut self) {
        self.current = self.input.next();
        self.pos += 1;
        self.column = 0;
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let pos = self.pos;
        let column = self.column;

        match self.current {
            None => {
                if self.is_eof {
                    None
                } else {
                    self.is_eof = true;
                    Some(Token {
                        data: token::Data::Eof,
                        pos,
                        column,
                    })
                }
            }
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
                            column,
                        })
                    } else {
                        Some(Token {
                            data: token::Data::Unexpected(c),
                            pos,
                            column,
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
                            column,
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
                                    column,
                                })
                            }
                            _ => {
                                self.mode.push(Mode::Ident);
                                Some(Token {
                                    data: token::Data::Dollar,
                                    pos,
                                    column,
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
                                                    column: self.column,
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
                                                        column: self.column,
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
                            column,
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
                            column,
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
                                                pos,
                                                column,
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
                                                    column: self.column,
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
                            column,
                        })
                    }
                },
                Mode::Normal => match c {
                    '\n' => {
                        self.consume_newline();
                        self.next()
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
                            column,
                        })
                    }
                    '"' => {
                        self.consume();
                        self.mode.push(Mode::String);
                        Some(Token {
                            data: token::Data::DoubleQuote,
                            pos,
                            column,
                        })
                    }
                    '\'' => {
                        self.consume();
                        self.mode.push(Mode::Char);
                        Some(Token {
                            data: token::Data::SingleQuote,
                            pos,
                            column,
                        })
                    }
                    '`' => {
                        self.consume();
                        self.mode.push(Mode::Cmd);

                        // Spaces are ignored in commands
                        while let Some(c) = self.current {
                            if c == ' ' {
                                self.consume();
                            } else {
                                break;
                            }
                        }

                        Some(Token {
                            data: token::Data::Backtick,
                            pos,
                            column,
                        })
                    }
                    ' ' => {
                        self.consume();
                        self.next()
                    }
                    '{' => {
                        self.consume();
                        self.mode.push(Mode::Normal);
                        Some(Token {
                            data: token::Data::LBrace,
                            pos,
                            column,
                        })
                    }
                    '}' => {
                        self.consume();
                        let _ = self.mode.pop().unwrap();
                        Some(Token {
                            data: token::Data::RBrace,
                            pos,
                            column,
                        })
                    }
                    '(' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::LParen,
                            pos,
                            column,
                        })
                    }
                    ')' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::RParen,
                            pos,
                            column,
                        })
                    }
                    '[' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::LBracket,
                            pos,
                            column,
                        })
                    }
                    ']' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::RBracket,
                            pos,
                            column,
                        })
                    }
                    '<' => {
                        self.consume();
                        if let Some('-') = self.current {
                            self.consume();
                            Some(Token {
                                data: token::Data::LeftArrow,
                                pos,
                                column,
                            })
                        } else {
                            Some(Token {
                                data: token::Data::LAngle,
                                pos,
                                column,
                            })
                        }
                    }
                    '>' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::RAngle,
                            pos,
                            column,
                        })
                    }
                    '|' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Pipe,
                            pos,
                            column,
                        })
                    }
                    ',' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Comma,
                            pos,
                            column,
                        })
                    }
                    ':' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Colon,
                            pos,
                            column,
                        })
                    }
                    '.' => {
                        self.consume();

                        match self.current {
                            Some(c) if c == '.' => {
                                self.consume();
                                Some(Token {
                                    data: token::Data::DotDot,
                                    pos,
                                    column,
                                })
                            }
                            _ => Some(Token {
                                data: token::Data::Dot,
                                pos,
                                column,
                            }),
                        }
                    }
                    '=' => {
                        self.consume();
                        match self.current {
                            Some('>') => {
                                self.consume();

                                Some(Token {
                                    data: token::Data::FatArrow,
                                    pos,
                                    column,
                                })
                            }
                            _ => Some(Token {
                                data: token::Data::Equals,
                                pos,
                                column,
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
                                    column,
                                })
                            }
                            _ => Some(Token {
                                data: token::Data::Hyphen,
                                pos,
                                column,
                            }),
                        }
                    }
                    '+' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Plus,
                            pos,
                            column,
                        })
                    }
                    '/' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Slash,
                            pos,
                            column,
                        })
                    }
                    '\\' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Backslash,
                            pos,
                            column,
                        })
                    }
                    '*' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Asterisk,
                            pos,
                            column,
                        })
                    }
                    '_' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Underscore,
                            pos,
                            column,
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
                            column,
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
                            column,
                        })
                    }
                    _ => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Unexpected(c),
                            pos,
                            column,
                        })
                    }
                },
                Mode::Cmd => match c {
                    '`' => {
                        self.consume();
                        self.mode.pop().unwrap();
                        Some(Token {
                            data: token::Data::Backtick,
                            pos,
                            column,
                        })
                    }
                    _ => {
                        let mut textual_length = 0;
                        let mut value = String::new();

                        while let Some(c) = self.current {
                            match c {
                                '`' => {
                                    break;
                                }
                                ' ' => {
                                    break;
                                }
                                '"' => {
                                    self.consume();
                                    self.mode.push(Mode::String);

                                    if textual_length == 0 {
                                        return Some(Token {
                                            data: token::Data::DoubleQuote,
                                            pos,
                                            column,
                                        });
                                    } else {
                                        break;
                                    }
                                }
                                '\\' => {
                                    self.consume();
                                    textual_length += 1;

                                    match self.current {
                                        Some('"') => {
                                            self.consume();
                                            value.write_char('"').unwrap();
                                            textual_length += 1;
                                        }
                                        Some('`') => {
                                            self.consume();
                                            value.write_char('`').unwrap();
                                            textual_length += 1;
                                        }
                                        Some('\\') => {
                                            self.consume();
                                            value.write_char('\\').unwrap();
                                            textual_length += 1;
                                        }
                                        Some(c) => {
                                            return Some(Token {
                                                data: token::Data::Unexpected(c),
                                                pos: self.pos,
                                                column: self.column,
                                            })
                                        }
                                        None => {
                                            return Some(Token {
                                                data: token::Data::Unexpected('\\'),
                                                pos: self.pos,
                                                column: self.column,
                                            })
                                        }
                                    }
                                }
                                _ => {
                                    self.consume();
                                    value.write_char(c).unwrap();
                                    textual_length += 1;
                                }
                            }
                        }

                        // Spaces are ignored in commands
                        while let Some(c) = self.current {
                            if c == ' ' {
                                self.consume();
                            } else {
                                break;
                            }
                        }

                        debug_assert!(textual_length != 0, "constructed empty Cmd value");
                        Some(Token {
                            data: token::Data::Cmd(Rc::from(value)),
                            pos,
                            column,
                        })
                    }
                },
            },
        }
    }
}
