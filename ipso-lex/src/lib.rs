#[cfg(test)]
mod test;

pub mod token;

use std::{fmt::Write, rc::Rc, str::Chars};
use token::{Sign, Token};

#[derive(Clone, Copy)]
enum Mode {
    String,
    Char,
    Ident,
    Normal,
    Cmd,
}

struct Context {
    current: Mode,
    saved: Vec<Mode>,
}

impl Context {
    fn new(mode: Mode) -> Self {
        Context {
            current: mode,
            saved: vec![],
        }
    }

    fn current(&self) -> Mode {
        self.current
    }

    fn push(&mut self, mode: Mode) {
        let top = std::mem::replace(&mut self.current, mode);
        self.saved.push(top);
    }

    fn pop(&mut self) -> Mode {
        let top = self.saved.pop().expect("no more saved items");
        std::mem::replace(&mut self.current, top)
    }
}

pub struct Lexer<'input> {
    pos: usize,
    column: usize,
    current: Option<char>,
    input: Chars<'input>,
    context: Context,
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
            context: Context::new(Mode::Normal),
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

    fn consume_int(&mut self, sign: Sign, c: char, pos: usize, column: usize) -> Option<Token> {
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
            data: token::Data::Int {
                sign,
                value,
                length,
            },
            pos,
            column,
        })
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
            Some(c) => match self.context.current() {
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

                        let _ = self.context.pop();

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

                        let _ = self.context.pop();

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
                                self.context.push(Mode::Normal);
                                Some(Token {
                                    data: token::Data::DollarLBrace,
                                    pos,
                                    column,
                                })
                            }
                            _ => {
                                self.context.push(Mode::Ident);
                                Some(Token {
                                    data: token::Data::Dollar,
                                    pos,
                                    column,
                                })
                            }
                        }
                    }
                    '\n' => {
                        self.consume();

                        Some(Token {
                            data: token::Data::Unexpected(c),
                            pos,
                            column,
                        })
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
                                    '$' | '"' | '\n' => {
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

                        let _ = self.context.pop();

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
                    ' ' => {
                        self.consume();
                        self.next()
                    }
                    '#' => {
                        self.consume();

                        while let Some(c) = self.current {
                            if c != '\n' {
                                self.consume();
                            } else {
                                break;
                            }
                        }

                        self.next()
                    }
                    '"' => {
                        self.consume();
                        self.context.push(Mode::String);
                        Some(Token {
                            data: token::Data::DoubleQuote,
                            pos,
                            column,
                        })
                    }
                    '\'' => {
                        self.consume();
                        self.context.push(Mode::Char);
                        Some(Token {
                            data: token::Data::SingleQuote,
                            pos,
                            column,
                        })
                    }
                    '`' => {
                        self.consume();
                        self.context.push(Mode::Cmd);

                        Some(Token {
                            data: token::Data::Backtick,
                            pos,
                            column,
                        })
                    }
                    '{' => {
                        self.consume();
                        self.context.push(Mode::Normal);
                        Some(Token {
                            data: token::Data::LBrace,
                            pos,
                            column,
                        })
                    }
                    '}' => {
                        self.consume();

                        let _ = self.context.pop();

                        Some(Token {
                            data: token::Data::RBrace,
                            pos,
                            column,
                        })
                    }
                    '(' => {
                        self.consume();

                        match self.current {
                            Some('|') => {
                                self.consume();
                                Some(Token {
                                    data: token::Data::LParenPipe,
                                    pos,
                                    column,
                                })
                            }
                            _ => Some(Token {
                                data: token::Data::LParen,
                                pos,
                                column,
                            }),
                        }
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

                        match self.current {
                            Some(')') => {
                                self.consume();
                                Some(Token {
                                    data: token::Data::PipeRParen,
                                    pos,
                                    column,
                                })
                            }
                            _ => Some(Token {
                                data: token::Data::Pipe,
                                pos,
                                column,
                            }),
                        }
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
                            Some(c) if c.is_ascii_digit() => {
                                self.consume_int(Sign::Negative, c, pos, column)
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
                    '!' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Bang,
                            pos,
                            column,
                        })
                    }
                    '&' => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Ampersand,
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
                    _ if c.is_ascii_digit() => self.consume_int(Sign::None, c, pos, column),
                    _ => {
                        self.consume();
                        Some(Token {
                            data: token::Data::Unexpected(c),
                            pos,
                            column,
                        })
                    }
                },
                Mode::Cmd => {
                    let pos = self.pos;
                    let column = self.column;

                    match c {
                        '`' => {
                            self.consume();

                            self.context.pop();

                            Some(Token {
                                data: token::Data::Backtick,
                                pos,
                                column,
                            })
                        }
                        ' ' => {
                            self.consume();

                            Some(Token {
                                data: token::Data::Space,
                                pos,
                                column,
                            })
                        }
                        '"' => {
                            self.consume();
                            self.context.push(Mode::String);

                            Some(Token {
                                data: token::Data::DoubleQuote,
                                pos,
                                column,
                            })
                        }
                        '$' => {
                            self.consume();
                            self.context.push(Mode::Ident);

                            Some(Token {
                                data: token::Data::Dollar,
                                pos,
                                column,
                            })
                        }
                        '\n' => {
                            self.consume();

                            Some(Token {
                                data: token::Data::Unexpected(c),
                                pos,
                                column,
                            })
                        }
                        _ => {
                            let mut textual_length = 0;
                            let mut value = String::new();

                            while let Some(c) = self.current {
                                match c {
                                    '`' | ' ' | '"' | '$' | '\n' => {
                                        break;
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

                            if textual_length > 0 {
                                Some(Token {
                                    data: token::Data::Cmd(Rc::from(value)),
                                    pos,
                                    column,
                                })
                            } else {
                                self.next()
                            }
                        }
                    }
                }
            },
        }
    }
}
