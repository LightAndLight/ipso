use std::{fs::File, io::Read, slice::Iter, str::Chars, vec::IntoIter};

use crate::syntax::{Declaration, Module};
use crate::{
    lex::{Lexer, Token, TokenType},
    syntax::Keyword,
};

#[derive(Debug)]
pub enum ParseError {
    Empty,
    Unexpected { pos: usize },
}

pub fn parse_file(filename: &String) -> Result<Module, ParseError> {
    let tokens: Vec<Token> = {
        let mut content = String::new();
        let mut file: File = File::open(filename).unwrap();
        file.read_to_string(&mut content).unwrap();
        let mut lexer = Lexer::new(&content);
        lexer.tokenize()
    };
    let mut parser: Parser = Parser::new(tokens);
    parser.module()
}

pub struct Parser {
    pos: usize,
    consumed: bool,
    input: IntoIter<Token>,
}

macro_rules! many {
    ($self:expr, $x:expr) => {{
        let mut error: Option<ParseError> = None;
        let mut acc: Vec<_> = Vec::new();
        loop {
            match $x {
                Err(err) => {
                    if $self.consumed {
                        error = Some(err);
                    };
                    break;
                }
                Ok(val) => acc.push(val),
            }
        }
        match error {
            None => Ok(acc),
            Some(err) => Err(err),
        }
    }};
}

macro_rules! choices {
    ($self:expr) => {
        $self.fail(ParseError::Empty)
    };
    ($self:expr, $x:expr $(, $y:expr)*) => {
        match $x {
            Err(err) => {
                if $self.consumed {
                    Err(err)
                } else {
                    choices!($self $(, $y)*)
                }
            }
            Ok(val) => Ok(val),
        }
    };
}

macro_rules! map2 {
    ($f:expr, $a:expr, $b:expr) => {
        match $a {
            Err(err) => Err(err),
            Ok(val1) => match $b {
                Err(err) => Err(err),
                Ok(val2) => Ok($f(val1, val2)),
            },
        }
    };
}

macro_rules! keep_right {
    ($a:expr, $b:expr) => {
        map2!(|_, a| a, $a, $b)
    };
}

macro_rules! optional {
    ($self:expr, $a:expr) => {
        match $a {
            Err(err) => {
                if $self.consumed {
                    Err(err)
                } else {
                    Ok(None)
                }
            }
            Ok(val) => Ok(Some(val)),
        }
    };
}

impl Parser {
    fn new(input: Vec<Token>) -> Self {
        let input = input.into_iter();
        Parser {
            pos: 0,
            consumed: false,
            input,
        }
    }

    fn fail<A>(&mut self, err: ParseError) -> Result<A, ParseError> {
        Err(err)
    }

    fn keyword(&mut self, expected: Keyword) -> Result<Token, ParseError> {
        match self.input.next() {
            None => Err(ParseError::Unexpected { pos: self.pos }),
            Some(actual) => match actual.token_type {
                TokenType::Ident(ref id) => {
                    if expected.matches(id) {
                        self.pos += actual.span.length;
                        return Ok(actual);
                    } else {
                        Err(ParseError::Unexpected { pos: self.pos })
                    }
                }
                _ => Err(ParseError::Unexpected { pos: self.pos }),
            },
        }
    }

    fn token(&mut self, expected: TokenType) -> Result<Token, ParseError> {
        match self.input.next() {
            None => Err(ParseError::Unexpected { pos: self.pos }),
            Some(actual) => {
                if expected == actual.token_type {
                    self.pos += actual.span.length;
                    return Ok(actual);
                } else {
                    Err(ParseError::Unexpected { pos: self.pos })
                }
            }
        }
    }

    fn ident(&mut self) -> Result<String, ParseError> {
        todo!()
    }

    fn definition(&mut self) -> Result<Declaration, ParseError> {
        todo!()
    }

    fn type_alias(&mut self) -> Result<Declaration, ParseError> {
        todo!()
    }

    fn import(&mut self) -> Result<Declaration, ParseError> {
        map2!(
            |module, name| Declaration::Import { module, name },
            keep_right!(self.keyword(Keyword::Import), self.ident()),
            optional!(self, keep_right!(self.keyword(Keyword::As), self.ident()))
        )
    }

    fn from_import(&mut self) -> Result<Declaration, ParseError> {
        todo!()
    }

    fn declaration(&mut self) -> Result<Declaration, ParseError> {
        choices!(
            self,
            self.definition(),
            self.type_alias(),
            self.import(),
            self.from_import()
        )
    }

    fn module(&mut self) -> Result<Module, ParseError> {
        many!(self, self.declaration()).map(|decls| Module { decls })
    }
}
