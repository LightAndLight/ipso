mod test;

use std::{collections::HashSet, fs::File, io::Read, vec::IntoIter};

use crate::syntax::{self, Declaration, Module};
use crate::{
    lex::{Lexer, Token, TokenType},
    syntax::Keyword,
};

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    Empty,
    Unexpected {
        pos: usize,
        expecting: HashSet<TokenType>,
    },
}

#[macro_export]
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

#[macro_export]
macro_rules! keep_left {
    ($a:expr, $b:expr) => {
        map2!(|a, _| a, $a, $b)
    };
}

pub fn parse_string(input: String) -> Result<Module, ParseError> {
    let tokens: Vec<Token> = {
        let mut lexer = Lexer::new(&input);
        lexer.tokenize()
    };
    let mut parser: Parser = Parser::new(tokens);
    keep_left!(parser.module(), parser.eof())
}

pub fn parse_file(filename: &String) -> Result<Module, ParseError> {
    let input: String = {
        let mut content = String::new();
        let mut file: File = File::open(filename).unwrap();
        file.read_to_string(&mut content).unwrap();
        content
    };
    parse_string(input)
}

pub struct Parser {
    pos: usize,
    consumed: bool,
    indentation: Vec<usize>,
    expecting: HashSet<TokenType>,
    current: Option<Token>,
    input: IntoIter<Token>,
}

macro_rules! many {
    ($self:expr, $x:expr) => {{
        let mut error: Option<ParseError> = None;
        let mut acc: Vec<_> = Vec::new();
        loop {
            $self.consumed = false;
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

macro_rules! many_ {
    ($self:expr, $x:expr) => {{
        let mut error: Option<ParseError> = None;
        loop {
            $self.consumed = false;
            match $x {
                Err(err) => {
                    if $self.consumed {
                        error = Some(err);
                    };
                    break;
                }
                Ok(_) => {}
            }
        }
        match error {
            None => Ok(()),
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
        let mut input = input.into_iter();
        let current = input.next();
        Parser {
            pos: 0,
            consumed: false,
            indentation: vec![0],
            expecting: HashSet::new(),
            current,
            input,
        }
    }

    fn fail<A>(&self, err: ParseError) -> Result<A, ParseError> {
        Err(err)
    }

    fn unexpected<A>(&self) -> Result<A, ParseError> {
        self.fail(ParseError::Unexpected {
            pos: self.pos,
            expecting: self.expecting.clone(),
        })
    }

    fn eof(&self) -> Result<(), ParseError> {
        match self.current {
            None => Ok(()),
            Some(_) => self.unexpected(),
        }
    }

    fn consume(&mut self) {
        let len = match self.current {
            None => 0,
            Some(ref token) => token.token_type.length(),
        };
        self.pos += len;
        self.current = self.input.next();
        self.expecting = HashSet::new();
        self.consumed = true;
    }

    fn current_indentation(&self) -> usize {
        self.indentation[0]
    }

    fn space(&mut self) -> Result<(), ParseError> {
        self.expecting.insert(TokenType::Space);
        match self.current {
            Some(ref token) => match token.token_type {
                TokenType::Indent(n) if n > self.current_indentation() => {
                    self.consume();
                    Ok(())
                }
                TokenType::Space => {
                    self.consume();
                    Ok(())
                }
                _ => self.unexpected(),
            },
            None => self.unexpected(),
        }
    }

    fn spaces(&mut self) -> Result<(), ParseError> {
        many_!(self, self.space())
    }

    fn keyword(&mut self, expected: Keyword) -> Result<(), ParseError> {
        self.expecting
            .insert(TokenType::Ident(String::from(expected.to_string())));
        keep_left!(
            match self.current {
                None => self.unexpected(),
                Some(ref actual) => match actual.token_type {
                    TokenType::Ident(ref id) => {
                        if expected.matches(id) {
                            self.consume();
                            Ok(())
                        } else {
                            self.unexpected()
                        }
                    }
                    _ => self.unexpected(),
                },
            },
            { self.spaces() }
        )
    }

    fn token(&mut self, expected: &Token) -> Result<(), ParseError> {
        self.expecting.insert(expected.token_type.clone());
        keep_left!(
            match self.current {
                Some(ref actual) if actual.token_type == expected.token_type => {
                    self.consume();
                    Ok(())
                }
                _ => self.unexpected(),
            },
            self.spaces()
        )
    }

    fn ident(&mut self) -> Result<String, ParseError> {
        self.expecting.insert(TokenType::Ident(String::new()));
        keep_left!(
            match self.current {
                Some(ref token) => match token.token_type {
                    TokenType::Ident(ref s) if !syntax::is_keyword(s) => {
                        let s = s.clone();
                        self.consume();
                        Ok(s)
                    }
                    _ => self.unexpected(),
                },
                None => self.unexpected(),
            },
            self.spaces()
        )
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
