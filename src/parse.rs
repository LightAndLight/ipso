mod test;

use std::{collections::HashSet, fs::File, io::Read, vec::IntoIter};

use crate::syntax::{self, Branch, Declaration, Expr, Module, Pattern, Type};
use crate::{
    lex::{Lexer, Token, TokenType},
    syntax::Keyword,
};

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    Unexpected {
        pos: usize,
        expecting: HashSet<TokenType>,
    },
}

#[macro_export]
macro_rules! apply {
    ($a:expr, $b:expr) => {
        match $a {
            Err(err) => Err(err),
            Ok(f) => match $b {
                Err(err) => Err(err),
                Ok(x) => Ok(f(x)),
            },
        }
    };
}

macro_rules! map0 {
    ($a:expr, $b:expr) => {
        match $b {
            Err(err) => Err(err),
            Ok(_) => Ok($a),
        }
    };
}

#[macro_export]
macro_rules! map1 {
    ($f:expr, $a:expr) => {
        match $a {
            Err(err) => Err(err),
            Ok(val1) => Ok($f(val1)),
        }
    };
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

#[macro_export]
macro_rules! map3 {
    ($f:expr, $a:expr, $b:expr, $c:expr) => {
        apply!(map2!(|a, b| move |c| $f(a, b, c), $a, $b), $c)
    };
}

#[macro_export]
macro_rules! map4 {
    ($f:expr, $a:expr, $b:expr, $c:expr, $d:expr) => {
        apply!(map3!($f, $a, $b, $c), $d)
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
        let lexer = Lexer::new(&input);
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

#[macro_export]
macro_rules! sep_by {
    ($self:expr, $a:expr, $sep:expr) => {
        choices!(
            $self,
            {
                match $a {
                    Ok(first) => {
                        let mut error: Option<ParseError> = None;
                        let mut acc: Vec<_> = vec![first];
                        loop {
                            $self.consumed = false;
                            match keep_right!($sep, $a) {
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
                    }
                    Err(err) => Err(err),
                }
            },
            Ok(Vec::new())
        )
    };
}

macro_rules! choices {
    ($self:expr) => {
        $self.unexpected()
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
        self.indentation[self.indentation.len() - 1]
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

    fn indent(&mut self) -> Result<(), ParseError> {
        self.expecting.insert(TokenType::Indent(0));
        let current_indentation = self.current_indentation();
        match self.current {
            Some(ref token) => match token.token_type {
                TokenType::Indent(n) if n > current_indentation => {
                    self.indentation.push(n);
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

    fn token(&mut self, expected: &TokenType) -> Result<(), ParseError> {
        self.expecting.insert(expected.clone());
        match self.current {
            Some(ref actual) if actual.token_type == *expected => {
                self.consume();
                Ok(())
            }
            _ => self.unexpected(),
        }
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

    fn int(&mut self) -> Result<u32, ParseError> {
        self.expecting.insert(TokenType::Int {
            value: 0,
            length: 0,
        });
        keep_left!(
            match self.current {
                Some(ref token) => match token.token_type {
                    TokenType::Int { value, length: _ } => {
                        self.consume();
                        Ok(value as u32)
                    }
                    _ => self.unexpected(),
                },
                None => self.unexpected(),
            },
            self.spaces()
        )
    }

    fn type_atom(&mut self) -> Result<Type, ParseError> {
        keep_left!(
            choices!(
                self,
                map0!(
                    Type::Bool,
                    self.token(&TokenType::Ident(String::from("Bool")))
                ),
                map0!(
                    Type::Int,
                    self.token(&TokenType::Ident(String::from("Int")))
                ),
                map0!(
                    Type::Char,
                    self.token(&TokenType::Ident(String::from("Char")))
                ),
                map0!(
                    Type::String,
                    self.token(&TokenType::Ident(String::from("String")))
                ),
                map0!(
                    Type::Array,
                    self.token(&TokenType::Ident(String::from("Array")))
                ),
                map0!(Type::IO, self.token(&TokenType::Ident(String::from("IO")))),
                map1!(|s| Type::Name(s), self.ident()),
                keep_right!(
                    keep_left!(self.token(&TokenType::LParen), self.spaces()),
                    keep_left!(self.type_(), self.token(&TokenType::RParen))
                )
            ),
            self.spaces()
        )
    }

    fn type_app(&mut self) -> Result<Type, ParseError> {
        let mut acc = self.type_atom()?;
        while let Ok(ty) = self.type_atom() {
            acc = Type::mk_app(acc, ty)
        }
        Ok(acc)
    }

    fn type_arrow(&mut self) -> Result<Type, ParseError> {
        let a = self.type_app()?;
        let m_b = optional!(
            self,
            map3!(
                |_, _, ty| ty,
                self.token(&TokenType::Arrow),
                self.spaces(),
                self.type_arrow()
            )
        )?;

        match m_b {
            None => Ok(a),
            Some(b) => Ok(Type::mk_arrow(a, b)),
        }
    }

    fn type_fatarrow(&mut self) -> Result<Type, ParseError> {
        let a = self.type_arrow()?;
        let m_b = optional!(
            self,
            map3!(
                |_, _, ty| ty,
                self.token(&TokenType::FatArrow),
                self.spaces(),
                self.type_fatarrow()
            )
        )?;

        match m_b {
            None => Ok(a),
            Some(b) => Ok(Type::mk_fatarrow(a, b)),
        }
    }

    fn type_(&mut self) -> Result<Type, ParseError> {
        self.type_fatarrow()
    }

    fn newline(&mut self) -> Result<(), ParseError> {
        let current = self.current_indentation();
        self.expecting.insert(TokenType::Indent(current));
        match self.current {
            None => self.unexpected(),
            Some(ref token) => match token.token_type {
                TokenType::Indent(n) if n == current => {
                    self.consume();
                    Ok(())
                }
                _ => self.unexpected(),
            },
        }
    }

    fn pattern(&mut self) -> Result<Pattern, ParseError> {
        choices!(
            self,
            map1!(|s| Pattern::Name(s), self.ident()),
            map0!(Pattern::Wildcard, self.token(&TokenType::Underscore))
        )
    }

    fn expr_atom(&mut self) -> Result<Expr, ParseError> {
        choices!(
            self,
            map1!(|n| Expr::Int(n), self.int()),
            map1!(|n| Expr::Var(n), self.ident())
        )
    }

    fn branch(&mut self) -> Result<Branch, ParseError> {
        map3!(
            |pattern, _, body| Branch { pattern, body },
            self.pattern(),
            keep_left!(self.token(&TokenType::Arrow), self.spaces()),
            self.expr()
        )
    }

    fn expr_case(&mut self) -> Result<Expr, ParseError> {
        let _ = self.token(&TokenType::Ident(String::from("case")))?;
        let _ = self.spaces()?;

        let cond = self.expr()?;

        let _ = self.token(&TokenType::Ident(String::from("of")))?;
        let _ = many_!(self, self.token(&TokenType::Space))?;

        let _ = self.indent()?;
        let branches = sep_by!(self, self.branch(), self.newline())?;
        self.indentation.pop();

        Ok(Expr::Case(Box::new(cond), branches))
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        choices!(self, self.expr_atom(), self.expr_case())
    }

    fn definition(&mut self) -> Result<Declaration, ParseError> {
        let name = self.ident()?;

        let _ = self.token(&TokenType::Colon)?;
        let _ = self.spaces()?;

        let ty = self.type_()?;

        let _ = self.newline()?;

        let _ = self.token(&TokenType::Ident(name.clone()))?;
        let args = many!(self, self.pattern())?;

        let _ = self.token(&TokenType::Equals)?;
        let _ = self.spaces()?;

        let body = self.expr()?;

        Ok(Declaration::Definition {
            name,
            ty,
            args,
            body,
        })
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
