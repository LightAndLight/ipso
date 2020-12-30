mod test;

use crate::syntax::Spanned;
use crate::Diagnostic;
use crate::Item;
use std::collections::BTreeSet;
use std::{fs::File, io::Read, vec::IntoIter};

use crate::syntax::{self, Branch, Declaration, Expr, Module, Pattern, Type};
use crate::{
    lex::{Lexer, Token, TokenType},
    syntax::{Keyword, Names},
};

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    Unexpected {
        pos: usize,
        expecting: BTreeSet<TokenType>,
    },
}

impl ParseError {
    fn position(&self) -> usize {
        match self {
            ParseError::Unexpected { pos, expecting: _ } => *pos,
        }
    }

    fn message(&self) -> String {
        match self {
            ParseError::Unexpected { pos: _, expecting } => {
                let mut str = String::from("expected one of: ");
                let mut iter = expecting.iter();
                match iter.next() {
                    None => return str,
                    Some(token) => {
                        str.push_str(token.render().as_str());
                        for token in iter {
                            str.push_str(", ");
                            str.push_str(token.render().as_str());
                        }
                        str
                    }
                }
            }
        }
    }

    pub fn report(self, diagnostic: &mut Diagnostic) {
        diagnostic.item(Item {
            pos: self.position(),
            message: self.message(),
            addendum: None,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseResult<A> {
    consumed: bool,
    result: Result<A, ParseError>,
}

impl<A> ParseResult<A> {
    fn and_then<B, F>(self, f: F) -> ParseResult<B>
    where
        F: FnOnce(A) -> ParseResult<B>,
    {
        match self.result {
            Err(err) => ParseResult {
                consumed: self.consumed,
                result: Err(err),
            },
            Ok(a) => {
                let mut next = f(a);
                next.consumed = next.consumed || self.consumed;
                next
            }
        }
    }
    fn map<B, F>(self, f: F) -> ParseResult<B>
    where
        F: FnOnce(A) -> B,
    {
        ParseResult {
            consumed: self.consumed,
            result: match self.result {
                Err(err) => Err(err),
                Ok(a) => Ok(f(a)),
            },
        }
    }

    fn pure(x: A) -> Self {
        ParseResult {
            consumed: false,
            result: Ok(x),
        }
    }
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
    ($a:expr, $b:expr) => {{
        let b = $b;
        ParseResult {
            consumed: b.consumed,
            result: match b.result {
                Err(err) => Err(err),
                Ok(_) => Ok($a),
            },
        }
    };};
}

#[macro_export]
macro_rules! map2 {
    ($f:expr, $a:expr, $b:expr) => {{
        let a = $a;
        match a.result {
            Err(err) => ParseResult {
                consumed: a.consumed,
                result: Err(err),
            },
            Ok(val1) => {
                let b = $b;
                ParseResult {
                    consumed: a.consumed || b.consumed,
                    result: match b.result {
                        Err(err) => Err(err),
                        Ok(val2) => Ok($f(val1, val2)),
                    },
                }
            }
        }
    };};
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
    keep_left!(parser.module(), parser.eof()).result
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
    indentation: Vec<usize>,
    expecting: BTreeSet<TokenType>,
    current: Option<Token>,
    input: IntoIter<Token>,
}

macro_rules! many_with {
    ($vec:expr, $self:expr, $x:expr) => {{
        let mut error: Option<ParseError> = None;
        let mut acc: Vec<_> = $vec;
        let mut consumed = false;
        loop {
            let next = $x;
            match next.result {
                Err(err) => {
                    if next.consumed {
                        error = Some(err);
                    };
                    break;
                }
                Ok(val) => {
                    consumed = consumed || next.consumed;
                    acc.push(val)
                }
            }
        }
        ParseResult {
            consumed,
            result: match error {
                None => Ok(acc),
                Some(err) => Err(err),
            },
        }
    }};
}

macro_rules! many {
    ($self:expr, $x:expr) => {
        many_with!(Vec::new(), $self, $x)
    };
}

macro_rules! many_ {
    ($self:expr, $x:expr) => {{
        let mut error: Option<ParseError> = None;
        let mut consumed = false;
        loop {
            let next = $x;
            match next.result {
                Err(err) => {
                    if next.consumed {
                        error = Some(err);
                    };
                    break;
                }
                Ok(_) => {
                    consumed = consumed || next.consumed;
                }
            }
        }
        ParseResult {
            consumed,
            result: match error {
                None => Ok(()),
                Some(err) => Err(err),
            },
        }
    }};
}

macro_rules! sep_by {
    ($self:expr, $x:expr, $sep:expr) => {
        choices!(
            $self,
            $x.and_then(|first| { many_with!(vec![first], $self, keep_right!($sep, $x)) }),
            ParseResult::pure(Vec::new())
        )
    };
}

macro_rules! choices {
    ($self:expr) => {
        $self.unexpected(false)
    };
    ($self:expr, $x:expr $(, $y:expr)*) => {{
        let first = $x;
        match first.result {
            Err(err) => {
                if first.consumed {
                    ParseResult{ consumed: true, result: Err(err) }
                } else {
                    let mut rest = choices!($self $(, $y)*);
                    rest.consumed = first.consumed || rest.consumed ;
                    rest
                }
            }
            Ok(val) => ParseResult{consumed:first.consumed, result:Ok(val)},
        }
    };
}}

macro_rules! optional {
    ($self:expr, $a:expr) => {{
        let first = $a;
        match first.result {
            Err(err) => {
                if first.consumed {
                    ParseResult {
                        consumed: true,
                        result: Err(err),
                    }
                } else {
                    ParseResult::pure(None)
                }
            }
            Ok(val) => ParseResult {
                consumed: first.consumed,
                result: Ok(Some(val)),
            },
        }
    }};
}

macro_rules! spanned {
    ($self:expr, $x:expr) => {{
        let pos = $self.pos;
        $x.map(|item| syntax::Spanned { pos, item })
    }};
}

impl Parser {
    fn new(input: Vec<Token>) -> Self {
        let mut input = input.into_iter();
        let current = input.next();
        Parser {
            pos: 0,
            indentation: vec![0],
            expecting: BTreeSet::new(),
            current,
            input,
        }
    }

    fn fail<A>(&self, consumed: bool, err: ParseError) -> ParseResult<A> {
        ParseResult {
            consumed,
            result: Err(err),
        }
    }

    fn unexpected<A>(&self, consumed: bool) -> ParseResult<A> {
        self.fail(
            consumed,
            ParseError::Unexpected {
                pos: self.pos,
                expecting: self.expecting.clone(),
            },
        )
    }

    fn eof(&self) -> ParseResult<()> {
        match self.current {
            None => ParseResult::pure(()),
            Some(_) => self.unexpected(false),
        }
    }

    fn consume(&mut self) -> ParseResult<()> {
        match self.current {
            None => self.unexpected(false),
            Some(ref token) => {
                self.pos += token.token_type.length();
                self.current = self.input.next();
                self.expecting = BTreeSet::new();
                ParseResult {
                    consumed: true,
                    result: Ok(()),
                }
            }
        }
    }

    fn current_indentation(&self) -> usize {
        self.indentation[self.indentation.len() - 1]
    }

    fn space(&mut self) -> ParseResult<()> {
        self.expecting.insert(TokenType::Space);
        match self.current {
            Some(ref token) => match token.token_type {
                TokenType::Indent(n) if n > self.current_indentation() => {
                    map0!((), self.consume())
                }
                TokenType::Space => {
                    map0!((), self.consume())
                }
                _ => self.unexpected(false),
            },
            None => self.unexpected(false),
        }
    }

    fn indent(&mut self) -> ParseResult<()> {
        self.expecting.insert(TokenType::Indent(0));
        let current_indentation = self.current_indentation();
        match self.current {
            Some(ref token) => match token.token_type {
                TokenType::Indent(n) if n > current_indentation => {
                    self.indentation.push(n);
                    map0!((), self.consume())
                }
                _ => self.unexpected(false),
            },
            None => self.unexpected(false),
        }
    }

    fn spaces(&mut self) -> ParseResult<()> {
        many_!(self, self.space())
    }

    fn keyword(&mut self, expected: Keyword) -> ParseResult<()> {
        self.expecting
            .insert(TokenType::Ident(String::from(expected.to_string())));
        keep_left!(
            match self.current {
                None => self.unexpected(false),
                Some(ref actual) => match actual.token_type {
                    TokenType::Ident(ref id) => {
                        if expected.matches(id) {
                            map0!((), self.consume())
                        } else {
                            self.unexpected(false)
                        }
                    }
                    _ => self.unexpected(false),
                },
            },
            self.spaces()
        )
    }

    fn token(&mut self, expected: &TokenType) -> ParseResult<()> {
        self.expecting.insert(expected.clone());
        match self.current {
            Some(ref actual) if actual.token_type == *expected => {
                map0!((), self.consume())
            }
            _ => self.unexpected(false),
        }
    }

    fn ident(&mut self) -> ParseResult<String> {
        self.expecting.insert(TokenType::Ident(String::new()));
        match self.current {
            Some(ref token) => match token.token_type {
                TokenType::Ident(ref s) if !syntax::is_keyword(s) => {
                    let s = s.clone();
                    map0!(s, self.consume())
                }
                _ => self.unexpected(false),
            },
            None => self.unexpected(false),
        }
    }

    fn int(&mut self) -> ParseResult<u32> {
        self.expecting.insert(TokenType::Int {
            value: 0,
            length: 0,
        });
        match self.current {
            Some(ref token) => match token.token_type {
                TokenType::Int { value, length: _ } => {
                    map0!(value as u32, self.consume())
                }
                _ => self.unexpected(false),
            },
            None => self.unexpected(false),
        }
    }

    fn type_atom(&mut self) -> ParseResult<Type<String>> {
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
                self.ident().map(|s| Type::Name(s)),
                keep_right!(
                    keep_left!(self.token(&TokenType::LParen), self.spaces()),
                    keep_left!(
                        optional!(self, self.type_()).map(|m_ty| m_ty.unwrap_or(Type::Unit)),
                        self.token(&TokenType::RParen)
                    )
                )
            ),
            self.spaces()
        )
    }

    fn type_app(&mut self) -> ParseResult<Type<String>> {
        self.type_atom().and_then(|first| {
            many!(self, self.type_atom()).map(|rest| {
                rest.into_iter()
                    .fold(first, |acc, el| Type::mk_app(acc, el))
            })
        })
    }

    fn type_arrow(&mut self) -> ParseResult<Type<String>> {
        self.type_app().and_then(|a| {
            optional!(
                self,
                map2!(
                    |_, ty| ty,
                    keep_left!(self.token(&TokenType::Arrow), self.spaces()),
                    self.type_arrow()
                )
            )
            .map(|m_b| match m_b {
                None => a,
                Some(b) => Type::mk_arrow(a, b),
            })
        })
    }

    fn type_fatarrow(&mut self) -> ParseResult<Type<String>> {
        self.type_arrow().and_then(|a| {
            optional!(
                self,
                map2!(
                    |_, ty| ty,
                    keep_left!(self.token(&TokenType::FatArrow), self.spaces()),
                    self.type_fatarrow()
                )
            )
            .map(|m_b| match m_b {
                None => a,
                Some(b) => Type::mk_fatarrow(a, b),
            })
        })
    }

    fn type_(&mut self) -> ParseResult<Type<String>> {
        self.type_fatarrow()
    }

    fn newline(&mut self) -> ParseResult<()> {
        let current = self.current_indentation();
        self.expecting.insert(TokenType::Indent(current));
        match self.current {
            None => self.unexpected(false),
            Some(ref token) => match token.token_type {
                TokenType::Indent(n) if n == current => {
                    map0!((), self.consume())
                }
                _ => self.unexpected(false),
            },
        }
    }

    fn pattern(&mut self) -> ParseResult<Pattern> {
        keep_left!(
            choices!(
                self,
                spanned!(self, self.ident()).map(|s| Pattern::Name(s)),
                map0!(Pattern::Wildcard, self.token(&TokenType::Underscore))
            ),
            self.spaces()
        )
    }

    fn expr_atom(&mut self) -> ParseResult<Spanned<Expr>> {
        keep_left!(
            spanned!(
                self,
                choices!(
                    self,
                    self.int().map(|n| Expr::Int(n)),
                    self.ident().map(|n| Expr::Var(n)),
                    keep_right!(
                        keep_left!(self.token(&TokenType::LParen), self.spaces()),
                        keep_left!(
                            optional!(self, self.expr()).map(|m_ty| match m_ty {
                                None => Expr::Unit,
                                Some(ty) => ty.item,
                            }),
                            self.token(&TokenType::RParen)
                        )
                    )
                )
            ),
            self.spaces()
        )
    }

    fn branch(&mut self) -> ParseResult<Branch> {
        keep_left!(spanned!(self, self.pattern()), self.spaces()).and_then(|pattern| {
            map2!(
                |_, body| Branch { pattern, body },
                keep_left!(self.token(&TokenType::Arrow), self.spaces()),
                self.expr()
            )
        })
    }

    fn expr_case(&mut self) -> ParseResult<Spanned<Expr>> {
        spanned!(
            self,
            keep_left!(
                self.token(&TokenType::Ident(String::from("case"))),
                self.spaces()
            )
            .and_then(|_| {
                self.expr().and_then(|cond| {
                    keep_left!(
                        self.token(&TokenType::Ident(String::from("of"))),
                        many_!(self, self.token(&TokenType::Space))
                    )
                    .and_then(|_| {
                        keep_right!(self.indent(), sep_by!(self, self.branch(), self.newline()))
                            .map(|branches| {
                                self.indentation.pop();

                                Expr::mk_case(cond, branches)
                            })
                    })
                })
            })
        )
    }

    fn expr_app(&mut self) -> ParseResult<Spanned<Expr>> {
        self.expr_atom().and_then(|first| {
            many!(self, self.expr_atom()).map(|rest| {
                rest.into_iter()
                    .fold(first, |acc, el| Expr::mk_app(acc, el))
            })
        })
    }

    fn expr(&mut self) -> ParseResult<Spanned<Expr>> {
        choices!(self, self.expr_app(), self.expr_case())
    }

    fn definition(&mut self) -> ParseResult<Declaration> {
        keep_left!(self.ident(), self.spaces()).and_then(|name| {
            keep_left!(self.token(&TokenType::Colon), self.spaces()).and_then(|_| {
                keep_left!(self.type_(), self.newline()).and_then(|ty| {
                    keep_right!(
                        keep_left!(self.token(&TokenType::Ident(name.clone())), self.spaces()),
                        many!(self, self.pattern())
                    )
                    .and_then(|args| {
                        keep_left!(self.token(&TokenType::Equals), self.spaces()).and_then(|_| {
                            self.expr().map(|body| Declaration::Definition {
                                name,
                                ty,
                                args,
                                body,
                            })
                        })
                    })
                })
            })
        })
    }

    fn type_alias(&mut self) -> ParseResult<Declaration> {
        keep_right!(
            keep_left!(self.keyword(Keyword::Type), self.spaces()),
            keep_left!(self.ident(), self.spaces()).and_then(|name| many!(
                self,
                keep_left!(self.ident(), self.spaces())
            )
            .and_then(|args| keep_right!(
                keep_left!(self.token(&TokenType::Equals), self.spaces()),
                self.type_()
                    .map(|body| Declaration::TypeAlias { name, args, body })
            )))
        )
    }

    fn import(&mut self) -> ParseResult<Declaration> {
        keep_left!(self.keyword(Keyword::Import), self.spaces()).and_then(|_| {
            keep_left!(self.ident(), self.spaces()).and_then(|module| {
                optional!(
                    self,
                    keep_left!(self.keyword(Keyword::As), self.spaces())
                        .and_then(|_| keep_left!(self.ident(), self.spaces()))
                )
                .map(|name| Declaration::Import { module, name })
            })
        })
    }

    fn from_import(&mut self) -> ParseResult<Declaration> {
        keep_right!(
            keep_left!(self.keyword(Keyword::From), self.spaces()),
            keep_left!(self.ident(), self.spaces()).and_then(|module| keep_right!(
                keep_left!(self.keyword(Keyword::Import), self.spaces()),
                choices!(
                    self,
                    keep_left!(
                        map0!(Names::All, self.token(&TokenType::Asterisk)),
                        self.spaces()
                    ),
                    sep_by!(
                        self,
                        keep_left!(self.ident(), self.spaces()),
                        keep_left!(self.token(&TokenType::Comma), self.spaces())
                    )
                    .map(|names| Names::Names(names))
                )
                .map(|names| Declaration::FromImport { module, names })
            ))
        )
    }

    fn declaration(&mut self) -> ParseResult<Declaration> {
        choices!(
            self,
            self.definition(),
            self.type_alias(),
            self.import(),
            self.from_import()
        )
    }

    fn module(&mut self) -> ParseResult<Module> {
        many!(self, spanned!(self, self.declaration())).map(|decls| Module { decls })
    }
}
