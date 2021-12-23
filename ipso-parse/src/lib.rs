pub mod grammar;
pub mod indentation;
mod test;

use fixedbitset::FixedBitSet;
use fnv::FnvHashSet;
use ipso_diagnostic::{Diagnostic, Location, Message, Source};
use ipso_lex::{
    token::{self, Relation, Token},
    Lexer,
};
use ipso_syntax::{self as syntax, Keyword, Module};
use std::{
    collections::BTreeSet,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
    rc::Rc,
    vec,
};

use crate::grammar::module::module;

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    Unexpected {
        source: Source,
        pos: usize,
        expecting: BTreeSet<token::Name>,
    },
}

impl ParseError {
    fn source(&self) -> Source {
        match self {
            ParseError::Unexpected { source, .. } => source.clone(),
        }
    }

    fn position(&self) -> usize {
        match self {
            ParseError::Unexpected { pos, .. } => *pos,
        }
    }

    fn message(&self) -> String {
        match self {
            ParseError::Unexpected { expecting, .. } => {
                let mut str = String::from("expected one of: ");
                let mut iter = expecting.iter();
                match iter.next() {
                    None => str,
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

    pub fn report(&self, diagnostic: &mut Diagnostic) {
        diagnostic.item(
            Some(Location {
                source: self.source(),
                offset: Some(self.position()),
            }),
            Message {
                content: self.message(),
                addendum: None,
            },
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseResult<A> {
    pub consumed: bool,
    pub result: Option<A>,
}

impl<A> ParseResult<A> {
    pub fn and_then<B, F>(self, f: F) -> ParseResult<B>
    where
        F: FnOnce(A) -> ParseResult<B>,
    {
        match self.result {
            None => ParseResult {
                consumed: self.consumed,
                result: None,
            },
            Some(a) => {
                let mut next = f(a);
                next.consumed = next.consumed || self.consumed;
                next
            }
        }
    }

    pub fn map<B, F>(self, f: F) -> ParseResult<B>
    where
        F: FnOnce(A) -> B,
    {
        ParseResult {
            consumed: self.consumed,
            result: self.result.map(f),
        }
    }

    fn pure(x: A) -> Self {
        ParseResult {
            consumed: false,
            result: Some(x),
        }
    }
}

#[macro_export]
macro_rules! apply {
    ($a:expr, $b:expr) => {
        match $a {
            None => None,
            Some(f) => match $b {
                None => None
                Some(x) => Some(f(x)),
            },
        }
    };
}

#[macro_export]
macro_rules! map0 {
    ($a:expr, $b:expr) => {{
        let b = $b;
        ParseResult {
            consumed: b.consumed,
            result: b.result.map(|_| $a),
        }
    }};
}

#[macro_export]
macro_rules! map2 {
    ($f:expr, $a:expr, $b:expr) => {{
        use $crate::ParseResult;

        let a = $a;
        match a.result {
            None => ParseResult {
                consumed: a.consumed,
                result: None,
            },
            Some(val1) => {
                let b = $b;
                ParseResult {
                    consumed: a.consumed || b.consumed,
                    result: b.result.map(|val2| $f(val1, val2)),
                }
            }
        }
    }};
}

#[macro_export]
macro_rules! keep_right {
    ($a:expr, $b:expr) => {{
        use crate::map2;
        map2!(|_, a| a, $a, $b)
    }};
}

#[macro_export]
macro_rules! keep_left {
    ($a:expr, $b:expr) => {{
        use $crate::map2;
        map2!(|a, _| a, $a, $b)
    }};
}

#[macro_export]
macro_rules! between {
    ($l:expr, $r:expr, $x:expr) => {{
        use crate::{keep_left, keep_right};
        keep_right!($l, keep_left!($x, $r))
    }};
}

#[macro_export]
macro_rules! parse_string {
    ($p:ident, $s:expr) => {{
        use ipso_diagnostic::Source;
        use ipso_lex::{token::Token, Lexer};
        use ipso_parse::{keep_left, map2, Parser};

        let mut parser: Parser = Parser::new(
            Source::Interactive {
                label: String::from("(string)"),
            },
            Lexer::new(&$s),
        );
        let result = keep_left!(parser.$p(), parser.eof());
        parser.into_parse_error(result.result)
    }};
}

#[macro_export]
macro_rules! parse_str {
    ($p:ident, $s:expr) => {{
        use ipso_parse::parse_string;
        let s = String::from($s);
        parse_string!($p, s)
    }};
}

pub fn parse_string_at(source: Source, input: String) -> Result<Module, ParseError> {
    let mut parser: Parser = Parser::new(source, Lexer::new(&input));
    let result = keep_left!(module(&mut parser), parser.eof());
    parser.into_parse_error(result.result)
}

pub fn parse_string(input: String) -> Result<Module, ParseError> {
    let source = Source::Interactive {
        label: String::from("(string)"),
    };
    parse_string_at(source, input)
}

pub fn parse_file(filename: &Path) -> Result<Module, ParseError> {
    let input: String = {
        let mut content = String::new();
        let mut file: File = File::open(filename).unwrap();
        file.read_to_string(&mut content).unwrap();
        content
    };
    let source = Source::File {
        path: PathBuf::from(filename),
    };
    parse_string_at(source, input)
}

struct Expecting {
    bitset: FixedBitSet,
    indents: FnvHashSet<(Relation, usize)>,
}

impl Expecting {
    fn new() -> Self {
        Expecting {
            bitset: FixedBitSet::with_capacity(token::Name::num_variants()),
            indents: FnvHashSet::with_hasher(Default::default()),
        }
    }

    fn clear(&mut self) {
        self.bitset.clear();
        self.clear_indents();
    }

    fn clear_indents(&mut self) {
        self.indents.clear();
    }

    fn insert(&mut self, t: token::Name) {
        self.bitset.insert(t.to_int());
        if let token::Name::Indent(relation, n) = t {
            self.indents.insert((relation, n));
        }
    }

    fn into_btreeset(self) -> BTreeSet<token::Name> {
        let mut set = BTreeSet::new();
        let mut has_indents = false;
        for ix in self.bitset.ones() {
            if ix == token::Name::num_variants() - 3
            /* Indent(_) */
            {
                has_indents = true;
            } else {
                set.insert(token::Name::from_int(ix).unwrap());
            }
        }
        if has_indents {
            set.extend(
                self.indents
                    .into_iter()
                    .map(|(relation, amount)| token::Name::Indent(relation, amount)),
            );
        }
        set
    }
}

pub struct Parser<'input> {
    source: Source,
    pos: usize,
    column: usize,
    indentation: Vec<usize>,
    expecting: Expecting,
    current: Option<Token>,
    input: Lexer<'input>,
}

#[macro_export]
macro_rules! many_ {
    ($self:expr, $x:expr) => {{
        let mut error: bool = false;
        let mut consumed = false;
        loop {
            let next = $x;
            match next.result {
                None => {
                    if next.consumed {
                        error = true;
                    };
                    break;
                }
                Some(_) => {
                    consumed = consumed || next.consumed;
                }
            }
        }
        ParseResult {
            consumed,
            result: match error {
                false => Some(()),
                true => None,
            },
        }
    }};
}

#[macro_export]
macro_rules! many_with {
    ($vec:expr, $self:expr, $x:expr) => {{
        let mut error: bool = false;
        let mut acc: Vec<_> = $vec;
        let mut consumed = false;
        loop {
            let next = $x;
            match next.result {
                None => {
                    if next.consumed {
                        error = true;
                    };
                    break;
                }
                Some(val) => {
                    consumed = consumed || next.consumed;
                    acc.push(val)
                }
            }
        }
        ParseResult {
            consumed,
            result: match error {
                false => Some(acc),
                true => None,
            },
        }
    }};
}

#[macro_export]
macro_rules! many {
    ($self:expr, $x:expr) => {{
        use crate::many_with;
        many_with!(Vec::new(), $self, $x)
    }};
}

#[macro_export]
macro_rules! sep_by {
    ($self:expr, $x:expr, $sep:expr) => {{
        use crate::many_with;
        choices!(
            $self,
            $x.and_then(|first| { many_with!(vec![first], $self, keep_right!($sep, $x)) }),
            ParseResult::pure(Vec::new())
        )
    }};
}

#[macro_export]
macro_rules! choices {
    ($self:expr) => {
        $self.unexpected(false)
    };
    ($self:expr, $x:expr $(, $y:expr)*) => {{
        let first = $x;
        match first.result {
            None => {
                if first.consumed {
                    ParseResult{ consumed: true, result: None }
                } else {
                    let mut rest = choices!($self $(, $y)*);
                    rest.consumed = first.consumed || rest.consumed ;
                    rest
                }
            }
            Some(val) => ParseResult{consumed: first.consumed, result: Some(val)},
        }
    }}
}

#[macro_export]
macro_rules! optional {
    ($self:expr, $a:expr) => {{
        let first = $a;
        match first.result {
            None => {
                if first.consumed {
                    ParseResult {
                        consumed: true,
                        result: None,
                    }
                } else {
                    ParseResult::pure(None)
                }
            }
            Some(val) => ParseResult {
                consumed: first.consumed,
                result: Some(Some(val)),
            },
        }
    }};
}

#[macro_export]
macro_rules! spanned {
    ($self:expr, $x:expr) => {{
        use ipso_syntax as syntax;
        let pos = $self.pos;
        $x.map(|item| syntax::Spanned { pos, item })
    }};
}

impl<'input> Parser<'input> {
    pub fn new(source: Source, mut input: Lexer<'input>) -> Self {
        let current = input.next();
        Parser {
            source,
            pos: 0,
            column: 0,
            indentation: vec![],
            expecting: Expecting::new(),
            current,
            input,
        }
    }

    pub fn into_parse_error<A>(self, result: Option<A>) -> Result<A, ParseError> {
        match result {
            Some(a) => Ok(a),
            None => Err(ParseError::Unexpected {
                source: self.source,
                pos: self.pos,
                expecting: self.expecting.into_btreeset(),
            }),
        }
    }

    fn unexpected<A>(&self, consumed: bool) -> ParseResult<A> {
        ParseResult {
            consumed,
            result: None,
        }
    }

    pub fn eof(&mut self) -> ParseResult<()> {
        self.token(&token::Data::Eof)
    }

    fn consume(&mut self) -> ParseResult<()> {
        match &self.current {
            None => self.unexpected(false),
            Some(_) => {
                self.current = self.input.next();
                match &self.current {
                    None => {}
                    Some(token) => {
                        self.pos = token.pos;
                        self.column = token.column;
                    }
                };
                self.expecting.clear();
                ParseResult {
                    consumed: true,
                    result: Some(()),
                }
            }
        }
    }

    fn comment(&mut self) -> ParseResult<()> {
        self.expecting.insert(token::Name::Comment);
        match &self.current {
            None => self.unexpected(false),
            Some(token) => match token.data {
                token::Data::Comment { .. } => map0!((), self.consume()),
                _ => self.unexpected(false),
            },
        }
    }

    fn keyword(&mut self, expected: &Keyword) -> ParseResult<()> {
        self.expecting.insert(token::Name::Keyword(*expected));
        match &self.current {
            None => self.unexpected(false),
            Some(actual) => match &actual.data {
                token::Data::Ident(id) => {
                    if expected.matches(id) {
                        map0!((), self.consume())
                    } else {
                        self.unexpected(false)
                    }
                }
                _ => self.unexpected(false),
            },
        }
    }

    fn token(&mut self, expected: &token::Data) -> ParseResult<()> {
        self.expecting.insert(expected.name());
        match &self.current {
            Some(actual) if actual.data == *expected => self
                .consume()
                .and_then(|_| map0!((), many_!(self, self.comment()))),
            _ => self.unexpected(false),
        }
    }

    fn ident(&mut self) -> ParseResult<Rc<str>> {
        self.expecting.insert(token::Name::Ident);
        match &self.current {
            Some(token) => match &token.data {
                token::Data::Ident(s) if !syntax::is_keyword(s) => match s.chars().next() {
                    Some(c) if c.is_lowercase() => {
                        let s = s.clone();
                        self.consume()
                            .and_then(|_| map0!(s, many_!(self, self.comment())))
                    }
                    _ => self.unexpected(false),
                },
                _ => self.unexpected(false),
            },
            None => self.unexpected(false),
        }
    }

    fn ident_owned(&mut self) -> ParseResult<String> {
        self.ident().map(|i| String::from(i.as_ref()))
    }

    fn ctor(&mut self) -> ParseResult<Rc<str>> {
        self.expecting.insert(token::Name::Ctor);
        match &self.current {
            Some(token) => match &token.data {
                token::Data::Ident(s) if !syntax::is_keyword(s) => match s.chars().next() {
                    Some(c) if c.is_uppercase() => {
                        let s = s.clone();
                        self.consume()
                            .and_then(|_| map0!(s, many_!(self, self.comment())))
                    }
                    _ => self.unexpected(false),
                },
                _ => self.unexpected(false),
            },
            None => self.unexpected(false),
        }
    }

    fn ctor_owned(&mut self) -> ParseResult<String> {
        self.ctor().map(|s| String::from(s.as_ref()))
    }

    fn int(&mut self) -> ParseResult<u32> {
        self.expecting.insert(token::Name::Int);
        match self.current {
            Some(ref token) => match token.data {
                token::Data::Int { value, length: _ } => {
                    map0!(value as u32, self.consume())
                }
                _ => self.unexpected(false),
            },
            None => self.unexpected(false),
        }
    }

    /// ```
    /// use std::rc::Rc;
    /// use ipso_diagnostic::Source;
    /// use ipso_lex::{self, token};
    /// use ipso_parse::{ParseError, parse_str};
    ///
    /// assert_eq!(parse_str!(char, "\'a\'"), Ok('a'));
    ///
    /// assert_eq!(parse_str!(char, "\'\\\\\'"), Ok('\\'));
    ///
    /// assert_eq!(parse_str!(char, "\'\\n\'"), Ok('\n'));
    ///
    /// assert_eq!(parse_str!(char, "'"), Err(ParseError::Unexpected {
    ///     source: Source::Interactive{label: String::from("(string)")},
    ///     pos: 1,
    ///     expecting: vec![token::Name::Char, token::Name::Comment].into_iter().collect(),
    /// }));
    ///
    /// assert_eq!(parse_str!(char, "\'\\\'"), Err(ParseError::Unexpected {
    ///     source: Source::Interactive{label: String::from("(string)")},
    ///     pos: 3,
    ///     expecting: vec![token::Name::SingleQuote].into_iter().collect(),
    /// }));
    ///
    /// assert_eq!(parse_str!(char, "\'\\"), Err(ParseError::Unexpected {
    ///     source: Source::Interactive{label: String::from("(string)")},
    ///     pos: 1,
    ///     expecting: vec![token::Name::Char, token::Name::Comment].into_iter().collect(),
    /// }));
    ///
    /// assert_eq!(parse_str!(char, "\'\\~\'"), Err(ParseError::Unexpected {
    ///     source: Source::Interactive{label: String::from("(string)")},
    ///     pos: 2,
    ///     expecting: vec![token::Name::Char, token::Name::Comment].into_iter().collect(),
    /// }));
    /// ```
    pub fn char(&mut self) -> ParseResult<char> {
        between!(
            self.token(&token::Data::SingleQuote),
            self.token(&token::Data::SingleQuote),
            {
                self.expecting.insert(token::Name::Char);
                match &self.current {
                    Some(token) => match token.data {
                        token::Data::Char { value, length: _ } => {
                            map0!(value, self.consume())
                        }
                        _ => self.unexpected(false),
                    },
                    None => self.unexpected(false),
                }
            }
        )
    }
}
