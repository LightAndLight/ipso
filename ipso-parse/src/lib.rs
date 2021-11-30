mod test;

use fixedbitset::FixedBitSet;
use fnv::FnvHashSet;
use ipso_diagnostic::{Diagnostic, Location, Message, Source};
use ipso_lex::{
    token::{self, Token},
    Lexer,
};
use ipso_syntax::{
    self as syntax, r#type::Type, Branch, CompLine, Declaration, Expr, Keyword, Module, Names,
    Pattern, Spanned, StringPart,
};
use std::{
    cmp,
    collections::BTreeSet,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
    rc::Rc,
    vec,
};

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
                offset: self.position(),
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
            result: b.result.map(|_| $a),
        }
    }};
}

#[macro_export]
macro_rules! map2 {
    ($f:expr, $a:expr, $b:expr) => {{
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

macro_rules! between {
    ($l:expr, $r:expr, $x:expr) => {
        keep_right!($l, keep_left!($x, $r))
    };
}

#[macro_export]
macro_rules! parse_string {
    ($p:ident, $s:expr) => {{
        use ipso_diagnostic::Source;
        use ipso_lex::{token::Token, Lexer};
        use ipso_parse::{keep_left, map2, ParseResult, Parser};

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
    let result = keep_left!(parser.module(), parser.eof());
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
    indents: FnvHashSet<usize>,
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
        self.indents.clear();
    }

    fn insert(&mut self, t: token::Name) {
        self.bitset.insert(t.to_int());
        if let token::Name::Indent(n) = t {
            self.indents.insert(n);
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
            set.extend(self.indents.into_iter().map(token::Name::Indent));
        }
        set
    }
}

pub struct Parser<'input> {
    source: Source,
    pos: usize,
    indentation: Vec<usize>,
    expecting: Expecting,
    current: Option<Token>,
    input: Lexer<'input>,
}

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

macro_rules! many {
    ($self:expr, $x:expr) => {
        many_with!(Vec::new(), $self, $x)
    };
}

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

macro_rules! some_ {
    ($self:expr, $x:expr) => {{
        $x.and_then(|_| many_!($self, $x))
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

macro_rules! spanned {
    ($self:expr, $x:expr) => {{
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
            indentation: vec![0],
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

    pub fn eof(&self) -> ParseResult<()> {
        match self.current {
            None => ParseResult::pure(()),
            Some(_) => self.unexpected(false),
        }
    }

    fn consume(&mut self) -> ParseResult<()> {
        match self.current {
            None => self.unexpected(false),
            Some(ref token) => {
                self.pos += token.data.length();
                self.current = self.input.next();
                self.expecting.clear();
                ParseResult {
                    consumed: true,
                    result: Some(()),
                }
            }
        }
    }

    fn current_indentation(&self) -> usize {
        self.indentation[self.indentation.len() - 1]
    }

    fn space_body(&mut self) -> ParseResult<()> {
        match self.current {
            Some(ref token) => match token.data {
                token::Data::Indent(n) if n > self.current_indentation() => {
                    map0!((), self.consume())
                }
                token::Data::Space => {
                    map0!((), self.consume())
                }
                _ => self.unexpected(false),
            },
            None => self.unexpected(false),
        }
    }

    fn space(&mut self) -> ParseResult<()> {
        self.expecting.insert(token::Name::Space);
        self.space_body()
    }

    fn indent(&mut self) -> ParseResult<()> {
        self.expecting.insert(token::Name::Indent(0));
        let current_indentation = self.current_indentation();
        match self.current {
            Some(ref token) => match token.data {
                token::Data::Indent(n) if n > current_indentation => {
                    self.indentation.push(n);
                    map0!((), self.consume())
                }
                _ => self.unexpected(false),
            },
            None => self.unexpected(false),
        }
    }

    fn dedent(&mut self) -> ParseResult<()> {
        self.expecting.insert(token::Name::Dedent);
        let dedent_to = match &self.current {
            Some(token) => match token.data {
                token::Data::Indent(n) => Some(n),
                _ => None,
            },
            None => Some(0),
        };
        match dedent_to {
            None => self.unexpected(false),
            Some(n) => match self.indentation.last() {
                None => panic!("dedent: indentation is empty"),
                Some(top) => match n.cmp(top) {
                    cmp::Ordering::Less => {
                        self.indentation.pop();
                        ParseResult::pure(())
                    }
                    cmp::Ordering::Equal => self.unexpected(false),
                    cmp::Ordering::Greater => self.unexpected(false),
                },
            },
        }
    }

    fn comment_body(&mut self) -> ParseResult<()> {
        match self.current {
            None => self.unexpected(false),
            Some(ref token) => match token.data {
                token::Data::Comment { .. } => map0!((), self.consume()),
                _ => self.unexpected(false),
            },
        }
    }

    fn comment(&mut self) -> ParseResult<()> {
        self.expecting.insert(token::Name::Comment);
        self.comment_body()
    }

    fn spaces(&mut self) -> ParseResult<()> {
        many_!(self, choices!(self, self.space(), self.comment()))
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
        match self.current {
            Some(ref actual) if actual.data == *expected => {
                map0!((), self.consume())
            }
            _ => self.unexpected(false),
        }
    }

    fn ident(&mut self) -> ParseResult<Rc<str>> {
        self.expecting.insert(token::Name::Ident);
        match self.current {
            Some(ref token) => match &token.data {
                token::Data::Ident(s) if !syntax::is_keyword(s) => match s.chars().next() {
                    Some(c) if c.is_lowercase() => {
                        let s = s.clone();
                        map0!(s, self.consume())
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
        match self.current {
            Some(ref token) => match token.data {
                token::Data::Ident(ref s) if !syntax::is_keyword(s) => match s.chars().next() {
                    Some(c) if c.is_uppercase() => {
                        let s = s.clone();
                        map0!(s, self.consume())
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
    /// assert_eq!(parse_str!(char, "\'\\\'"), Err(ParseError::Unexpected {
    ///     source: Source::Interactive{label: String::from("(string)")},
    ///     pos: 3,
    ///     expecting: vec![token::Name::SingleQuote].into_iter().collect(),
    /// }));
    ///
    /// assert_eq!(parse_str!(char, "\'\\"), Err(ParseError::Unexpected {
    ///     source: Source::Interactive{label: String::from("(string)")},
    ///     pos: 1,
    ///     expecting: vec![token::Name::Char].into_iter().collect(),
    /// }));
    ///
    /// assert_eq!(parse_str!(char, "\'\\~\'"), Err(ParseError::Unexpected {
    ///     source: Source::Interactive{label: String::from("(string)")},
    ///     pos: 1,
    ///     expecting: vec![token::Name::Char].into_iter().collect(),
    /// }));
    /// ```
    pub fn char(&mut self) -> ParseResult<char> {
        between!(
            self.token(&token::Data::SingleQuote),
            self.token(&token::Data::SingleQuote),
            {
                self.expecting.insert(token::Name::Char);
                match self.current {
                    Some(ref token) => match token.data {
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

    /*
    type_record ::=
      '{' record_fields '}'

    type_record_fields ::=
      [ident [':' type [',' record_fields]]]
     */
    fn type_record(&mut self) -> ParseResult<Type<Rc<str>>> {
        fn type_record_fields(
            parser: &mut Parser,
            fields: &mut Vec<(Rc<str>, Type<Rc<str>>)>,
        ) -> ParseResult<Option<Type<Rc<str>>>> {
            optional!(self, keep_left!(parser.ident(), parser.spaces())).and_then(|m_ident| {
                match m_ident {
                    None => ParseResult::pure(None),
                    Some(ident) => optional!(
                        self,
                        keep_right!(
                            keep_left!(parser.token(&token::Data::Colon), parser.spaces()),
                            parser.type_()
                        )
                    )
                    .and_then(|m_ty| match m_ty {
                        None => ParseResult::pure(Some(Type::Var(ident))),
                        Some(ty) => {
                            fields.push((ident, ty));
                            optional!(
                                parser,
                                keep_right!(
                                    keep_left!(parser.token(&token::Data::Comma), parser.spaces()),
                                    type_record_fields(parser, fields)
                                )
                            )
                            .map(|m_rest| match m_rest {
                                None => None,
                                Some(rest) => rest,
                            })
                        }
                    }),
                }
            })
        }
        keep_right!(
            keep_left!(self.token(&token::Data::LBrace), self.spaces()),
            keep_left!(
                {
                    let mut fields = Vec::new();
                    type_record_fields(self, &mut fields).map(|rest| Type::mk_record(fields, rest))
                },
                self.token(&token::Data::RBrace)
            )
        )
    }

    /*
    type_variant ::=
      '<' [type_variant_ctors] '>'

    type_variant_ctors ::=
      ident
      ctor ':' type ['|' type_variant_ctors]
     */
    fn type_variant(&mut self) -> ParseResult<Type<Rc<str>>> {
        fn type_variant_ctors(
            parser: &mut Parser,
            ctors: &mut Vec<(Rc<str>, Type<Rc<str>>)>,
        ) -> ParseResult<Option<Type<Rc<str>>>> {
            choices!(
                parser,
                keep_left!(parser.ident(), parser.spaces()).map(|x| Some(Type::Var(x))),
                keep_left!(parser.ctor(), parser.spaces()).and_then(|ctor| keep_left!(
                    parser.token(&token::Data::Colon),
                    parser.spaces()
                )
                .and_then(|_| parser.type_().and_then(|ty| {
                    ctors.push((ctor, ty));
                    optional!(
                        parser,
                        keep_left!(parser.token(&token::Data::Pipe), parser.spaces())
                            .and_then(|_| type_variant_ctors(parser, ctors))
                    )
                    .map(|m_rest| match m_rest {
                        None => None,
                        Some(rest) => rest,
                    })
                })))
            )
        }

        between!(
            keep_left!(self.token(&token::Data::LAngle), self.spaces()),
            self.token(&token::Data::RAngle),
            {
                let mut ctors = Vec::new();
                optional!(self, type_variant_ctors(self, &mut ctors)).map(|m_rest| match m_rest {
                    None => Type::mk_variant(Vec::new(), None),
                    Some(rest) => Type::mk_variant(ctors, rest),
                })
            }
        )
    }

    fn type_atom(&mut self) -> ParseResult<Type<Rc<str>>> {
        keep_left!(
            choices!(
                self,
                map0!(
                    Type::Bool,
                    self.token(&token::Data::Ident(Rc::from("Bool")))
                ),
                map0!(Type::Int, self.token(&token::Data::Ident(Rc::from("Int")))),
                map0!(
                    Type::Char,
                    self.token(&token::Data::Ident(Rc::from("Char")))
                ),
                map0!(
                    Type::String,
                    self.token(&token::Data::Ident(Rc::from("String")))
                ),
                map0!(
                    Type::Array,
                    self.token(&token::Data::Ident(Rc::from("Array")))
                ),
                map0!(Type::IO, self.token(&token::Data::Ident(Rc::from("IO")))),
                self.type_record(),
                self.type_variant(),
                self.ctor().map(Type::Name),
                self.ident().map(Type::Var),
                keep_right!(
                    keep_left!(self.token(&token::Data::LParen), self.spaces()),
                    keep_left!(
                        optional!(self, self.type_()).map(|m_ty| m_ty.unwrap_or(Type::Unit)),
                        self.token(&token::Data::RParen)
                    )
                )
            ),
            self.spaces()
        )
    }

    fn type_app(&mut self) -> ParseResult<Type<Rc<str>>> {
        self.type_atom().and_then(|first| {
            many!(self, self.type_atom()).map(|rest| rest.into_iter().fold(first, Type::mk_app))
        })
    }

    fn type_arrow(&mut self) -> ParseResult<Type<Rc<str>>> {
        self.type_app().and_then(|a| {
            optional!(
                self,
                map2!(
                    |_, ty| ty,
                    keep_left!(self.token(&token::Data::Arrow), self.spaces()),
                    self.type_arrow()
                )
            )
            .map(|m_b| match m_b {
                None => a,
                Some(b) => Type::mk_arrow(a, b),
            })
        })
    }

    fn type_fatarrow(&mut self) -> ParseResult<Type<Rc<str>>> {
        self.type_arrow().and_then(|a| {
            optional!(
                self,
                map2!(
                    |_, ty| ty,
                    keep_left!(self.token(&token::Data::FatArrow), self.spaces()),
                    self.type_fatarrow()
                )
            )
            .map(|m_b| match m_b {
                None => a,
                Some(b) => Type::mk_fatarrow(a, b),
            })
        })
    }

    fn type_(&mut self) -> ParseResult<Type<Rc<str>>> {
        self.type_fatarrow()
    }

    fn newline(&mut self) -> ParseResult<()> {
        keep_right!(optional!(self, self.comment()), {
            let current = self.current_indentation();
            self.expecting.insert(token::Name::Indent(current));
            match self.current {
                None => self.unexpected(false),
                Some(ref token) => match token.data {
                    token::Data::Indent(n) if n == current => {
                        map0!((), self.consume())
                    }
                    _ => self.unexpected(false),
                },
            }
        })
    }

    fn comp_line_bind(&mut self) -> ParseResult<CompLine> {
        keep_left!(self.keyword(&Keyword::Bind), self.spaces()).and_then(|_| {
            keep_left!(self.ident(), self.spaces()).and_then(|name| {
                keep_left!(self.token(&token::Data::LeftArrow), self.spaces())
                    .and_then(|_| self.expr().map(|value| CompLine::Bind(name, value)))
            })
        })
    }

    fn comp_line_return(&mut self) -> ParseResult<CompLine> {
        keep_left!(self.keyword(&Keyword::Return), self.spaces())
            .and_then(|_| self.expr().map(CompLine::Return))
    }

    /**
    comp_line ::=
      'bind' ident '<-' expr
      'return' expr
      expr
    */
    fn comp_line(&mut self) -> ParseResult<CompLine> {
        choices!(
            self,
            self.comp_line_bind(),
            self.comp_line_return(),
            self.expr().map(CompLine::Expr)
        )
    }

    fn expr_comp(&mut self) -> ParseResult<Spanned<Expr>> {
        spanned!(
            self,
            keep_left!(self.keyword(&Keyword::Comp), self.spaces()).and_then(|_| between!(
                keep_left!(
                    self.token(&token::Data::LBrace),
                    many_!(self, self.token(&token::Data::Space))
                ),
                keep_right!(self.spaces(), self.token(&token::Data::RBrace)),
                choices!(
                    self,
                    between!(
                        self.indent(),
                        self.dedent(),
                        sep_by!(self, self.comp_line(), self.newline()).map(Expr::Comp)
                    ),
                    ParseResult::pure(Expr::Comp(Vec::new()))
                )
            ))
        )
    }

    /*
    pattern_record ::=
      '{' [pattern_record_fields] '}'

    pattern_record_fields ::=
      ident [',' pattern_record_fields]
      '..' ident
     */
    fn pattern_record(&mut self) -> ParseResult<Pattern> {
        fn pattern_record_fields(
            parser: &mut Parser,
            names: &mut Vec<Spanned<String>>,
        ) -> ParseResult<Option<Spanned<String>>> {
            choices!(
                parser,
                keep_left!(spanned!(parser, parser.ident_owned()), parser.spaces()).and_then(
                    |name| {
                        names.push(name);
                        optional!(
                            parser,
                            keep_right!(
                                keep_left!(parser.token(&token::Data::Comma), parser.spaces()),
                                pattern_record_fields(parser, names)
                            )
                        )
                        .map(|m_rest| match m_rest {
                            None => None,
                            Some(rest) => rest,
                        })
                    }
                ),
                keep_right!(
                    keep_left!(
                        keep_left!(
                            parser.token(&token::Data::Dot),
                            parser.token(&token::Data::Dot)
                        ),
                        parser.spaces()
                    ),
                    keep_left!(spanned!(parser, parser.ident_owned()), parser.spaces()).map(Some)
                )
            )
        }

        keep_right!(
            keep_left!(self.token(&token::Data::LBrace), self.spaces()),
            keep_left!(
                {
                    let mut names = Vec::new();
                    pattern_record_fields(self, &mut names)
                        .map(|rest| Pattern::Record { names, rest })
                },
                keep_left!(self.token(&token::Data::RBrace), self.spaces())
            )
        )
    }

    fn pattern_variant(&mut self) -> ParseResult<Pattern> {
        keep_left!(self.ctor_owned(), self.spaces()).and_then(|name| {
            spanned!(self, self.ident_owned()).map(|arg| Pattern::Variant { name, arg })
        })
    }

    fn pattern(&mut self) -> ParseResult<Pattern> {
        keep_left!(
            choices!(
                self,
                spanned!(self, self.ident_owned()).map(Pattern::Name),
                self.pattern_record(),
                self.pattern_variant(),
                map0!(Pattern::Wildcard, self.token(&token::Data::Underscore))
            ),
            self.spaces()
        )
    }

    fn string_part_expr(&mut self) -> ParseResult<StringPart> {
        choices!(
            self,
            keep_right!(
                keep_left!(self.token(&token::Data::DollarLBrace), self.spaces()),
                keep_left!(
                    self.expr().map(StringPart::Expr),
                    self.token(&token::Data::RBrace)
                )
            ),
            keep_right!(
                self.token(&token::Data::Dollar),
                spanned!(self, self.ident_owned().map(Expr::Var)).map(StringPart::Expr)
            )
        )
    }

    fn string_part_string(&mut self) -> ParseResult<StringPart> {
        self.expecting.insert(token::Name::String);
        let str = match &self.current {
            Some(current) => match &current.data {
                token::Data::String { value, .. } => value.clone(),
                _ => return self.unexpected(false),
            },
            None => return self.unexpected(false),
        };
        self.consume();
        ParseResult::pure(StringPart::String(str))
    }

    fn string(&mut self) -> ParseResult<Vec<StringPart>> {
        self.token(&token::Data::DoubleQuote).and_then(|_| {
            keep_left!(
                many!(
                    self,
                    choices!(self, self.string_part_expr(), self.string_part_string())
                ),
                self.token(&token::Data::DoubleQuote)
            )
        })
    }

    /*
    expr_record ::=
      '{' [expr_record_fields] '}'

    expr_record_fields ::=
      ident '=' expr [',' expr_record_fields]
      '..' expr_atom
     */
    fn expr_record(&mut self) -> ParseResult<Expr> {
        fn expr_record_fields(
            parser: &mut Parser,
            fields: &mut Vec<(String, Spanned<Expr>)>,
        ) -> ParseResult<Option<Spanned<Expr>>> {
            choices!(
                parser,
                keep_left!(
                    parser.ident_owned(),
                    keep_left!(
                        parser.spaces(),
                        keep_left!(parser.token(&token::Data::Equals), parser.spaces())
                    )
                )
                .and_then(|name| {
                    parser.expr().and_then(|expr| {
                        fields.push((name, expr));
                        optional!(
                            parser,
                            keep_right!(
                                keep_left!(parser.token(&token::Data::Comma), parser.spaces()),
                                expr_record_fields(parser, fields)
                            )
                        )
                        .map(|m_rest| match m_rest {
                            None => None,
                            Some(rest) => rest,
                        })
                    })
                }),
                keep_right!(
                    keep_left!(
                        keep_left!(
                            parser.token(&token::Data::Dot),
                            parser.token(&token::Data::Dot)
                        ),
                        parser.spaces()
                    ),
                    keep_left!(parser.expr_atom(), parser.spaces()).map(Some)
                )
            )
        }
        keep_right!(
            keep_left!(self.token(&token::Data::LBrace), self.spaces()),
            keep_left!(
                {
                    let mut fields = Vec::new();
                    expr_record_fields(self, &mut fields).map(|rest| Expr::mk_record(fields, rest))
                },
                keep_left!(self.token(&token::Data::RBrace), self.spaces())
            )
        )
    }

    /*
    expr_embed ::=
      '<' ctor '|' expr '>'
     */
    fn expr_embed(&mut self) -> ParseResult<Expr> {
        keep_right!(
            keep_left!(self.token(&token::Data::LAngle), self.spaces()),
            keep_left!(
                keep_left!(self.ctor_owned(), self.spaces()).and_then(|ctor| keep_left!(
                    self.token(&token::Data::Pipe),
                    self.spaces()
                )
                .and_then(|_| self.expr().map(|rest| Expr::mk_embed(ctor, rest)))),
                keep_left!(self.token(&token::Data::RAngle), self.spaces())
            )
        )
    }

    fn expr_array(&mut self) -> ParseResult<Expr> {
        between!(
            keep_left!(self.token(&token::Data::LBracket), self.spaces()),
            keep_left!(self.token(&token::Data::RBracket), self.spaces()),
            sep_by!(
                self,
                self.expr(),
                keep_left!(self.token(&token::Data::Comma), self.spaces())
            )
        )
        .map(Expr::Array)
    }

    fn expr_atom(&mut self) -> ParseResult<Spanned<Expr>> {
        keep_left!(
            spanned!(
                self,
                choices!(
                    self,
                    self.int().map(Expr::Int),
                    self.char().map(Expr::Char),
                    self.keyword(&Keyword::False).map(|_| Expr::False),
                    self.keyword(&Keyword::True).map(|_| Expr::True),
                    self.ident_owned().map(Expr::Var),
                    self.ctor_owned().map(Expr::Variant),
                    self.expr_record(),
                    self.expr_embed(),
                    self.expr_array(),
                    keep_right!(
                        keep_left!(self.token(&token::Data::LParen), self.spaces()),
                        keep_left!(
                            optional!(self, self.expr()).map(|m_ty| match m_ty {
                                None => Expr::Unit,
                                Some(ty) => ty.item,
                            }),
                            self.token(&token::Data::RParen)
                        )
                    ),
                    self.string().map(Expr::String)
                )
            ),
            self.spaces()
        )
    }

    fn expr_project(&mut self) -> ParseResult<Spanned<Expr>> {
        self.expr_atom().and_then(|val| {
            many!(
                self,
                keep_left!(self.token(&token::Data::Dot), self.spaces())
                    .and_then(|_| keep_left!(self.ident_owned(), self.spaces()))
            )
            .map(|fields| {
                let mut expr = val;
                for field in fields {
                    expr = Spanned {
                        pos: expr.pos,
                        item: Expr::mk_project(expr, field),
                    };
                }
                expr
            })
        })
    }

    fn branch(&mut self) -> ParseResult<Branch> {
        keep_left!(spanned!(self, self.pattern()), self.spaces()).and_then(|pattern| {
            map2!(
                |_, body| Branch { pattern, body },
                keep_left!(self.token(&token::Data::Arrow), self.spaces()),
                self.expr()
            )
        })
    }

    fn expr_case(&mut self) -> ParseResult<Spanned<Expr>> {
        spanned!(
            self,
            keep_left!(self.keyword(&Keyword::Case), self.spaces()).and_then(|_| {
                self.expr().and_then(|cond| {
                    keep_left!(
                        self.keyword(&Keyword::Of),
                        many_!(self, self.token(&token::Data::Space))
                    )
                    .and_then(|_| {
                        keep_right!(self.indent(), sep_by!(self, self.branch(), self.newline()))
                            .and_then(|branches| {
                                self.dedent().map(|_| Expr::mk_case(cond, branches))
                            })
                    })
                })
            })
        )
    }

    fn expr_app(&mut self) -> ParseResult<Spanned<Expr>> {
        self.expr_project().and_then(|first| {
            many!(self, self.expr_project()).map(|rest| rest.into_iter().fold(first, Expr::mk_app))
        })
    }

    fn expr_lam(&mut self) -> ParseResult<Spanned<Expr>> {
        spanned!(
            self,
            keep_right!(
                keep_left!(self.token(&token::Data::Backslash), self.spaces()),
                many!(self, self.pattern()).and_then(|args| keep_right!(
                    keep_left!(self.token(&token::Data::Arrow), self.spaces()),
                    self.expr().map(|body| syntax::Expr::mk_lam(args, body))
                ))
            )
        )
    }

    fn expr_ifthenelse(&mut self) -> ParseResult<Spanned<Expr>> {
        spanned!(
            self,
            keep_left!(self.keyword(&Keyword::If), self.spaces()).and_then(|_| self
                .expr()
                .and_then(
                    |cond| keep_left!(self.keyword(&Keyword::Then), self.spaces()).and_then(|_| {
                        self.expr().and_then(|then| {
                            keep_left!(self.keyword(&Keyword::Else), self.spaces()).and_then(|_| {
                                self.expr()
                                    .map(|else_| syntax::Expr::mk_ifthenelse(cond, then, else_))
                            })
                        })
                    })
                ))
        )
    }

    fn expr_let(&mut self) -> ParseResult<Spanned<Expr>> {
        spanned!(
            self,
            keep_left!(self.keyword(&Keyword::Let), self.spaces()).and_then(|_| keep_left!(
                self.ident(),
                self.spaces()
            )
            .and_then(
                move |name| keep_left!(self.token(&token::Data::Equals), self.spaces()).and_then(
                    |_| self.expr().and_then(move |value| keep_left!(
                        self.keyword(&Keyword::In),
                        self.spaces()
                    )
                    .and_then(|_| self.expr().map(move |rest| syntax::Expr::Let {
                        name,
                        value: Rc::new(value),
                        rest: Rc::new(rest)
                    })))
                )
            ))
        )
    }

    fn expr(&mut self) -> ParseResult<Spanned<Expr>> {
        keep_left!(
            choices!(
                self,
                self.expr_case(),
                self.expr_lam(),
                self.expr_ifthenelse(),
                self.expr_let(),
                self.expr_comp(),
                self.expr_app()
            ),
            self.spaces()
        )
    }

    fn definition(&mut self) -> ParseResult<Declaration> {
        keep_left!(self.ident_owned(), self.spaces()).and_then(|name| {
            keep_left!(self.token(&token::Data::Colon), self.spaces()).and_then(|_| {
                keep_left!(self.type_(), self.newline()).and_then(|ty| {
                    keep_right!(
                        keep_left!(
                            self.token(&token::Data::Ident(Rc::from(name.as_ref()))),
                            self.spaces()
                        ),
                        many!(self, self.pattern())
                    )
                    .and_then(|args| {
                        keep_left!(self.token(&token::Data::Equals), self.spaces()).and_then(|_| {
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
            keep_left!(self.keyword(&Keyword::Type), self.spaces()),
            keep_left!(self.ctor_owned(), self.spaces()).and_then(|name| many!(
                self,
                keep_left!(self.ident_owned(), self.spaces())
            )
            .and_then(|args| keep_right!(
                keep_left!(self.token(&token::Data::Equals), self.spaces()),
                self.type_()
                    .map(|body| Declaration::TypeAlias { name, args, body })
            )))
        )
    }

    fn import(&mut self) -> ParseResult<Declaration> {
        keep_left!(self.keyword(&Keyword::Import), self.spaces()).and_then(|_| {
            keep_left!(spanned!(self, self.ident_owned()), self.spaces()).and_then(|module| {
                optional!(
                    self,
                    keep_left!(self.keyword(&Keyword::As), self.spaces()).and_then(|_| keep_left!(
                        spanned!(self, self.ident_owned()),
                        self.spaces()
                    ))
                )
                .map(|name| Declaration::Import { module, name })
            })
        })
    }

    fn from_import(&mut self) -> ParseResult<Declaration> {
        keep_right!(
            keep_left!(self.keyword(&Keyword::From), self.spaces()),
            keep_left!(spanned!(self, self.ident_owned()), self.spaces()).and_then(
                |module| keep_right!(
                    keep_left!(self.keyword(&Keyword::Import), self.spaces()),
                    choices!(
                        self,
                        keep_left!(
                            map0!(Names::All, self.token(&token::Data::Asterisk)),
                            self.spaces()
                        ),
                        sep_by!(
                            self,
                            keep_left!(self.ident_owned(), self.spaces()),
                            keep_left!(self.token(&token::Data::Comma), self.spaces())
                        )
                        .map(Names::Names)
                    )
                    .map(|names| Declaration::FromImport { module, names })
                )
            )
        )
    }

    fn assumptions(&mut self) -> ParseResult<Vec<Spanned<Type<Rc<str>>>>> {
        optional!(
            self,
            between!(
                keep_left!(self.token(&token::Data::LParen), self.spaces()),
                keep_left!(self.token(&token::Data::RParen), self.spaces()),
                many!(self, spanned!(self, self.type_()))
            )
        )
        .and_then(|m_tys| match m_tys {
            None => ParseResult::pure(Vec::new()),
            Some(tys) => keep_left!(self.token(&token::Data::FatArrow), self.spaces()).map(|_| tys),
        })
    }

    fn class_member(&mut self) -> ParseResult<(String, Type<Rc<str>>)> {
        keep_left!(self.ident_owned(), self.spaces()).and_then(|name| {
            keep_left!(self.token(&token::Data::Colon), self.spaces()).and_then(|_| {
                keep_left!(self.type_(), many_!(self, self.token(&token::Data::Space)))
                    .map(|type_| (name, type_))
            })
        })
    }

    fn class(&mut self) -> ParseResult<Declaration> {
        keep_left!(self.keyword(&Keyword::Class), self.spaces()).and_then(|_| {
            self.assumptions().and_then(|supers| {
                keep_left!(self.ctor(), self.spaces()).and_then(|name| {
                    many!(
                        self,
                        keep_left!(spanned!(self, self.ident()), self.spaces())
                    )
                    .and_then(|args| {
                        keep_left!(
                            self.keyword(&Keyword::Where),
                            many_!(self, self.token(&token::Data::Space))
                        )
                        .and_then(|_| {
                            keep_right!(
                                self.indent(),
                                sep_by!(self, self.class_member(), self.newline()).and_then(
                                    |members| keep_right!(
                                        self.dedent(),
                                        ParseResult::pure(Declaration::Class {
                                            supers,
                                            name,
                                            args,
                                            members,
                                        })
                                    )
                                )
                            )
                        })
                    })
                })
            })
        })
    }

    fn instance_member(&mut self) -> ParseResult<(Spanned<String>, Vec<Pattern>, Spanned<Expr>)> {
        keep_left!(spanned!(self, self.ident_owned()), self.spaces()).and_then(|name| {
            many!(self, keep_left!(self.pattern(), self.spaces())).and_then(|args| {
                keep_left!(self.token(&token::Data::Equals), self.spaces())
                    .and_then(|_| self.expr().map(|body| (name, args, body)))
            })
        })
    }

    fn instance(&mut self) -> ParseResult<Declaration> {
        keep_left!(self.keyword(&Keyword::Instance), self.spaces()).and_then(|_| {
            self.assumptions().and_then(|assumes| {
                keep_left!(spanned!(self, self.ctor()), self.spaces()).and_then(|name| {
                    many!(self, keep_left!(self.type_(), self.spaces())).and_then(|args| {
                        keep_left!(
                            self.keyword(&Keyword::Where),
                            many_!(self, self.token(&token::Data::Space))
                        )
                        .and_then(|_| {
                            keep_right!(
                                self.indent(),
                                sep_by!(self, self.instance_member(), self.newline()).and_then(
                                    |members| keep_right!(
                                        self.dedent(),
                                        ParseResult::pure(Declaration::Instance {
                                            assumes,
                                            name,
                                            args,
                                            members,
                                        })
                                    )
                                )
                            )
                        })
                    })
                })
            })
        })
    }

    fn declaration(&mut self) -> ParseResult<Declaration> {
        choices!(
            self,
            self.definition(),
            self.type_alias(),
            self.import(),
            self.from_import(),
            self.class(),
            self.instance()
        )
    }

    pub fn module(&mut self) -> ParseResult<Module> {
        keep_right!(
            many!(self, self.newline()),
            sep_by!(
                self,
                spanned!(self, self.declaration()),
                some_!(self, self.newline())
            )
            .map(|decls| Module { decls })
        )
    }
}
