mod test;

use fixedbitset::FixedBitSet;
use fnv::FnvHashSet;
use ipso_diagnostic::{Diagnostic, Location, Message, Source};
use ipso_lex::{
    token::{self, Relation, Token},
    Lexer,
};
use ipso_syntax::{
    self as syntax, r#type::Type, Branch, CompLine, Declaration, Expr, Keyword, Module, Names,
    Pattern, Spanned, StringPart,
};
use std::{
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
pub enum ParseResultError {
    Unexpected,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseResult<A> {
    pub consumed: bool,
    pub result: Result<A, ParseResultError>,
}

impl<A> ParseResult<A> {
    pub fn and_then<B, F>(self, f: F) -> ParseResult<B>
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
            result: b.result.map(|_| $a),
        }
    }};
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

macro_rules! many_ {
    ($self:expr, $x:expr) => {{
        let mut error: bool = false;
        let mut consumed = false;
        loop {
            let next = $x;
            match next.result {
                Err(_) => {
                    if next.consumed {
                        error = true;
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
                false => Ok(()),
                true => Err(ParseResultError::Unexpected),
            },
        }
    }};
}

macro_rules! many_with {
    ($vec:expr, $self:expr, $x:expr) => {{
        let mut error: bool = false;
        let mut acc: Vec<_> = $vec;
        let mut consumed = false;
        loop {
            let next = $x;
            match next.result {
                Err(_) => {
                    if next.consumed {
                        error = true;
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
                false => Ok(acc),
                true => Err(ParseResultError::Unexpected),
            },
        }
    }};
}

macro_rules! many {
    ($self:expr, $x:expr) => {
        many_with!(Vec::new(), $self, $x)
    };
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
            Ok(val) => ParseResult{consumed: first.consumed, result: Ok(val)},
        }
    }}
}

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

macro_rules! indent {
    ($self:expr, $relation:expr, $body:expr) => {{
        let current_indentation: Option<usize> = $self.indentation.last().copied();
        $self.expecting.insert(match current_indentation {
            None => token::Name::Indent(Relation::Gte, 0),
            Some(current_indentation) => token::Name::Indent($relation, current_indentation),
        });
        match &$self.current {
            None => $self.unexpected(false),
            Some(token) => {
                let current_indentation_matches = match current_indentation {
                    None => match $relation {
                        Relation::Gt => true,
                        Relation::Gte => true,
                        Relation::Eq => false,
                    },
                    Some(current_indentation) => match $relation {
                        Relation::Gt => token.column > current_indentation,
                        Relation::Gte => token.column >= current_indentation,
                        Relation::Eq => token.column == current_indentation,
                    },
                };
                if current_indentation_matches {
                    $self.expecting.clear_indents();
                    $body
                } else {
                    $self.unexpected(false)
                }
            }
        }
    }};
}

macro_rules! indent_scope {
    ($self:expr, $body:expr) => {{
        $self.indentation.push($self.column);
        let b = $body;
        $self.indentation.pop().unwrap();
        b
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

    pub fn into_parse_error<A>(self, result: Result<A, ParseResultError>) -> Result<A, ParseError> {
        match result {
            Ok(a) => Ok(a),
            Err(err) => Err(match err {
                ParseResultError::Unexpected => ParseError::Unexpected {
                    source: self.source,
                    pos: self.pos,
                    expecting: self.expecting.into_btreeset(),
                },
            }),
        }
    }

    fn unexpected<A>(&self, consumed: bool) -> ParseResult<A> {
        ParseResult {
            consumed,
            result: Err(ParseResultError::Unexpected),
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
                    result: Ok(()),
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
                .and_then(|_| map0!((), optional!(self, self.comment()))),
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
                            .and_then(|_| map0!(s, optional!(self, self.comment())))
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
                            .and_then(|_| map0!(s, optional!(self, self.comment())))
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
    ///     expecting: vec![token::Name::Char].into_iter().collect(),
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
    ///     expecting: vec![token::Name::Char].into_iter().collect(),
    /// }));
    ///
    /// assert_eq!(parse_str!(char, "\'\\~\'"), Err(ParseError::Unexpected {
    ///     source: Source::Interactive{label: String::from("(string)")},
    ///     pos: 2,
    ///     expecting: vec![token::Name::Char].into_iter().collect(),
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

    /*
    type_record_fields ::=
      [ident [':' type [',' type_record_fields]]
    */
    fn type_record_fields(
        &mut self,
        fields: &mut Vec<(Rc<str>, Type<Rc<str>>)>,
    ) -> ParseResult<Option<Type<Rc<str>>>> {
        /*
        `indent_gte` allows the record type's contents to be in any column
        greater than or equal to the record type's opening brace.
        */
        optional!(self, indent!(self, Relation::Gte, self.ident())).and_then(
            |m_ident| match m_ident {
                None => ParseResult::pure(None),
                Some(ident) => optional!(
                    self,
                    keep_right!(
                        indent!(self, Relation::Gte, self.token(&token::Data::Colon)),
                        indent!(self, Relation::Gte, self.type_())
                    )
                )
                .and_then(|m_ty| match m_ty {
                    None => ParseResult::pure(Some(Type::Var(ident))),
                    Some(ty) => {
                        fields.push((ident, ty));
                        optional!(
                            parser,
                            keep_right!(
                                indent!(self, Relation::Gte, self.token(&token::Data::Comma)),
                                indent!(self, Relation::Gte, self.type_record_fields(fields))
                            )
                        )
                        .map(|m_rest| match m_rest {
                            None => None,
                            Some(rest) => rest,
                        })
                    }
                }),
            },
        )
    }

    /*
    type_record ::=
      '{' type_record_fields '}'
     */
    fn type_record(&mut self) -> ParseResult<Type<Rc<str>>> {
        indent_scope!(self, {
            between!(
                indent!(self, Relation::Eq, self.token(&token::Data::LBrace)),
                indent!(self, Relation::Gte, self.token(&token::Data::RBrace)),
                {
                    let mut fields = Vec::new();
                    self.type_record_fields(&mut fields)
                        .map(|rest| Type::mk_record(fields, rest))
                }
            )
        })
    }

    /*
    type_variant_ctors ::=
      ident
      ctor ':' type ['|' type_variant_ctors]
    */
    fn type_variant_ctors(
        &mut self,
        ctors: &mut Vec<(Rc<str>, Type<Rc<str>>)>,
    ) -> ParseResult<Option<Type<Rc<str>>>> {
        choices!(
            self,
            indent!(self, Relation::Gte, self.ident()).map(|x| Some(Type::Var(x))),
            indent!(self, Relation::Gte, self.ctor())
                .and_then(|ctor| map0!(
                    ctor,
                    indent!(self, Relation::Gte, self.token(&token::Data::Colon))
                ))
                .and_then(|ctor| self.type_().map(|ty| (ctor, ty)))
                .and_then(|(ctor, ty)| {
                    ctors.push((ctor, ty));
                    optional!(
                        parser,
                        indent!(self, Relation::Gte, self.token(&token::Data::Pipe))
                            .and_then(|_| self.type_variant_ctors(ctors))
                    )
                    .map(|m_rest| match m_rest {
                        None => None,
                        Some(rest) => rest,
                    })
                })
        )
    }

    /*
    type_variant ::=
      '<' [type_variant_ctors] '>'
     */
    fn type_variant(&mut self) -> ParseResult<Type<Rc<str>>> {
        indent_scope!(self, {
            between!(
                indent!(self, Relation::Eq, self.token(&token::Data::LAngle)),
                indent!(self, Relation::Gte, self.token(&token::Data::RAngle)),
                {
                    let mut ctors = Vec::new();
                    optional!(self, self.type_variant_ctors(&mut ctors)).map(
                        |m_rest| match m_rest {
                            None => Type::mk_variant(Vec::new(), None),
                            Some(rest) => Type::mk_variant(ctors, rest),
                        },
                    )
                }
            )
        })
    }

    /*
    type_atom ::=
      'Bool'
      'Int'
      'Char'
      'String'
      'Array'
      'IO'
      type_record
      type_variant
      ident
      ctor
      '(' type ')'
    */
    fn type_atom(&mut self) -> ParseResult<Type<Rc<str>>> {
        indent!(
            self,
            Relation::Gt,
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
                between!(
                    indent!(self, Relation::Gt, self.token(&token::Data::LParen)),
                    indent!(self, Relation::Gt, self.token(&token::Data::RParen)),
                    optional!(self, indent!(self, Relation::Gt, self.type_()))
                        .map(|m_ty| m_ty.unwrap_or(Type::Unit))
                )
            )
        )
    }

    /*
    type_app ::=
      type_atom+
     */
    fn type_app(&mut self) -> ParseResult<Type<Rc<str>>> {
        self.type_atom().and_then(|first| {
            many!(self, self.type_atom()).map(|rest| rest.into_iter().fold(first, Type::mk_app))
        })
    }

    /*
    type_arrow ::=
      type_app ['->' type_arrow]
     */
    fn type_arrow(&mut self) -> ParseResult<Type<Rc<str>>> {
        self.type_app().and_then(|a| {
            optional!(
                self,
                map2!(
                    |_, ty| ty,
                    indent!(self, Relation::Gt, self.token(&token::Data::Arrow)),
                    self.type_arrow()
                )
            )
            .map(|m_b| match m_b {
                None => a,
                Some(b) => Type::mk_arrow(a, b),
            })
        })
    }

    /*
    type_fatarrow ::=
      type_arrow ['=>' type_fatarrow]
     */
    fn type_fatarrow(&mut self) -> ParseResult<Type<Rc<str>>> {
        self.type_arrow().and_then(|a| {
            optional!(
                self,
                map2!(
                    |_, ty| ty,
                    indent!(self, Relation::Gt, self.token(&token::Data::FatArrow)),
                    self.type_fatarrow()
                )
            )
            .map(|m_b| match m_b {
                None => a,
                Some(b) => Type::mk_fatarrow(a, b),
            })
        })
    }

    /*
    type ::=
      type_fatarrow
    */
    fn type_(&mut self) -> ParseResult<Type<Rc<str>>> {
        self.type_fatarrow()
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
            // 'bind' ident '<-' expr
            {
                keep_right!(
                    self.keyword(&Keyword::Bind),
                    indent!(self, Relation::Gt, self.ident())
                )
                .and_then(|name| {
                    keep_right!(
                        indent!(self, Relation::Gt, self.token(&token::Data::LeftArrow)),
                        indent!(self, Relation::Gt, self.expr())
                    )
                    .map(|value| CompLine::Bind(name, value))
                })
            },
            // 'return' expr
            {
                keep_right!(
                    self.keyword(&Keyword::Return),
                    indent!(self, Relation::Gt, self.expr())
                )
                .map(CompLine::Return)
            },
            self.expr().map(CompLine::Expr)
        )
    }

    /*
    expr_comp ::=
      'comp' comp_line*
     */
    fn expr_comp(&mut self) -> ParseResult<Spanned<Expr>> {
        spanned!(
            self,
            indent_scope!(
                self,
                keep_right!(
                    indent!(self, Relation::Eq, self.keyword(&Keyword::Comp)),
                    indent!(
                        self,
                        Relation::Gt,
                        indent_scope!(
                            self,
                            many!(self, indent!(self, Relation::Eq, self.comp_line()))
                                .map(Expr::Comp)
                        )
                    )
                )
            )
        )
    }

    /*
    pattern_record_fields ::=
      ident [',' pattern_record_fields]
      '..' ident
    */
    fn pattern_record_fields(
        &mut self,
        names: &mut Vec<Spanned<String>>,
    ) -> ParseResult<Option<Spanned<String>>> {
        choices!(
            self,
            // ident [',' pattern_record_fields]
            spanned!(self, indent!(self, Relation::Gte, self.ident_owned())).and_then(|name| {
                names.push(name);
                optional!(
                    self,
                    keep_right!(
                        indent!(self, Relation::Gte, self.token(&token::Data::Comma)),
                        self.pattern_record_fields(names)
                    )
                )
                .map(|m_rest| match m_rest {
                    None => None,
                    Some(rest) => rest,
                })
            }),
            // '..' ident
            keep_right!(
                indent!(self, Relation::Gte, self.token(&token::Data::DotDot)),
                spanned!(self, indent!(self, Relation::Gte, self.ident_owned())).map(Some)
            )
        )
    }

    /*
    pattern_record ::=
      '{' [pattern_record_fields] '}'
    */
    fn pattern_record(&mut self) -> ParseResult<Pattern> {
        indent_scope!(self, {
            between!(
                indent!(self, Relation::Eq, self.token(&token::Data::LBrace)),
                indent!(self, Relation::Gte, self.token(&token::Data::RBrace)),
                {
                    let mut names = Vec::new();
                    self.pattern_record_fields(&mut names)
                        .map(|rest| Pattern::Record { names, rest })
                }
            )
        })
    }

    /*
    pattern_variant ::=
      ctor ident
    */
    fn pattern_variant(&mut self) -> ParseResult<Pattern> {
        self.ctor_owned().and_then(|name| {
            spanned!(self, indent!(self, Relation::Gt, self.ident_owned()))
                .map(|arg| Pattern::Variant { name, arg })
        })
    }

    /*
    pattern ::=
      ident
      pattern_record
      pattern_variant
      '_'
    */
    fn pattern(&mut self) -> ParseResult<Pattern> {
        choices!(
            self,
            spanned!(self, self.ident_owned()).map(Pattern::Name),
            self.pattern_record(),
            self.pattern_variant(),
            map0!(Pattern::Wildcard, self.token(&token::Data::Underscore))
        )
    }

    /*
    string_part_expr ::=
      '${' expr '}'
      '$' ident
    */
    fn string_part_expr(&mut self) -> ParseResult<StringPart> {
        choices!(
            self,
            between!(
                indent!(self, Relation::Gte, self.token(&token::Data::DollarLBrace)),
                indent!(self, Relation::Gte, self.token(&token::Data::RBrace)),
                self.expr().map(StringPart::Expr)
            ),
            keep_right!(
                indent!(self, Relation::Gte, self.token(&token::Data::Dollar)),
                spanned!(self, self.ident_owned().map(Expr::Var)).map(StringPart::Expr)
            )
        )
    }

    fn string_part_string(&mut self) -> ParseResult<StringPart> {
        self.expecting.insert(token::Name::String);

        indent!(self, Relation::Gte, {
            let str = match &self.current {
                Some(current) => match &current.data {
                    token::Data::String { value, .. } => value.clone(),
                    _ => return self.unexpected(false),
                },
                None => return self.unexpected(false),
            };
            self.consume();
            ParseResult::pure(StringPart::String(str))
        })
    }

    /*
    string_part ::=
      string_part_expr
      string_part_string

    string ::=
      '"' string_part* '"'
    */
    fn string(&mut self) -> ParseResult<Vec<StringPart>> {
        indent_scope!(self, {
            between!(
                indent!(self, Relation::Eq, self.token(&token::Data::DoubleQuote)),
                indent!(self, Relation::Gte, self.token(&token::Data::DoubleQuote)),
                many!(
                    self,
                    choices!(self, self.string_part_expr(), self.string_part_string())
                )
            )
        })
    }

    /*
    expr_record_fields ::=
      ident '=' expr [',' expr_record_fields]
      '..' expr_atom
    */
    fn expr_record_fields(
        &mut self,
        fields: &mut Vec<(String, Spanned<Expr>)>,
    ) -> ParseResult<Option<Spanned<Expr>>> {
        choices!(
            self,
            keep_left!(
                indent!(self, Relation::Gte, self.ident_owned()),
                indent!(self, Relation::Gte, self.token(&token::Data::Equals))
            )
            .and_then(|name| {
                self.expr().and_then(|expr| {
                    fields.push((name, expr));
                    optional!(
                        self,
                        keep_right!(
                            indent!(self, Relation::Gte, self.token(&token::Data::Comma)),
                            self.expr_record_fields(fields)
                        )
                    )
                    .map(|m_rest| match m_rest {
                        None => None,
                        Some(rest) => rest,
                    })
                })
            }),
            keep_right!(
                indent!(self, Relation::Gte, self.token(&token::Data::DotDot)),
                indent!(self, Relation::Gte, self.expr_atom()).map(Some)
            )
        )
    }

    /*
    expr_record ::=
      '{' [expr_record_fields] '}'
    */
    fn expr_record(&mut self) -> ParseResult<Expr> {
        indent_scope!(self, {
            between!(
                indent!(self, Relation::Eq, self.token(&token::Data::LBrace)),
                indent!(self, Relation::Gte, self.token(&token::Data::RBrace)),
                {
                    let mut fields = Vec::new();
                    self.expr_record_fields(&mut fields)
                        .map(|rest| Expr::mk_record(fields, rest))
                }
            )
        })
    }

    /*
    expr_embed ::=
      '<' ctor '|' expr '>'
     */
    fn expr_embed(&mut self) -> ParseResult<Expr> {
        indent_scope!(self, {
            between!(
                indent!(self, Relation::Eq, self.token(&token::Data::LAngle)),
                indent!(self, Relation::Gte, self.token(&token::Data::RAngle)),
                indent!(self, Relation::Gte, self.ctor_owned()).and_then(|ctor| indent!(
                    self,
                    Relation::Gte,
                    self.token(&token::Data::Pipe)
                )
                .and_then(|_| self.expr().map(|rest| Expr::mk_embed(ctor, rest))))
            )
        })
    }

    /*
    expr_array ::=
      '[' expr [',' expr] ']'
    */
    fn expr_array(&mut self) -> ParseResult<Expr> {
        indent_scope!(self, {
            between!(
                indent!(self, Relation::Eq, self.token(&token::Data::LBracket)),
                indent!(self, Relation::Gte, self.token(&token::Data::RBracket)),
                sep_by!(
                    self,
                    self.expr(),
                    indent!(self, Relation::Gte, self.token(&token::Data::Comma))
                )
            )
            .map(Expr::Array)
        })
    }

    /*
    expr_atom ::=
      int
      char
      'false'
      'true'
      ident
      ctor
      expr_record
      expr_embed
      expr_array
      '(' expr ')'
      string
    */
    fn expr_atom(&mut self) -> ParseResult<Spanned<Expr>> {
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
                between!(
                    self.token(&token::Data::LParen),
                    indent!(self, Relation::Gt, self.token(&token::Data::RParen)),
                    optional!(self, indent!(self, Relation::Gt, self.expr())).map(|m_ty| {
                        match m_ty {
                            None => Expr::Unit,
                            Some(ty) => ty.item,
                        }
                    })
                ),
                self.string().map(Expr::String)
            )
        )
    }

    /*
    expr_project ::=
      expr_atom ('.' ident)*
    */
    fn expr_project(&mut self) -> ParseResult<Spanned<Expr>> {
        self.expr_atom().and_then(|val| {
            many!(
                self,
                keep_right!(
                    indent!(self, Relation::Gt, self.token(&token::Data::Dot)),
                    indent!(self, Relation::Gt, self.ident_owned())
                )
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

    /*
    case_branch ::=
      pattern '->' expr
    */
    fn case_branch(&mut self) -> ParseResult<Branch> {
        spanned!(self, indent!(self, Relation::Eq, self.pattern())).and_then(|pattern| {
            map2!(
                |_, body| Branch { pattern, body },
                indent!(self, Relation::Gt, self.token(&token::Data::Arrow)),
                self.expr()
            )
        })
    }

    /*
    expr_case ::=
      'case' expr 'of' case_branch*
    */
    fn expr_case(&mut self) -> ParseResult<Spanned<Expr>> {
        spanned!(
            self,
            keep_right!(self.keyword(&Keyword::Case), self.expr()).and_then(|cond| keep_right!(
                indent!(self, Relation::Gt, self.keyword(&Keyword::Of)),
                indent!(
                    self,
                    Relation::Gt,
                    indent_scope!(self, { many!(self, self.case_branch()) })
                )
            )
            .map(|branches| Expr::mk_case(cond, branches)))
        )
    }

    /*
    expr_app ::=
      expr_project+
    */
    fn expr_app(&mut self) -> ParseResult<Spanned<Expr>> {
        self.expr_project().and_then(|first| {
            many!(self, indent!(self, Relation::Gt, self.expr_project()))
                .map(|rest| rest.into_iter().fold(first, Expr::mk_app))
        })
    }

    /*
    expr_lam ::=
      '\' pattern '->' expr
    */
    fn expr_lam(&mut self) -> ParseResult<Spanned<Expr>> {
        spanned!(
            self,
            keep_right!(
                self.token(&token::Data::Backslash),
                many!(self, indent!(self, Relation::Gt, self.pattern())).and_then(
                    |args| keep_right!(
                        indent!(self, Relation::Gt, self.token(&token::Data::Arrow)),
                        self.expr().map(|body| syntax::Expr::mk_lam(args, body))
                    )
                )
            )
        )
    }

    /*
    expr_ifthenelse ::=
      'if' expr 'then' expr 'else' expr
    */
    fn expr_ifthenelse(&mut self) -> ParseResult<Spanned<Expr>> {
        spanned!(
            self,
            keep_right!(
                self.keyword(&Keyword::If),
                self.expr().and_then(|cond| keep_right!(
                    indent!(self, Relation::Gt, self.keyword(&Keyword::Then)),
                    self.expr().and_then(|then| keep_right!(
                        indent!(self, Relation::Gt, self.keyword(&Keyword::Else)),
                        self.expr()
                            .map(|else_| syntax::Expr::mk_ifthenelse(cond, then, else_))
                    ))
                ))
            )
        )
    }

    /*
    expr_let ::=
      'let' ident '=' expr 'in' expr
    */
    fn expr_let(&mut self) -> ParseResult<Spanned<Expr>> {
        spanned!(
            self,
            keep_right!(
                self.keyword(&Keyword::Let),
                indent!(self, Relation::Gt, self.ident()).and_then(|name| {
                    keep_right!(
                        indent!(self, Relation::Gt, self.token(&token::Data::Equals)),
                        self.expr().and_then(|value| {
                            keep_right!(
                                indent!(self, Relation::Gte, self.keyword(&Keyword::In)),
                                self.expr().map(|rest| {
                                    syntax::Expr::Let {
                                        name,
                                        value: Rc::new(value),
                                        rest: Rc::new(rest),
                                    }
                                })
                            )
                        })
                    )
                })
            )
        )
    }

    /*
    expr ::=
      expr_case
      expr_lam
      expr_ifthenelse
      expr_let
      expr_comp
      expr_app
    */
    fn expr(&mut self) -> ParseResult<Spanned<Expr>> {
        choices!(
            self,
            self.expr_case(),
            self.expr_lam(),
            self.expr_ifthenelse(),
            self.expr_let(),
            self.expr_comp(),
            self.expr_app()
        )
    }

    /*
    definition ::=
      ident ':' type ident pattern* '=' expr
    */
    fn definition(&mut self) -> ParseResult<Declaration> {
        indent_scope!(self, {
            keep_left!(
                indent!(self, Relation::Eq, self.ident_owned()),
                indent!(self, Relation::Gt, self.token(&token::Data::Colon))
            )
            .and_then(|name| {
                self.type_().and_then(|ty| {
                    keep_right!(
                        indent!(
                            self,
                            Relation::Eq,
                            self.token(&token::Data::Ident(Rc::from(name.as_ref())))
                        ),
                        many!(self, indent!(self, Relation::Gt, self.pattern()))
                    )
                    .and_then(|args| {
                        keep_right!(
                            indent!(self, Relation::Gt, self.token(&token::Data::Equals)),
                            indent!(self, Relation::Gt, self.expr()).map(|body| {
                                Declaration::Definition {
                                    name,
                                    ty,
                                    args,
                                    body,
                                }
                            })
                        )
                    })
                })
            })
        })
    }

    /*
    type_alias ::=
      'type' ctor '=' type
    */
    fn type_alias(&mut self) -> ParseResult<Declaration> {
        indent_scope!(self, {
            keep_right!(
                indent!(self, Relation::Eq, self.keyword(&Keyword::Type)),
                indent!(self, Relation::Gt, self.ctor_owned()).and_then(|name| many!(
                    self,
                    self.ident_owned()
                )
                .and_then(|args| keep_right!(
                    indent!(self, Relation::Gt, self.token(&token::Data::Equals)),
                    indent!(self, Relation::Gt, self.type_()).map(|body| Declaration::TypeAlias {
                        name,
                        args,
                        body
                    })
                )))
            )
        })
    }

    /*
    import ::=
      'import' ident ['as' ident]
    */
    fn import(&mut self) -> ParseResult<Declaration> {
        indent_scope!(self, {
            keep_right!(
                indent!(self, Relation::Eq, self.keyword(&Keyword::Import)),
                spanned!(self, indent!(self, Relation::Gt, self.ident_owned())).and_then(
                    |module| {
                        optional!(
                            self,
                            keep_right!(
                                indent!(self, Relation::Gt, self.keyword(&Keyword::As)),
                                spanned!(self, indent!(self, Relation::Gt, self.ident_owned()))
                            )
                        )
                        .map(|name| Declaration::Import { module, name })
                    }
                )
            )
        })
    }

    /*
    from_import_choices ::=
      '*'
      ident [',' ident]

    from_import ::=
      'from' ident 'import'
    */
    fn from_import(&mut self) -> ParseResult<Declaration> {
        indent_scope!(self, {
            keep_right!(
                self.keyword(&Keyword::From),
                spanned!(self, indent!(self, Relation::Gt, self.ident_owned())).and_then(
                    |module| keep_right!(
                        indent!(self, Relation::Gt, self.keyword(&Keyword::Import)),
                        choices!(
                            self,
                            map0!(
                                Names::All,
                                indent!(self, Relation::Gt, self.token(&token::Data::Asterisk))
                            ),
                            sep_by!(
                                self,
                                indent!(self, Relation::Gt, self.ident_owned()),
                                indent!(self, Relation::Gt, self.token(&token::Data::Comma))
                            )
                            .map(Names::Names)
                        )
                        .map(|names| Declaration::FromImport { module, names })
                    )
                )
            )
        })
    }

    /*
    assumptions ::=
      epsilon
      '(' type* ')' '=>'
    */
    fn assumptions(&mut self) -> ParseResult<Vec<Spanned<Type<Rc<str>>>>> {
        indent_scope!(self, {
            optional!(
                self,
                between!(
                    indent!(self, Relation::Eq, self.token(&token::Data::LParen)),
                    indent!(self, Relation::Gte, self.token(&token::Data::RParen)),
                    many!(
                        self,
                        spanned!(self, indent!(self, Relation::Gte, self.type_()))
                    )
                )
            )
            .and_then(|m_tys| match m_tys {
                None => ParseResult::pure(Vec::new()),
                Some(tys) => {
                    indent!(self, Relation::Gte, self.token(&token::Data::FatArrow)).map(|_| tys)
                }
            })
        })
    }

    /*
    class_member ::=
      ident ':' type
    */
    fn class_member(&mut self) -> ParseResult<(String, Type<Rc<str>>)> {
        self.ident_owned().and_then(|name| {
            keep_right!(
                indent!(self, Relation::Gt, self.token(&token::Data::Colon)),
                indent!(self, Relation::Gt, self.type_()).map(|type_| (name, type_))
            )
        })
    }

    /*
    class ::=
      'class' assumptions ctor ident* 'where' class_member*
    */
    fn class(&mut self) -> ParseResult<Declaration> {
        indent_scope!(self, {
            keep_right!(
                indent!(self, Relation::Eq, self.keyword(&Keyword::Class)),
                indent!(self, Relation::Gt, self.assumptions()).and_then(|supers| {
                    indent!(self, Relation::Gt, self.ctor()).and_then(|name| {
                        many!(
                            self,
                            spanned!(self, indent!(self, Relation::Gt, self.ident()))
                        )
                        .and_then(|args| {
                            keep_right!(
                                indent!(self, Relation::Gt, self.keyword(&Keyword::Where)),
                                indent!(
                                    self,
                                    Relation::Gt,
                                    indent_scope!(
                                        self,
                                        many!(
                                            self,
                                            indent!(self, Relation::Eq, self.class_member())
                                        )
                                        .map(|members| {
                                            Declaration::Class {
                                                supers,
                                                name,
                                                args,
                                                members,
                                            }
                                        })
                                    )
                                )
                            )
                        })
                    })
                })
            )
        })
    }

    /*
    instance_member ::=
      ident pattern* '=' expr
    */
    fn instance_member(&mut self) -> ParseResult<(Spanned<String>, Vec<Pattern>, Spanned<Expr>)> {
        spanned!(self, self.ident_owned()).and_then(|name| {
            many!(self, indent!(self, Relation::Gt, self.pattern())).and_then(|args| {
                keep_right!(
                    indent!(self, Relation::Gt, self.token(&token::Data::Equals)),
                    self.expr().map(|body| (name, args, body))
                )
            })
        })
    }

    /*
    instance ::=
      'instance' assumptions ctor type* 'where' instance_member*
    */
    fn instance(&mut self) -> ParseResult<Declaration> {
        indent_scope!(self, {
            keep_right!(
                indent!(self, Relation::Eq, self.keyword(&Keyword::Instance)),
                indent!(self, Relation::Gt, self.assumptions()).and_then(|assumes| {
                    spanned!(self, indent!(self, Relation::Gt, self.ctor())).and_then(|name| {
                        many!(self, indent!(self, Relation::Gt, self.type_())).and_then(|args| {
                            keep_right!(
                                indent!(self, Relation::Gt, self.keyword(&Keyword::Where)),
                                indent!(
                                    self,
                                    Relation::Gt,
                                    indent_scope!(
                                        self,
                                        many!(
                                            self,
                                            indent!(self, Relation::Eq, self.instance_member())
                                        )
                                        .map(|members| {
                                            Declaration::Instance {
                                                assumes,
                                                name,
                                                args,
                                                members,
                                            }
                                        })
                                    )
                                )
                            )
                        })
                    })
                })
            )
        })
    }

    /*
    declaration ::=
      definition
      type_alias
      import
      from_import
      class
      instance
    */
    fn declaration(&mut self) -> ParseResult<Declaration> {
        keep_right!(
            many_!(self, self.comment()),
            choices!(
                self,
                self.definition(),
                self.type_alias(),
                self.import(),
                self.from_import(),
                self.class(),
                self.instance()
            )
        )
    }

    /*
    module ::=
      declaration*
    */
    pub fn module(&mut self) -> ParseResult<Module> {
        indent_scope!(
            self,
            many!(
                self,
                spanned!(self, indent!(self, Relation::Eq, self.declaration()))
            )
        )
        .map(|decls| Module { decls })
    }
}
