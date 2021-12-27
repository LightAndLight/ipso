mod test;

use ipso_syntax::Keyword;
use quickcheck::Arbitrary;
use std::rc::Rc;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub enum Relation {
    Gt,
    Gte,
    Eq,
}

impl Arbitrary for Relation {
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        let vals = &[Relation::Gt, Relation::Gte, Relation::Eq];
        g.choose(vals).unwrap().clone()
    }
}

pub const INDENT_TAG: usize = 52;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub enum Name {
    Unexpected,
    Comment,
    Ctor,
    Ident,
    Keyword(Keyword),
    Int,
    DoubleQuote,
    Dollar,
    DollarLBrace,
    String,
    SingleQuote,
    Char,
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LAngle,
    RAngle,
    Backslash,
    LeftArrow,
    Arrow,
    FatArrow,
    Dot,
    DotDot,
    Asterisk,
    Equals,
    Colon,
    Comma,
    Pipe,
    Underscore,
    Hyphen,
    Plus,
    Slash,
    Indent(Relation, usize),
    Dedent,
    Eof,
    Backtick,
    Cmd,
    Bang,
    Ampersand,
    LParenPipe,
    PipeRParen,
}

impl Arbitrary for Name {
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        let vals = &[
            Name::Unexpected,
            Name::Comment,
            Name::Ctor,
            Name::Ident,
            Name::Keyword(Keyword::arbitrary(g)),
            Name::Int,
            Name::DoubleQuote,
            Name::Dollar,
            Name::DollarLBrace,
            Name::String,
            Name::SingleQuote,
            Name::Char,
            Name::LBrace,
            Name::RBrace,
            Name::LParen,
            Name::RParen,
            Name::LBracket,
            Name::RBracket,
            Name::LAngle,
            Name::RAngle,
            Name::Backslash,
            Name::LeftArrow,
            Name::Arrow,
            Name::FatArrow,
            Name::Dot,
            Name::DotDot,
            Name::Asterisk,
            Name::Equals,
            Name::Colon,
            Name::Comma,
            Name::Pipe,
            Name::Underscore,
            Name::Hyphen,
            Name::Plus,
            Name::Slash,
            Name::Indent(Relation::arbitrary(g), usize::arbitrary(g)),
            Name::Dedent,
            Name::Backtick,
            Name::Cmd,
            Name::Bang,
            Name::Ampersand,
            Name::LParenPipe,
            Name::PipeRParen,
        ];
        g.choose(vals).unwrap().clone()
    }
}

impl Name {
    pub fn num_variants() -> usize {
        44 + Keyword::num_variants()
    }

    pub fn from_int(ix: usize) -> Option<Self> {
        match ix {
            0 => Some(Self::Unexpected),
            1 => Some(Self::Comment),
            2 => Some(Self::Ctor),
            3 => Some(Self::Ident),
            4 => Some(Self::Keyword(Keyword::Case)),
            5 => Some(Self::Keyword(Keyword::Of)),
            6 => Some(Self::Keyword(Keyword::If)),
            7 => Some(Self::Keyword(Keyword::Then)),
            8 => Some(Self::Keyword(Keyword::Else)),
            9 => Some(Self::Keyword(Keyword::True)),
            10 => Some(Self::Keyword(Keyword::False)),
            11 => Some(Self::Keyword(Keyword::Import)),
            12 => Some(Self::Keyword(Keyword::As)),
            13 => Some(Self::Keyword(Keyword::From)),
            14 => Some(Self::Keyword(Keyword::Type)),
            15 => Some(Self::Keyword(Keyword::Class)),
            16 => Some(Self::Keyword(Keyword::Instance)),
            17 => Some(Self::Keyword(Keyword::Where)),
            18 => Some(Self::Keyword(Keyword::Let)),
            19 => Some(Self::Keyword(Keyword::In)),
            20 => Some(Self::Keyword(Keyword::Comp)),
            21 => Some(Self::Keyword(Keyword::Bind)),
            22 => Some(Self::Int),
            23 => Some(Self::DoubleQuote),
            24 => Some(Self::Dollar),
            25 => Some(Self::DollarLBrace),
            26 => Some(Self::String),
            27 => Some(Self::SingleQuote),
            28 => Some(Self::Char),
            29 => Some(Self::LBrace),
            30 => Some(Self::RBrace),
            31 => Some(Self::LParen),
            32 => Some(Self::RParen),
            33 => Some(Self::LBracket),
            34 => Some(Self::RBracket),
            35 => Some(Self::LAngle),
            36 => Some(Self::RAngle),
            37 => Some(Self::Backslash),
            38 => Some(Self::LeftArrow),
            39 => Some(Self::Arrow),
            40 => Some(Self::FatArrow),
            41 => Some(Self::Dot),
            42 => Some(Self::DotDot),
            43 => Some(Self::Asterisk),
            44 => Some(Self::Equals),
            45 => Some(Self::Colon),
            46 => Some(Self::Comma),
            47 => Some(Self::Pipe),
            48 => Some(Self::Underscore),
            49 => Some(Self::Hyphen),
            50 => Some(Self::Plus),
            51 => Some(Self::Slash),
            // INDENT_TAG => Self::Indent(_),
            53 => Some(Self::Dedent),
            54 => Some(Self::Eof),
            55 => Some(Self::Backtick),
            56 => Some(Self::Cmd),
            57 => Some(Self::Bang),
            58 => Some(Self::Ampersand),
            59 => Some(Self::LParenPipe),
            60 => Some(Self::PipeRParen),
            _ => None,
        }
    }

    pub fn to_int(&self) -> usize {
        match self {
            Self::Unexpected => 0,
            Self::Comment => 1,
            Self::Ctor => 2,
            Self::Ident => 3,
            Self::Keyword(Keyword::Case) => 4,
            Self::Keyword(Keyword::Of) => 5,
            Self::Keyword(Keyword::If) => 6,
            Self::Keyword(Keyword::Then) => 7,
            Self::Keyword(Keyword::Else) => 8,
            Self::Keyword(Keyword::True) => 9,
            Self::Keyword(Keyword::False) => 10,
            Self::Keyword(Keyword::Import) => 11,
            Self::Keyword(Keyword::As) => 12,
            Self::Keyword(Keyword::From) => 13,
            Self::Keyword(Keyword::Type) => 14,
            Self::Keyword(Keyword::Class) => 15,
            Self::Keyword(Keyword::Instance) => 16,
            Self::Keyword(Keyword::Where) => 17,
            Self::Keyword(Keyword::Let) => 18,
            Self::Keyword(Keyword::In) => 19,
            Self::Keyword(Keyword::Comp) => 20,
            Self::Keyword(Keyword::Bind) => 21,
            Self::Int => 22,
            Self::DoubleQuote => 23,
            Self::Dollar => 24,
            Self::DollarLBrace => 25,
            Self::String => 26,
            Self::SingleQuote => 27,
            Self::Char => 28,
            Self::LBrace => 29,
            Self::RBrace => 30,
            Self::LParen => 31,
            Self::RParen => 32,
            Self::LBracket => 33,
            Self::RBracket => 34,
            Self::LAngle => 35,
            Self::RAngle => 36,
            Self::Backslash => 37,
            Self::LeftArrow => 38,
            Self::Arrow => 39,
            Self::FatArrow => 40,
            Self::Dot => 41,
            Self::DotDot => 42,
            Self::Asterisk => 43,
            Self::Equals => 44,
            Self::Colon => 45,
            Self::Comma => 46,
            Self::Pipe => 47,
            Self::Underscore => 48,
            Self::Hyphen => 49,
            Self::Plus => 50,
            Self::Slash => 51,
            Self::Indent(_, _) => INDENT_TAG,
            Self::Dedent => 53,
            Self::Eof => 54,
            Self::Backtick => 55,
            Self::Cmd => 56,
            Self::Bang => 57,
            Self::Ampersand => 58,
            Self::LParenPipe => 59,
            Self::PipeRParen => 60,
        }
    }

    pub fn render(&self) -> String {
        match self {
            Name::Unexpected => String::from("unexpected"),
            Name::Ident => String::from("identifier"),
            Name::Keyword(keyword) => String::from(keyword.to_string()),
            Name::Int => String::from("integer"),
            Name::Comment => String::from("comment"),
            Name::DoubleQuote => String::from("'\"'"),
            Name::Dollar => String::from("'$'"),
            Name::DollarLBrace => String::from("'${'"),
            Name::String => String::from("string"),
            Name::SingleQuote => String::from("'"),
            Name::Char => String::from("character"),
            Name::LBrace => String::from("'{'"),
            Name::RBrace => String::from("'}'"),
            Name::LParen => String::from("'('"),
            Name::RParen => String::from("')'"),
            Name::LBracket => String::from("'['"),
            Name::RBracket => String::from("']'"),
            Name::Backslash => String::from("'\\'"),
            Name::LeftArrow => String::from("'<-'"),
            Name::Arrow => String::from("'->'"),
            Name::FatArrow => String::from("'=>'"),
            Name::Dot => String::from("'.'"),
            Name::DotDot => String::from("'..'"),
            Name::Asterisk => String::from("'*'"),
            Name::Equals => String::from("'='"),
            Name::Colon => String::from("':'"),
            Name::Comma => String::from("','"),
            Name::Underscore => String::from("'_'"),
            Name::Hyphen => String::from("'-'"),
            Name::Plus => String::from("'+'"),
            Name::Slash => String::from("'/'"),
            Name::Indent(relation, n) => {
                format!(
                    "indent ({} {})",
                    match relation {
                        Relation::Gt => ">",
                        Relation::Gte => ">=",
                        Relation::Eq => "==",
                    },
                    n
                )
            }
            Name::Dedent => String::from("dedent"),
            Name::Ctor => String::from("constructor"),
            Name::Pipe => String::from("'|'"),
            Name::LAngle => String::from("'<'"),
            Name::RAngle => String::from("'>'"),
            Name::Eof => String::from("end of input"),
            Name::Backtick => String::from("'`'"),
            Name::Cmd => String::from("command fragment"),
            Name::Bang => String::from('!'),
            Name::Ampersand => String::from('&'),
            Name::LParenPipe => String::from("(|"),
            Name::PipeRParen => String::from("|)"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum Data {
    Unexpected(char),
    Eof,

    Comment { length: usize },

    Ctor,
    Ident(Rc<str>),
    Int { value: usize, length: usize },

    DoubleQuote,
    Dollar,
    DollarLBrace,
    String { value: String, length: usize },

    SingleQuote,
    Char { value: char, length: usize },

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LAngle,
    RAngle,

    Backslash,
    LeftArrow,
    Arrow,
    FatArrow,

    Dot,
    DotDot,

    Asterisk,

    Equals,
    Colon,

    Comma,
    Pipe,

    Underscore,

    Hyphen,
    Plus,
    Slash,

    Backtick,
    Cmd(Rc<str>),

    Bang,
    Ampersand,

    LParenPipe,
    PipeRParen,
}

impl Data {
    /// The number of bytes that were consumed to produce the token.
    pub fn length(&self) -> usize {
        match self {
            Data::Unexpected(_) => 1,
            Data::Eof => 0,
            Data::Comment { length } => *length,
            Data::Ident(s) => s.len(),
            Data::Int { value: _, length } => *length,
            Data::LBrace => 1,
            Data::RBrace => 1,
            Data::LParen => 1,
            Data::RParen => 1,
            Data::LBracket => 1,
            Data::RBracket => 1,
            Data::Backslash => 1,
            Data::LeftArrow => 2,
            Data::Arrow => 2,
            Data::FatArrow => 2,
            Data::Dot => 1,
            Data::DotDot => 2,
            Data::Asterisk => 1,
            Data::Equals => 1,
            Data::Colon => 1,
            Data::Comma => 1,
            Data::Underscore => 1,
            Data::Hyphen => 1,
            Data::Plus => 1,
            Data::Slash => 1,
            Data::DoubleQuote => 1,
            Data::SingleQuote => 1,
            Data::Dollar => 1,
            Data::DollarLBrace => 2,
            Data::String { length, .. } => *length,
            Data::Char { length, .. } => *length,
            Data::Pipe => 1,
            Data::LAngle => 1,
            Data::RAngle => 1,
            Data::Backtick => 1,
            Data::Cmd(value) => value.len(),
            Data::Bang => 1,
            Data::Ampersand => 1,
            Data::LParenPipe => 2,
            Data::PipeRParen => 2,

            Data::Ctor => panic!("Data::Ctor.len()"),
        }
    }

    pub fn name(&self) -> Name {
        match self {
            Data::Unexpected(_) => Name::Unexpected,
            Data::Eof => Name::Eof,
            Data::Comment { .. } => Name::Comment,
            Data::Ctor => Name::Ctor,
            Data::Ident(_) => Name::Ident,
            Data::Int { .. } => Name::Int,
            Data::DoubleQuote => Name::DoubleQuote,
            Data::Dollar => Name::Dollar,
            Data::DollarLBrace => Name::DollarLBrace,
            Data::String { .. } => Name::String,
            Data::SingleQuote => Name::SingleQuote,
            Data::Char { .. } => Name::Char,
            Data::LBrace => Name::LBrace,
            Data::RBrace => Name::RBrace,
            Data::LParen => Name::LParen,
            Data::RParen => Name::RParen,
            Data::LBracket => Name::LBracket,
            Data::RBracket => Name::RBracket,
            Data::LAngle => Name::LAngle,
            Data::RAngle => Name::RAngle,
            Data::Backslash => Name::Backslash,
            Data::LeftArrow => Name::LeftArrow,
            Data::Arrow => Name::Arrow,
            Data::FatArrow => Name::FatArrow,
            Data::Dot => Name::Dot,
            Data::DotDot => Name::DotDot,
            Data::Asterisk => Name::Asterisk,
            Data::Equals => Name::Equals,
            Data::Colon => Name::Colon,
            Data::Comma => Name::Comma,
            Data::Pipe => Name::Pipe,
            Data::Underscore => Name::Underscore,
            Data::Hyphen => Name::Hyphen,
            Data::Plus => Name::Plus,
            Data::Slash => Name::Slash,
            Data::Backtick => Name::Backtick,
            Data::Cmd(_) => Name::Cmd,
            Data::Bang => Name::Bang,
            Data::Ampersand => Name::Ampersand,
            Data::LParenPipe => Name::LParenPipe,
            Data::PipeRParen => Name::PipeRParen,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    /// The token's position in the input stream, in bytes
    pub pos: usize,

    /// The token's column
    ///
    /// To facilitate indentation-sensitive parsing, an input stream is divided
    /// into rows and columns. Rows are terminated by the '\\n' character. The
    /// token's column is the position at which it can be found in the row, starting
    /// from 0.
    ///
    /// References:
    ///
    /// > Adams, M. D. (2013).
    /// > Principled parsing for indentation-sensitive languages: revisiting landin's offside rule.
    /// > ACM SIGPLAN Notices, 48(1), 511-522.
    pub column: usize,

    pub data: Data,
}
