mod test;

use ipso_syntax::Keyword;
use quickcheck::Arbitrary;
use std::rc::Rc;

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
    Arrow,
    FatArrow,
    Dot,
    Asterisk,
    Equals,
    Colon,
    Comma,
    Pipe,
    Underscore,
    Hyphen,
    Plus,
    Slash,
    Indent(usize),
    Dedent,
    Space,
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
            Name::Arrow,
            Name::FatArrow,
            Name::Dot,
            Name::Asterisk,
            Name::Equals,
            Name::Colon,
            Name::Comma,
            Name::Pipe,
            Name::Underscore,
            Name::Hyphen,
            Name::Plus,
            Name::Slash,
            Name::Indent(usize::arbitrary(g)),
            Name::Dedent,
            Name::Space,
        ];
        g.choose(vals).unwrap().clone()
    }
}

impl Name {
    pub fn num_variants() -> usize {
        35 + Keyword::num_variants()
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
            20 => Some(Self::Int),
            21 => Some(Self::DoubleQuote),
            22 => Some(Self::Dollar),
            23 => Some(Self::DollarLBrace),
            24 => Some(Self::String),
            25 => Some(Self::SingleQuote),
            26 => Some(Self::Char),
            27 => Some(Self::LBrace),
            28 => Some(Self::RBrace),
            29 => Some(Self::LParen),
            30 => Some(Self::RParen),
            31 => Some(Self::LBracket),
            32 => Some(Self::RBracket),
            33 => Some(Self::LAngle),
            34 => Some(Self::RAngle),
            35 => Some(Self::Backslash),
            36 => Some(Self::Arrow),
            37 => Some(Self::FatArrow),
            38 => Some(Self::Dot),
            39 => Some(Self::Asterisk),
            40 => Some(Self::Equals),
            41 => Some(Self::Colon),
            42 => Some(Self::Comma),
            43 => Some(Self::Pipe),
            44 => Some(Self::Underscore),
            45 => Some(Self::Hyphen),
            46 => Some(Self::Plus),
            47 => Some(Self::Slash),
            // 48 => Self::Indent(_),
            49 => Some(Self::Dedent),
            50 => Some(Self::Space),
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
            Self::Int => 20,
            Self::DoubleQuote => 21,
            Self::Dollar => 22,
            Self::DollarLBrace => 23,
            Self::String => 24,
            Self::SingleQuote => 25,
            Self::Char => 26,
            Self::LBrace => 27,
            Self::RBrace => 28,
            Self::LParen => 29,
            Self::RParen => 30,
            Self::LBracket => 31,
            Self::RBracket => 32,
            Self::LAngle => 33,
            Self::RAngle => 34,
            Self::Backslash => 35,
            Self::Arrow => 36,
            Self::FatArrow => 37,
            Self::Dot => 38,
            Self::Asterisk => 39,
            Self::Equals => 40,
            Self::Colon => 41,
            Self::Comma => 42,
            Self::Pipe => 43,
            Self::Underscore => 44,
            Self::Hyphen => 45,
            Self::Plus => 46,
            Self::Slash => 47,
            Self::Indent(_) => 48,
            Self::Dedent => 49,
            Self::Space => 50,
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
            Name::Arrow => String::from("'->'"),
            Name::FatArrow => String::from("'=>'"),
            Name::Dot => String::from("'.'"),
            Name::Asterisk => String::from("'*'"),
            Name::Equals => String::from("'='"),
            Name::Colon => String::from("':'"),
            Name::Comma => String::from("','"),
            Name::Underscore => String::from("'_'"),
            Name::Hyphen => String::from("'-'"),
            Name::Plus => String::from("'+'"),
            Name::Slash => String::from("'/'"),
            Name::Indent(n) => {
                if *n == 0 {
                    String::from("newline")
                } else {
                    format!("indent ({})", n)
                }
            }
            Name::Dedent => String::from("dedent"),
            Name::Space => String::from("space"),
            Name::Ctor => String::from("constructor"),
            Name::Pipe => String::from("'|'"),
            Name::LAngle => String::from("'<'"),
            Name::RAngle => String::from("'>'"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum Data {
    Unexpected(char),

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
    Arrow,
    FatArrow,

    Dot,

    Asterisk,

    Equals,
    Colon,

    Comma,
    Pipe,

    Underscore,

    Hyphen,
    Plus,
    Slash,

    Indent(usize),
    Dedent,
    Space,
}

impl Data {
    pub fn render(&self) -> String {
        match self {
            Data::Unexpected(_) => String::from("unexpected"),
            Data::Ident(s) => {
                if s.is_empty() {
                    String::from("identifier")
                } else {
                    format!("\"{}\"", s)
                }
            }
            Data::Int { value, length } => {
                if *length == 0 {
                    String::from("integer")
                } else {
                    format!("\"{}\"", value)
                }
            }
            Data::Comment { .. } => String::from("comment"),
            Data::DoubleQuote => String::from("'\"'"),
            Data::Dollar => String::from("'$'"),
            Data::DollarLBrace => String::from("'${'"),
            Data::String { value, .. } => format!("{:?}", value),
            Data::SingleQuote => String::from("'"),
            Data::Char { value, .. } => format!("'{}'", value),
            Data::LBrace => String::from("'{'"),
            Data::RBrace => String::from("'}'"),
            Data::LParen => String::from("'('"),
            Data::RParen => String::from("')'"),
            Data::LBracket => String::from("'['"),
            Data::RBracket => String::from("']'"),
            Data::Backslash => String::from("'\\'"),
            Data::Arrow => String::from("'->'"),
            Data::FatArrow => String::from("'=>'"),
            Data::Dot => String::from("'.'"),
            Data::Asterisk => String::from("'*'"),
            Data::Equals => String::from("'='"),
            Data::Colon => String::from("':'"),
            Data::Comma => String::from("','"),
            Data::Underscore => String::from("'_'"),
            Data::Hyphen => String::from("'-'"),
            Data::Plus => String::from("'+'"),
            Data::Slash => String::from("'/'"),
            Data::Indent(n) => {
                if *n == 0 {
                    String::from("newline")
                } else {
                    format!("indent ({})", n)
                }
            }
            Data::Dedent => String::from("dedent"),
            Data::Space => String::from("space"),
            Data::Ctor => String::from("constructor"),
            Data::Pipe => String::from("'|'"),
            Data::LAngle => String::from("'<'"),
            Data::RAngle => String::from("'>'"),
        }
    }

    pub fn length(&self) -> usize {
        match self {
            Data::Unexpected(_) => 1,
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
            Data::Arrow => 2,
            Data::FatArrow => 2,
            Data::Dot => 1,
            Data::Asterisk => 1,
            Data::Equals => 1,
            Data::Colon => 1,
            Data::Comma => 1,
            Data::Underscore => 1,
            Data::Hyphen => 1,
            Data::Plus => 1,
            Data::Slash => 1,
            Data::Indent(n) => n + 1,
            Data::Space => 1,
            Data::DoubleQuote => 1,
            Data::SingleQuote => 1,
            Data::Dollar => 1,
            Data::DollarLBrace => 2,
            Data::String { length, .. } => *length,
            Data::Char { length, .. } => *length,
            Data::Pipe => 1,
            Data::LAngle => 1,
            Data::RAngle => 1,

            Data::Dedent => panic!("Data::Dedent.len()"),
            Data::Ctor => panic!("Data::Ctor.len()"),
        }
    }

    pub fn name(&self) -> Name {
        match self {
            Data::Unexpected(_) => Name::Unexpected,
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
            Data::Arrow => Name::Arrow,
            Data::FatArrow => Name::FatArrow,
            Data::Dot => Name::Dot,
            Data::Asterisk => Name::Asterisk,
            Data::Equals => Name::Equals,
            Data::Colon => Name::Colon,
            Data::Comma => Name::Comma,
            Data::Pipe => Name::Pipe,
            Data::Underscore => Name::Underscore,
            Data::Hyphen => Name::Hyphen,
            Data::Plus => Name::Plus,
            Data::Slash => Name::Slash,
            Data::Indent(n) => Name::Indent(*n),
            Data::Dedent => Name::Dedent,
            Data::Space => Name::Space,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub pos: usize,
    pub data: Data,
}
