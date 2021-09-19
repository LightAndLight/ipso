use std::rc::Rc;

use crate::syntax::Keyword;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
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

impl Name {
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
