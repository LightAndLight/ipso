use lazy_static::lazy_static;
use std::collections::HashMap;

pub enum Keyword {
    Case,
    Of,
    If,
    Then,
    Else,
    True,
    False,
    Import,
    As,
    From,
}

impl Keyword {
    pub fn matches(&self, actual: &String) -> bool {
        self.to_string() == actual
    }

    pub fn to_string(&self) -> &str {
        match self {
            Keyword::Case => "case",
            Keyword::Of => "of",
            Keyword::If => "if",
            Keyword::Then => "then",
            Keyword::Else => "else",
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Import => "import",
            Keyword::As => "as",
            Keyword::From => "from",
        }
    }
}

lazy_static! {
    static ref KEYWORDS: Vec<&'static str> =
        vec!["case", "of", "if", "then", "else", "true", "false", "import", "as", "from"];
}

pub fn is_keyword(val: &String) -> bool {
    KEYWORDS.contains(&val.as_str())
}

#[derive(Debug, PartialEq, Eq)]
enum Binop {
    Add,
    Multiply,
    Subtract,
    Divide,

    Append,

    Or,
    And,

    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
}

#[derive(Debug, PartialEq, Eq)]
enum StringPart {
    String(String),
    Expr(Expr),
}

#[derive(Debug, PartialEq, Eq)]
enum Pattern {
    Name(String),
    Record {
        names: Vec<String>,
        rest: Option<String>,
    },
    Variant {
        name: String,
        args: Vec<Option<String>>,
    },
    Wildcard,
}

#[derive(Debug, PartialEq, Eq)]
struct Branch {
    pattern: Pattern,
    body: Expr,
}

#[derive(Debug, PartialEq, Eq)]
enum Expr {
    Var(String),

    App(Box<Expr>, Box<Expr>),
    Lam {
        args: Vec<Pattern>,
        body: Box<Expr>,
    },

    True,
    False,
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),

    Int(i32),

    Binop(Binop, Box<Expr>, Box<Expr>),

    Char(char),

    String(Vec<StringPart>),

    Array(Vec<Expr>),

    Record {
        fields: HashMap<String, Expr>,
        rest: Option<Box<Expr>>,
    },
    Project(Box<Expr>, String),

    Variant(String, Vec<Expr>),
    Case(Box<Expr>, Vec<Branch>),
}

#[derive(Debug, PartialEq, Eq)]
enum Type {
    Var(String),
    Name(String),
    Bool,
    Int,
    Char,
    String,
    Arrow,
    FatArrow,
    Array,
    Record(Vec<(String, Type)>, Option<String>),
    Variant(Vec<(String, Type)>, Option<String>),
    IO,
    App(Box<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Eq)]
enum Names {
    All,
    Names(Vec<String>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration {
    Definition {
        name: String,
        ty: Type,
        args: Vec<Pattern>,
        body: Expr,
    },
    TypeAlias {
        name: String,
        args: Vec<String>,
        body: Type,
    },
    Import {
        module: String,
        name: Option<String>,
    },
    FromImport {
        module: String,
        name: Names,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub decls: Vec<Declaration>,
}
