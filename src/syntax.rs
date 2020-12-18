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
pub enum Binop {
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
pub enum StringPart {
    String(String),
    Expr(Expr),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Pattern {
    Name(String),
    Record {
        names: Vec<String>,
        rest: Option<String>,
    },
    Variant {
        name: String,
        args: Vec<String>,
    },
    Wildcard,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Branch {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Var(String),

    App(Box<Expr>, Box<Expr>),
    Lam {
        args: Vec<Pattern>,
        body: Box<Expr>,
    },

    True,
    False,
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),

    Int(u32),

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

    Unit,
}

impl Expr {
    pub fn mk_var(v: &str) -> Expr {
        Expr::Var(String::from(v))
    }

    pub fn mk_lam(args: Vec<Pattern>, body: Expr) -> Expr {
        Expr::Lam {
            args,
            body: Box::new(body),
        }
    }

    pub fn mk_case(cond: Expr, branches: Vec<Branch>) -> Expr {
        Expr::Case(Box::new(cond), branches)
    }

    pub fn mk_app(a: Expr, b: Expr) -> Expr {
        Expr::App(Box::new(a), Box::new(b))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Name(String),
    Bool,
    Int,
    Char,
    String,
    Arrow,
    FatArrow,
    Constraints(Vec<Type>),
    Array,
    Record,
    Variant,
    IO,
    App(Box<Type>, Box<Type>),
    RowNil,
    RowCons(String, Box<Type>, Box<Type>),
    Unit,
}

impl Type {
    pub fn mk_app(a: Type, b: Type) -> Type {
        Type::App(Box::new(a), Box::new(b))
    }

    pub fn mk_arrow(a: Type, b: Type) -> Type {
        Type::mk_app(Type::mk_app(Type::Arrow, a), b)
    }

    pub fn mk_fatarrow(a: Type, b: Type) -> Type {
        Type::mk_app(Type::mk_app(Type::FatArrow, a), b)
    }

    pub fn mk_name(s: &str) -> Type {
        Type::Name(String::from(s))
    }

    pub fn mk_rowcons(field: &str, a: Type, b: Type) -> Type {
        Type::RowCons(String::from(field), Box::new(a), Box::new(b))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Names {
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
