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
        match self {
            Case => actual == "case",
            Of => actual == "of",
            If => actual == "if",
            Then => actual == "then",
            Else => actual == "else",
            True => actual == "true",
            False => actual == "false",
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
enum StringPart {
    String(String),
    Expr(Expr),
}

#[derive(Debug)]
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

#[derive(Debug)]
struct Branch {
    pattern: Pattern,
    body: Expr,
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
enum Names {
    All,
    Names(Vec<String>),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Module {
    pub decls: Vec<Declaration>,
}
