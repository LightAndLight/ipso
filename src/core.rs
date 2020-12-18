use crate::syntax;

#[derive(Debug, PartialEq, Eq)]
pub enum Pattern {
    Name,
    Record { fields: usize, rest: bool },
    Variant { tag: usize, args: usize },
    Wildcard,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Branch {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Eq)]
enum StringPart {
    String(String),
    Expr(Expr),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Var(usize),

    App(Box<Expr>, Box<Expr>),
    Lam { arg: Pattern, body: Box<Expr> },

    True,
    False,
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),

    Int(u32),

    Binop(syntax::Binop, Box<Expr>, Box<Expr>),

    Char(char),

    String(Vec<StringPart>),

    Array(Vec<Expr>),

    Append(Vec<Expr>, Box<Expr>),
    Record(Vec<Expr>),
    Project(Box<Expr>, usize),

    Variant(usize, Vec<Expr>),
    Case(Box<Expr>, Vec<Branch>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration {
    Definition {
        name: String,
        ty: syntax::Type,
        body: Expr,
    },
    TypeAlias {
        name: String,
        args: Vec<String>,
        body: syntax::Type,
    },
    Import {
        module: String,
        name: Option<String>,
    },
    FromImport {
        module: String,
        name: syntax::Names,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub decls: Vec<Declaration>,
}