use crate::syntax;

#[derive(Debug, PartialEq, Eq)]
pub enum Pattern {
    Name,
    Record { names: usize, rest: bool },
    Variant { name: String },
    Wildcard,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Branch {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StringPart {
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
    Unit,
}

impl Expr {
    pub fn mk_app(a: Expr, b: Expr) -> Expr {
        Expr::App(Box::new(a), Box::new(b))
    }

    pub fn mk_lam(arg: Pattern, body: Expr) -> Expr {
        Expr::Lam {
            arg,
            body: Box::new(body),
        }
    }

    pub fn mk_ifthenelse(x: Expr, y: Expr, z: Expr) -> Expr {
        Expr::IfThenElse(Box::new(x), Box::new(y), Box::new(z))
    }
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
