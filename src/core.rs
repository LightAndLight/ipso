use crate::evidence::EVar;
use crate::syntax;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pattern {
    Name,
    Record { names: usize, rest: bool },
    Variant { name: String },
    Wildcard,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Branch {
    pub pattern: Pattern,
    pub body: Expr,
}

impl Branch {
    pub fn subst_evar<F: FnMut(&EVar) -> Expr>(&self, f: &mut F) -> Self {
        Branch {
            pattern: self.pattern.clone(),
            body: self.body.subst_evar(f),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StringPart {
    String(String),
    Expr(Expr),
}

impl StringPart {
    pub fn subst_evar<F: FnMut(&EVar) -> Expr>(&self, f: &mut F) -> Self {
        match self {
            StringPart::String(s) => StringPart::String(s.clone()),
            StringPart::Expr(e) => StringPart::Expr(e.subst_evar(f)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Builtin {
    MapIO,
    PureIO,
    BindIO,
    Trace,
    ToUtf8,
    Stdout,
    Stdin,
    WriteStdout,
    ReadLineStdin,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Var(usize),
    EVar(EVar),
    Name(String),
    Builtin(Builtin),

    App(Box<Expr>, Box<Expr>),
    Lam { arg: bool, body: Box<Expr> },

    True,
    False,
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),

    Int(u32),

    Binop(syntax::Binop, Box<Expr>, Box<Expr>),

    Char(char),

    String(Vec<StringPart>),

    Array(Vec<Expr>),

    Extend(Box<Expr>, Box<Expr>, Box<Expr>),
    Record(Vec<(Expr, Expr)>),
    Project(Box<Expr>, Box<Expr>),

    Variant(Box<Expr>, Box<Expr>),
    Case(Box<Expr>, Vec<Branch>),
    Unit,
}

impl Expr {
    pub fn mk_app(a: Expr, b: Expr) -> Expr {
        Expr::App(Box::new(a), Box::new(b))
    }

    pub fn mk_lam(arg: bool, body: Expr) -> Expr {
        Expr::Lam {
            arg,
            body: Box::new(body),
        }
    }

    pub fn mk_ifthenelse(x: Expr, y: Expr, z: Expr) -> Expr {
        Expr::IfThenElse(Box::new(x), Box::new(y), Box::new(z))
    }

    pub fn mk_record(fields: Vec<(Expr, Expr)>, rest: Option<Expr>) -> Expr {
        match rest {
            None => Expr::Record(fields),
            Some(rest) => fields.into_iter().rev().fold(rest, |acc, (ev, field)| {
                Expr::Extend(Box::new(ev), Box::new(field), Box::new(acc))
            }),
        }
    }

    pub fn mk_project(expr: Expr, offset: Expr) -> Expr {
        Expr::Project(Box::new(expr), Box::new(offset))
    }

    pub fn mk_extend(field: Expr, value: Expr, rest: Expr) -> Expr {
        Expr::Extend(Box::new(field), Box::new(value), Box::new(rest))
    }

    pub fn mk_variant(tag: Expr, expr: Expr) -> Expr {
        Expr::Variant(Box::new(tag), Box::new(expr))
    }

    pub fn mk_binop(op: syntax::Binop, a: Expr, b: Expr) -> Expr {
        Expr::Binop(op, Box::new(a), Box::new(b))
    }

    pub fn mk_case(expr: Expr, branches: Vec<Branch>) -> Expr {
        Expr::Case(Box::new(expr), branches)
    }

    pub fn mk_evar(ev: usize) -> Expr {
        Expr::EVar(EVar(ev))
    }

    pub fn subst_evar<F: FnMut(&EVar) -> Expr>(&self, f: &mut F) -> Expr {
        match self {
            Expr::Var(n) => Expr::Var(*n),
            Expr::EVar(v) => f(v),
            Expr::Name(n) => Expr::Name(n.clone()),
            Expr::Builtin(b) => Expr::Builtin(*b),
            Expr::App(a, b) => Expr::mk_app(a.subst_evar(f), b.subst_evar(f)),
            Expr::Lam { arg, body } => Expr::mk_lam(*arg, body.subst_evar(f)),
            Expr::True => Expr::True,
            Expr::False => Expr::False,
            Expr::IfThenElse(a, b, c) => {
                Expr::mk_ifthenelse(a.subst_evar(f), b.subst_evar(f), c.subst_evar(f))
            }
            Expr::Int(n) => Expr::Int(*n),
            Expr::Binop(a, b, c) => Expr::mk_binop(*a, b.subst_evar(f), c.subst_evar(f)),
            Expr::Char(c) => Expr::Char(*c),
            Expr::String(s) => Expr::String(s.iter().map(|x| x.subst_evar(f)).collect()),
            Expr::Array(xs) => Expr::Array(xs.iter().map(|x| x.subst_evar(f)).collect()),
            Expr::Extend(a, b, c) => {
                Expr::mk_extend(a.subst_evar(f), b.subst_evar(f), c.subst_evar(f))
            }
            Expr::Record(xs) => Expr::Record(
                xs.iter()
                    .map(|(x, y)| (x.subst_evar(f), y.subst_evar(f)))
                    .collect(),
            ),
            Expr::Project(a, b) => Expr::mk_project(a.subst_evar(f), b.subst_evar(f)),
            Expr::Variant(a, b) => Expr::mk_variant(a.subst_evar(f), b.subst_evar(f)),
            Expr::Case(a, bs) => Expr::mk_case(
                a.subst_evar(f),
                bs.iter().map(|b| b.subst_evar(f)).collect(),
            ),
            Expr::Unit => Expr::Unit,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeSig {
    pub ty_vars: Vec<syntax::Kind>,
    pub body: syntax::Type<usize>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassMember {
    name: String,
    sig: TypeSig,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration {
    BuiltinType {
        name: String,
        kind: syntax::Kind,
    },
    Definition {
        name: String,
        sig: TypeSig,
        body: Expr,
    },
    TypeAlias {
        name: String,
        args: Vec<syntax::Kind>,
        body: syntax::Type<usize>,
    },
    Import {
        module: String,
        name: Option<String>,
    },
    FromImport {
        module: String,
        name: syntax::Names,
    },
    Class {
        ty_vars: Vec<syntax::Kind>,
        supers: Vec<syntax::Type<usize>>,
        name: String,
        args: Vec<usize>,
        members: Vec<ClassMember>,
    },
    Instance {
        ty_vars: Vec<syntax::Kind>,
        assumes: Vec<syntax::Type<usize>>,
        head: syntax::Type<usize>,
        dict: Expr,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub decls: Vec<Declaration>,
}
