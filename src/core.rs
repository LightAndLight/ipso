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
    pub fn subst_evar<E, F: FnMut(&EVar) -> Result<Expr, E>>(&self, f: &mut F) -> Result<Self, E> {
        self.body.subst_evar(f).map(|body| Branch {
            pattern: self.pattern.clone(),
            body,
        })
    }

    pub fn abstract_evar(&self, depth: usize, ev: EVar) -> Self {
        Branch {
            pattern: self.pattern.clone(),
            body: self.body.abstract_evar(
                depth
                    + match &self.pattern {
                        Pattern::Name => 1,
                        Pattern::Record { names, rest } => names + if *rest { 1 } else { 0 },
                        Pattern::Variant { name } => 1,
                        Pattern::Wildcard => 0,
                    },
                ev,
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StringPart {
    String(String),
    Expr(Expr),
}

impl StringPart {
    pub fn subst_evar<E, F: FnMut(&EVar) -> Result<Expr, E>>(&self, f: &mut F) -> Result<Self, E> {
        match self {
            StringPart::String(s) => Ok(StringPart::String(s.clone())),
            StringPart::Expr(e) => e.subst_evar(f).map(|e| StringPart::Expr(e)),
        }
    }

    pub fn abstract_evar(&self, depth: usize, ev: EVar) -> Self {
        match self {
            StringPart::String(s) => StringPart::String(s.clone()),
            StringPart::Expr(e) => StringPart::Expr(e.abstract_evar(depth, ev)),
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

    pub fn subst_evar<E, F: FnMut(&EVar) -> Result<Expr, E>>(&self, f: &mut F) -> Result<Expr, E> {
        match self {
            Expr::Var(n) => Ok(Expr::Var(*n)),
            Expr::EVar(v) => f(v),
            Expr::Name(n) => Ok(Expr::Name(n.clone())),
            Expr::Builtin(b) => Ok(Expr::Builtin(*b)),
            Expr::App(a, b) => a
                .subst_evar(f)
                .and_then(|a| b.subst_evar(f).map(|b| Expr::mk_app(a, b))),
            Expr::Lam { arg, body } => body.subst_evar(f).map(|body| Expr::mk_lam(*arg, body)),
            Expr::True => Ok(Expr::True),
            Expr::False => Ok(Expr::False),
            Expr::IfThenElse(a, b, c) => a.subst_evar(f).and_then(|a| {
                b.subst_evar(f)
                    .and_then(|b| c.subst_evar(f).map(|c| Expr::mk_ifthenelse(a, b, c)))
            }),
            Expr::Int(n) => Ok(Expr::Int(*n)),
            Expr::Binop(a, b, c) => b
                .subst_evar(f)
                .and_then(|b| c.subst_evar(f).map(|c| Expr::mk_binop(*a, b, c))),
            Expr::Char(c) => Ok(Expr::Char(*c)),
            Expr::String(parts) => {
                let mut new_parts = Vec::new();
                for part in parts {
                    match part.subst_evar(f) {
                        Err(err) => {
                            return Err(err);
                        }
                        Ok(new_part) => {
                            new_parts.push(new_part);
                        }
                    }
                }
                Ok(Expr::String(new_parts))
            }
            Expr::Array(items) => {
                let mut new_items = Vec::new();
                for item in items {
                    match item.subst_evar(f) {
                        Err(err) => {
                            return Err(err);
                        }
                        Ok(new_item) => {
                            new_items.push(new_item);
                        }
                    }
                }
                Ok(Expr::Array(new_items))
            }
            Expr::Extend(a, b, c) => a.subst_evar(f).and_then(|a| {
                b.subst_evar(f)
                    .and_then(|b| c.subst_evar(f).map(|c| Expr::mk_extend(a, b, c)))
            }),
            Expr::Record(items) => {
                let mut new_items = Vec::new();
                for (a, b) in items {
                    match a
                        .subst_evar(f)
                        .and_then(|a| b.subst_evar(f).map(|b| (a, b)))
                    {
                        Err(err) => {
                            return Err(err);
                        }
                        Ok(new_item) => {
                            new_items.push(new_item);
                        }
                    }
                }
                Ok(Expr::Record(new_items))
            }
            Expr::Project(a, b) => a
                .subst_evar(f)
                .and_then(|a| b.subst_evar(f).map(|b| Expr::mk_project(a, b))),
            Expr::Variant(a, b) => a
                .subst_evar(f)
                .and_then(|a| b.subst_evar(f).map(|b| Expr::mk_variant(a, b))),
            Expr::Case(a, bs) => a.subst_evar(f).and_then(|a| {
                let mut new_bs = Vec::new();
                for b in bs {
                    match b.subst_evar(f) {
                        Err(err) => {
                            return Err(err);
                        }
                        Ok(new_b) => {
                            new_bs.push(new_b);
                        }
                    }
                }
                Ok(Expr::mk_case(a, new_bs))
            }),

            Expr::Unit => Ok(Expr::Unit),
        }
    }

    pub fn abstract_evar(&self, depth: usize, ev: EVar) -> Expr {
        let body = match self {
            Expr::Var(n) => Expr::Var(n + 1),
            Expr::EVar(current_ev) => {
                if ev == *current_ev {
                    Expr::Var(depth)
                } else {
                    Expr::EVar(*current_ev)
                }
            }
            Expr::Name(n) => Expr::Name(n.clone()),
            Expr::Builtin(b) => Expr::Builtin(*b),
            Expr::App(a, b) => Expr::mk_app(a.abstract_evar(depth, ev), b.abstract_evar(depth, ev)),
            Expr::Lam { arg, body } => Expr::mk_lam(
                *arg,
                body.abstract_evar(if *arg { depth + 1 } else { depth }, ev),
            ),
            Expr::True => Expr::True,
            Expr::False => Expr::False,
            Expr::IfThenElse(a, b, c) => Expr::mk_ifthenelse(
                a.abstract_evar(depth, ev),
                b.abstract_evar(depth, ev),
                c.abstract_evar(depth, ev),
            ),
            Expr::Int(n) => Expr::Int(*n),
            Expr::Binop(a, b, c) => {
                Expr::mk_binop(*a, b.abstract_evar(depth, ev), c.abstract_evar(depth, ev))
            }
            Expr::Char(c) => Expr::Char(*c),
            Expr::String(parts) => Expr::String(
                parts
                    .iter()
                    .map(|part| part.abstract_evar(depth, ev))
                    .collect(),
            ),
            Expr::Array(items) => Expr::Array(
                items
                    .iter()
                    .map(|item| item.abstract_evar(depth, ev))
                    .collect(),
            ),
            Expr::Extend(a, b, c) => Expr::mk_extend(
                a.abstract_evar(depth, ev),
                b.abstract_evar(depth, ev),
                c.abstract_evar(depth, ev),
            ),
            Expr::Record(items) => Expr::Record(
                items
                    .iter()
                    .map(|(a, b)| (a.abstract_evar(depth, ev), b.abstract_evar(depth, ev)))
                    .collect(),
            ),
            Expr::Project(a, b) => {
                Expr::mk_project(a.abstract_evar(depth, ev), b.abstract_evar(depth, ev))
            }
            Expr::Variant(a, b) => {
                Expr::mk_variant(a.abstract_evar(depth, ev), b.abstract_evar(depth, ev))
            }
            Expr::Case(a, bs) => Expr::mk_case(
                a.abstract_evar(depth, ev),
                bs.iter().map(|b| b.abstract_evar(depth, ev)).collect(),
            ),
            Expr::Unit => Expr::Unit,
        };
        Expr::mk_lam(true, body)
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
