use std::{
    cmp,
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

use ipso_syntax::{self as syntax, ModuleName, Type};
use ipso_util::iter::Step;

#[cfg(test)]
mod test;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pattern {
    Name,
    Record { names: Vec<Expr>, rest: bool },
    Variant { tag: Rc<Expr> },
    Wildcard,
}

impl Pattern {
    pub fn map_expr<F: Fn(&Expr) -> Expr>(&self, f: F) -> Self {
        match self {
            Pattern::Name => Pattern::Name,
            Pattern::Record { names, rest } => Pattern::Record {
                names: names.iter().map(|name| f(name)).collect(),
                rest: *rest,
            },
            Pattern::Variant { tag } => Pattern::mk_variant(f(tag)),
            Pattern::Wildcard => Pattern::Wildcard,
        }
    }

    pub fn mk_variant(tag: Expr) -> Pattern {
        Pattern::Variant { tag: Rc::new(tag) }
    }

    pub fn subst_placeholder<E, F: FnMut(&Placeholder) -> Result<Expr, E>>(
        &self,
        f: &mut F,
    ) -> Result<Self, E> {
        match self {
            Pattern::Name => Ok(Pattern::Name),
            Pattern::Record { names, rest } => {
                let names = {
                    let mut new_names = Vec::new();
                    for name in names {
                        match name.subst_placeholder(f) {
                            Err(err) => return Err(err),
                            Ok(new_name) => {
                                new_names.push(new_name);
                            }
                        }
                    }
                    new_names
                };
                Ok(Pattern::Record { names, rest: *rest })
            }
            Pattern::Variant { tag } => tag.subst_placeholder(f).map(Pattern::mk_variant),
            Pattern::Wildcard => Ok(Pattern::Wildcard),
        }
    }

    pub fn __instantiate(&self, depth: usize, val: &Expr) -> Self {
        match self {
            Pattern::Name => Pattern::Name,
            Pattern::Record { names, rest } => Pattern::Record {
                names: names
                    .iter()
                    .map(|name| name.__instantiate(depth, val))
                    .collect(),
                rest: *rest,
            },
            Pattern::Variant { tag } => Pattern::mk_variant(tag.__instantiate(depth, val)),
            Pattern::Wildcard => Pattern::Wildcard,
        }
    }

    pub fn __abstract_evar(&self, depth: usize, ev: EVar) -> Self {
        match self {
            Pattern::Name => Pattern::Name,
            Pattern::Record { names, rest } => Pattern::Record {
                names: names
                    .iter()
                    .map(|name| name.__abstract_evar(depth, ev))
                    .collect(),
                rest: *rest,
            },
            Pattern::Variant { tag } => Pattern::mk_variant(tag.__abstract_evar(depth, ev)),
            Pattern::Wildcard => Pattern::Wildcard,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Branch {
    pub pattern: Pattern,
    pub body: Expr,
}

impl Branch {
    pub fn map_expr<F: Fn(&Expr) -> Expr>(&self, f: F) -> Branch {
        Branch {
            pattern: self.pattern.map_expr(&f),
            body: f(&self.body),
        }
    }

    pub fn subst_placeholder<E, F: FnMut(&Placeholder) -> Result<Expr, E>>(
        &self,
        f: &mut F,
    ) -> Result<Self, E> {
        self.body.subst_placeholder(f).and_then(|body| {
            self.pattern
                .subst_placeholder(f)
                .map(|pattern| Branch { pattern, body })
        })
    }

    pub fn __instantiate(&self, depth: usize, val: &Expr) -> Self {
        Branch {
            pattern: self.pattern.__instantiate(depth, val),
            body: self.body.__instantiate(
                depth
                    + match &self.pattern {
                        Pattern::Name => 1,
                        Pattern::Record { names, rest } => names.len() + if *rest { 1 } else { 0 },
                        Pattern::Variant { tag: _ } => 1,
                        Pattern::Wildcard => 1,
                    },
                val,
            ),
        }
    }

    pub fn __abstract_evar(&self, depth: usize, ev: EVar) -> Self {
        Branch {
            pattern: self.pattern.__abstract_evar(depth, ev),
            body: self.body.__abstract_evar(
                depth
                    + match &self.pattern {
                        Pattern::Name => 1,
                        Pattern::Record { names, rest } => names.len() + if *rest { 1 } else { 0 },
                        Pattern::Variant { tag: _ } => 1,
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
    pub fn map_expr<F: Fn(&Expr) -> Expr>(&self, f: F) -> Self {
        match self {
            StringPart::String(s) => StringPart::String(s.clone()),
            StringPart::Expr(e) => StringPart::Expr(f(e)),
        }
    }

    pub fn subst_placeholder<E, F: FnMut(&Placeholder) -> Result<Expr, E>>(
        &self,
        f: &mut F,
    ) -> Result<Self, E> {
        match self {
            StringPart::String(s) => Ok(StringPart::String(s.clone())),
            StringPart::Expr(e) => e.subst_placeholder(f).map(StringPart::Expr),
        }
    }

    pub fn __instantiate(&self, depth: usize, val: &Expr) -> Self {
        match self {
            StringPart::String(s) => StringPart::String(s.clone()),
            StringPart::Expr(e) => StringPart::Expr(e.__instantiate(depth, val)),
        }
    }

    pub fn __abstract_evar(&self, depth: usize, ev: EVar) -> Self {
        match self {
            StringPart::String(s) => StringPart::String(s.clone()),
            StringPart::Expr(e) => StringPart::Expr(e.__abstract_evar(depth, ev)),
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
    FlushStdout,
    ReadLineStdin,
    EqInt,
    LtInt,
    Add,
    Subtract,
    Multiply,
    ShowInt,
    FoldlArray,
    EqArray,
    LtArray,
    GenerateArray,
    LengthArray,
    IndexArray,
    SliceArray,
    EqString,
    FilterString,
    EqChar,
    SplitString,
    FoldlString,
    SnocArray,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct EVar(pub usize);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Placeholder(pub usize);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Var(usize),
    EVar(EVar),
    Placeholder(Placeholder),
    Name(String),
    Module(ModuleName, String),
    Builtin(Builtin),

    App(Rc<Expr>, Rc<Expr>),
    Lam { arg: bool, body: Rc<Expr> },

    Let { value: Rc<Expr>, rest: Rc<Expr> },

    True,
    False,
    IfThenElse(Rc<Expr>, Rc<Expr>, Rc<Expr>),

    Int(u32),

    Binop(syntax::Binop, Rc<Expr>, Rc<Expr>),

    Char(char),

    String(Vec<StringPart>),

    Array(Vec<Expr>),

    Extend(Rc<Expr>, Rc<Expr>, Rc<Expr>),
    Record(Vec<(Expr, Expr)>),
    Project(Rc<Expr>, Rc<Expr>),

    Variant(Rc<Expr>),
    Embed(Rc<Expr>, Rc<Expr>),
    Case(Rc<Expr>, Vec<Branch>),
    Unit,
}

impl Expr {
    pub fn alloc_builtin(b: Builtin) -> Rc<Expr> {
        Rc::new(Expr::Builtin(b))
    }

    pub fn mk_app(a: Expr, b: Expr) -> Expr {
        match a {
            Expr::Lam { arg, body } => {
                if arg {
                    // this potentially increases the number of redexes in the term.
                    // todo: bind the arg with a let?
                    body.instantiate(&b)
                } else {
                    (*body).clone()
                }
            }
            a => Expr::App(Rc::new(a), Rc::new(b)),
        }
    }

    pub fn mk_lam(arg: bool, body: Expr) -> Expr {
        Expr::Lam {
            arg,
            body: Rc::new(body),
        }
    }

    pub fn mk_ifthenelse(x: Expr, y: Expr, z: Expr) -> Expr {
        Expr::IfThenElse(Rc::new(x), Rc::new(y), Rc::new(z))
    }

    pub fn mk_extend(ev: Expr, field: Expr, rest: Expr) -> Expr {
        Expr::Extend(Rc::new(ev), Rc::new(field), Rc::new(rest))
    }

    pub fn mk_record(fields: Vec<(Expr, Expr)>, rest: Option<Expr>) -> Expr {
        match rest {
            None => Expr::Record(fields),
            Some(rest) => fields
                .into_iter()
                .rev()
                .fold(rest, |acc, (ev, field)| Expr::mk_extend(ev, field, acc)),
        }
    }

    pub fn mk_project(expr: Expr, offset: Expr) -> Expr {
        match expr {
            Expr::Record(fields) => match offset {
                Expr::Int(ix) => match fields.iter().find_map(|(pos, val)| match pos {
                    Expr::Int(ixx) if ix == *ixx => Some(val.clone()),
                    _ => None,
                }) {
                    Some(val) => val,
                    None => Self::Project(Rc::new(Expr::Record(fields)), Rc::new(Expr::Int(ix))),
                },
                offset => Self::Project(Rc::new(Expr::Record(fields)), Rc::new(offset)),
            },
            expr => Self::Project(Rc::new(expr), Rc::new(offset)),
        }
    }

    pub fn mk_variant(tag: Expr) -> Expr {
        Expr::Variant(Rc::new(tag))
    }

    pub fn mk_embed(tag: Expr, rest: Expr) -> Expr {
        Expr::Embed(Rc::new(tag), Rc::new(rest))
    }

    pub fn mk_binop(op: syntax::Binop, a: Expr, b: Expr) -> Expr {
        if op == syntax::Binop::Add {
            #[allow(clippy::single_match)]
            match (&a, &b) {
                (Expr::Int(a), Expr::Int(b)) => {
                    return Expr::Int(*a + *b);
                }
                _ => {}
            }
        }
        Expr::Binop(op, Rc::new(a), Rc::new(b))
    }

    pub fn mk_case(expr: Expr, branches: Vec<Branch>) -> Expr {
        Expr::Case(Rc::new(expr), branches)
    }

    pub fn mk_placeholder(p: usize) -> Expr {
        Expr::Placeholder(Placeholder(p))
    }

    pub fn map_vars<F: Fn(usize) -> usize>(&self, f: F) -> Expr {
        enum Function<'a, F> {
            Code(F),
            Under(usize, &'a Function<'a, F>),
        }

        impl<'a, F: Fn(usize) -> usize> Function<'a, F> {
            fn apply(&self, x: usize) -> usize {
                match self {
                    Function::Code(f) => f(x),
                    Function::Under(n, f) => {
                        if x < *n {
                            x
                        } else {
                            n + f.apply(x - n)
                        }
                    }
                }
            }
        }

        fn go<'a, F: Fn(usize) -> usize>(expr: &Expr, f: &'a Function<'a, F>) -> Expr {
            match expr {
                Expr::Var(v) => Expr::Var(f.apply(*v)),
                Expr::Module(n, f) => Expr::Module(n.clone(), f.clone()),
                Expr::EVar(v) => Expr::EVar(*v),
                Expr::Placeholder(p) => Expr::Placeholder(*p),
                Expr::Name(n) => Expr::Name(n.clone()),
                Expr::Builtin(b) => Expr::Builtin(*b),
                Expr::App(a, b) => Expr::mk_app(go(a, f), go(b, f)),
                Expr::Lam { arg, body } => {
                    if *arg {
                        Expr::mk_lam(*arg, go(body, &Function::Under(1, f)))
                    } else {
                        Expr::mk_lam(*arg, go(body, f))
                    }
                }
                Expr::Let { value, rest } => Expr::Let {
                    value: Rc::new(go(value, f)),
                    rest: Rc::new(go(rest, &Function::Under(1, f))),
                },
                Expr::True => Expr::True,
                Expr::False => Expr::False,
                Expr::IfThenElse(a, b, c) => Expr::mk_ifthenelse(go(a, f), go(b, f), go(c, f)),
                Expr::Int(n) => Expr::Int(*n),
                Expr::Binop(op, l, r) => Expr::mk_binop(*op, go(l, f), go(r, f)),
                Expr::Char(c) => Expr::Char(*c),
                Expr::String(ss) => {
                    Expr::String(ss.iter().map(|s| s.map_expr(|e| go(e, f))).collect())
                }
                Expr::Array(es) => Expr::Array(es.iter().map(|e| go(e, f)).collect()),
                Expr::Extend(a, b, c) => Expr::mk_extend(go(a, f), go(b, f), go(c, f)),
                Expr::Record(xs) => {
                    Expr::Record(xs.iter().map(|(a, b)| (go(a, f), go(b, f))).collect())
                }
                Expr::Project(a, b) => Expr::mk_project(go(a, f), go(b, f)),
                Expr::Variant(a) => Expr::mk_variant(go(a, f)),
                Expr::Embed(a, b) => Expr::mk_embed(go(a, f), go(b, f)),
                Expr::Case(a, bs) => Expr::mk_case(
                    go(a, f),
                    bs.iter()
                        .map(|b| match &b.pattern {
                            Pattern::Name => b.map_expr(|e| go(e, &Function::Under(1, f))),
                            Pattern::Record { names, rest } => {
                                let offset = names.len() + if *rest { 1 } else { 0 };
                                b.map_expr(|e| go(e, &Function::Under(offset, f)))
                            }
                            Pattern::Variant { tag: _ } => {
                                b.map_expr(|e| go(e, &Function::Under(1, f)))
                            }
                            Pattern::Wildcard => b.map_expr(|e| go(e, f)),
                        })
                        .collect(),
                ),
                Expr::Unit => Expr::Unit,
            }
        }

        go(self, &Function::Code(f))
    }

    /// ```
    /// use ipso_core::Expr;
    ///
    /// assert_eq!(
    ///     Expr::mk_app(Expr::Var(0), Expr::mk_lam(true, Expr::mk_app(Expr::Var(0), Expr::Var(1)))).instantiate(&Expr::Int(99)),
    ///     Expr::mk_app(Expr::Int(99), Expr::mk_lam(true, Expr::mk_app(Expr::Var(0), Expr::Int(99))))
    /// )
    /// ```
    ///
    /// ```
    /// use ipso_core::Expr;
    ///
    /// assert_eq!(
    ///     Expr::mk_app(Expr::Var(0), Expr::mk_lam(true, Expr::mk_app(Expr::Var(0), Expr::Var(1)))).instantiate(&Expr::Var(0)),
    ///     Expr::mk_app(Expr::Var(0), Expr::mk_lam(true, Expr::mk_app(Expr::Var(0), Expr::Var(1))))
    /// )
    /// ```
    ///
    /// ```
    /// use ipso_core::Expr;
    ///
    /// assert_eq!(
    ///     Expr::mk_app(Expr::Var(0), Expr::mk_lam(true, Expr::mk_app(Expr::Var(0), Expr::Var(2)))).instantiate(&Expr::Int(99)),
    ///     Expr::mk_app(Expr::Int(99), Expr::mk_lam(true, Expr::mk_app(Expr::Var(0), Expr::Var(1))))
    /// )
    /// ```
    pub fn instantiate(&self, val: &Expr) -> Expr {
        self.__instantiate(0, val)
    }

    pub fn __instantiate(&self, depth: usize, val: &Expr) -> Expr {
        match self {
            Expr::Var(n) => match Ord::cmp(n, &depth) {
                cmp::Ordering::Less => Expr::Var(*n),
                cmp::Ordering::Equal => val.map_vars(|n| n + depth),
                cmp::Ordering::Greater => Expr::Var(*n - 1),
            },
            Expr::Module(n, f) => Expr::Module(n.clone(), f.clone()),
            Expr::EVar(v) => Expr::EVar(*v),
            Expr::Placeholder(v) => Expr::Placeholder(*v),
            Expr::Name(n) => Expr::Name(n.clone()),
            Expr::Builtin(b) => Expr::Builtin(*b),
            Expr::App(a, b) => {
                Expr::mk_app(a.__instantiate(depth, val), b.__instantiate(depth, val))
            }
            Expr::Lam { arg, body } => Expr::mk_lam(
                *arg,
                body.__instantiate(if *arg { depth + 1 } else { depth }, val),
            ),
            Expr::Let { value, rest } => Expr::Let {
                value: Rc::new(value.__instantiate(depth, value)),
                rest: Rc::new(rest.__instantiate(depth + 1, rest)),
            },
            Expr::True => Expr::True,
            Expr::False => Expr::False,
            Expr::IfThenElse(a, b, c) => Expr::mk_ifthenelse(
                a.__instantiate(depth, val),
                b.__instantiate(depth, val),
                c.__instantiate(depth, val),
            ),
            Expr::Int(n) => Expr::Int(*n),
            Expr::Binop(a, b, c) => {
                Expr::mk_binop(*a, b.__instantiate(depth, val), c.__instantiate(depth, val))
            }
            Expr::Char(c) => Expr::Char(*c),
            Expr::String(parts) => Expr::String(
                parts
                    .iter()
                    .map(|part| part.__instantiate(depth, val))
                    .collect(),
            ),
            Expr::Array(items) => Expr::Array(
                items
                    .iter()
                    .map(|item| item.__instantiate(depth, val))
                    .collect(),
            ),
            Expr::Extend(a, b, c) => Expr::mk_extend(
                a.__instantiate(depth, val),
                b.__instantiate(depth, val),
                c.__instantiate(depth, val),
            ),
            Expr::Record(items) => Expr::Record(
                items
                    .iter()
                    .map(|(a, b)| (a.__instantiate(depth, val), b.__instantiate(depth, val)))
                    .collect(),
            ),
            Expr::Project(a, b) => {
                Expr::mk_project(a.__instantiate(depth, val), b.__instantiate(depth, val))
            }
            Expr::Variant(a) => Expr::mk_variant(a.__instantiate(depth, val)),
            Expr::Embed(a, b) => {
                Expr::mk_embed(a.__instantiate(depth, val), b.__instantiate(depth, val))
            }
            Expr::Case(a, bs) => Expr::mk_case(
                a.__instantiate(depth, val),
                bs.iter().map(|b| b.__instantiate(depth, val)).collect(),
            ),

            Expr::Unit => Expr::Unit,
        }
    }

    pub fn subst_placeholder<E, F: FnMut(&Placeholder) -> Result<Expr, E>>(
        &self,
        f: &mut F,
    ) -> Result<Expr, E> {
        match self {
            Expr::Var(n) => Ok(Expr::Var(*n)),
            Expr::EVar(n) => Ok(Expr::EVar(*n)),
            Expr::Module(n, f) => Ok(Expr::Module(n.clone(), f.clone())),
            Expr::Placeholder(v) => f(v),
            Expr::Name(n) => Ok(Expr::Name(n.clone())),
            Expr::Builtin(b) => Ok(Expr::Builtin(*b)),
            Expr::App(a, b) => a
                .subst_placeholder(f)
                .and_then(|a| b.subst_placeholder(f).map(|b| Expr::mk_app(a, b))),
            Expr::Lam { arg, body } => body
                .subst_placeholder(f)
                .map(|body| Expr::mk_lam(*arg, body)),
            Expr::Let { value, rest } => value.subst_placeholder(f).and_then(|value| {
                rest.subst_placeholder(f).map(|rest| Expr::Let {
                    value: Rc::new(value),
                    rest: Rc::new(rest),
                })
            }),
            Expr::True => Ok(Expr::True),
            Expr::False => Ok(Expr::False),
            Expr::IfThenElse(a, b, c) => a.subst_placeholder(f).and_then(|a| {
                b.subst_placeholder(f)
                    .and_then(|b| c.subst_placeholder(f).map(|c| Expr::mk_ifthenelse(a, b, c)))
            }),
            Expr::Int(n) => Ok(Expr::Int(*n)),
            Expr::Binop(a, b, c) => b
                .subst_placeholder(f)
                .and_then(|b| c.subst_placeholder(f).map(|c| Expr::mk_binop(*a, b, c))),
            Expr::Char(c) => Ok(Expr::Char(*c)),
            Expr::String(parts) => {
                let mut new_parts = Vec::new();
                for part in parts {
                    match part.subst_placeholder(f) {
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
                    match item.subst_placeholder(f) {
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
            Expr::Extend(a, b, c) => a.subst_placeholder(f).and_then(|a| {
                b.subst_placeholder(f)
                    .and_then(|b| c.subst_placeholder(f).map(|c| Expr::mk_extend(a, b, c)))
            }),
            Expr::Record(items) => {
                let mut new_items = Vec::new();
                for (a, b) in items {
                    match a
                        .subst_placeholder(f)
                        .and_then(|a| b.subst_placeholder(f).map(|b| (a, b)))
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
                .subst_placeholder(f)
                .and_then(|a| b.subst_placeholder(f).map(|b| Expr::mk_project(a, b))),
            Expr::Variant(a) => a.subst_placeholder(f).map(Expr::mk_variant),
            Expr::Embed(a, b) => a
                .subst_placeholder(f)
                .and_then(|a| b.subst_placeholder(f).map(|b| Expr::mk_embed(a, b))),
            Expr::Case(a, bs) => a.subst_placeholder(f).and_then(|a| {
                let mut new_bs = Vec::new();
                for b in bs {
                    match b.subst_placeholder(f) {
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

    pub fn __abstract_evar(&self, depth: usize, ev: EVar) -> Expr {
        match self {
            Expr::Var(n) => Expr::Var(if *n >= depth { *n + 1 } else { *n }),
            Expr::EVar(current_ev) => {
                if ev == *current_ev {
                    Expr::Var(depth)
                } else {
                    Expr::EVar(*current_ev)
                }
            }
            Expr::Name(n) => Expr::Name(n.clone()),
            Expr::Module(n, f) => Expr::Module(n.clone(), f.clone()),
            Expr::Placeholder(n) => Expr::Placeholder(*n),
            Expr::Builtin(b) => Expr::Builtin(*b),
            Expr::App(a, b) => {
                Expr::mk_app(a.__abstract_evar(depth, ev), b.__abstract_evar(depth, ev))
            }
            Expr::Lam { arg, body } => Expr::mk_lam(
                *arg,
                body.__abstract_evar(if *arg { depth + 1 } else { depth }, ev),
            ),
            Expr::Let { value, rest } => Expr::Let {
                value: Rc::new(value.__abstract_evar(depth, ev)),
                rest: Rc::new(rest.__abstract_evar(depth + 1, ev)),
            },
            Expr::True => Expr::True,
            Expr::False => Expr::False,
            Expr::IfThenElse(a, b, c) => Expr::mk_ifthenelse(
                a.__abstract_evar(depth, ev),
                b.__abstract_evar(depth, ev),
                c.__abstract_evar(depth, ev),
            ),
            Expr::Int(n) => Expr::Int(*n),
            Expr::Binop(a, b, c) => Expr::mk_binop(
                *a,
                b.__abstract_evar(depth, ev),
                c.__abstract_evar(depth, ev),
            ),
            Expr::Char(c) => Expr::Char(*c),
            Expr::String(parts) => Expr::String(
                parts
                    .iter()
                    .map(|part| part.__abstract_evar(depth, ev))
                    .collect(),
            ),
            Expr::Array(items) => Expr::Array(
                items
                    .iter()
                    .map(|item| item.__abstract_evar(depth, ev))
                    .collect(),
            ),
            Expr::Extend(a, b, c) => Expr::mk_extend(
                a.__abstract_evar(depth, ev),
                b.__abstract_evar(depth, ev),
                c.__abstract_evar(depth, ev),
            ),
            Expr::Record(items) => Expr::Record(
                items
                    .iter()
                    .map(|(a, b)| (a.__abstract_evar(depth, ev), b.__abstract_evar(depth, ev)))
                    .collect(),
            ),
            Expr::Project(a, b) => {
                Expr::mk_project(a.__abstract_evar(depth, ev), b.__abstract_evar(depth, ev))
            }
            Expr::Variant(a) => Expr::mk_variant(a.__abstract_evar(depth, ev)),
            Expr::Embed(a, b) => {
                Expr::mk_embed(a.__abstract_evar(depth, ev), b.__abstract_evar(depth, ev))
            }
            Expr::Case(a, bs) => Expr::mk_case(
                a.__abstract_evar(depth, ev),
                bs.iter().map(|b| b.__abstract_evar(depth, ev)).collect(),
            ),
            Expr::Unit => Expr::Unit,
        }
    }

    /// ```
    /// use ipso_core::{EVar, Expr};
    ///
    /// assert_eq!(
    ///     Expr::mk_lam(true, Expr::mk_app(Expr::Var(0), Expr::EVar(EVar(0)))).abstract_evar(EVar(0)),
    ///     Expr::mk_lam(true, Expr::mk_lam(true, Expr::mk_app(Expr::Var(0), Expr::Var(1))))
    /// )
    /// ```
    ///
    /// ```
    /// use ipso_core::{EVar, Expr};
    ///
    /// assert_eq!(
    ///     Expr::mk_lam(true, Expr::mk_app(Expr::Var(1), Expr::EVar(EVar(0)))).abstract_evar(EVar(0)),
    ///     Expr::mk_lam(true, Expr::mk_lam(true, Expr::mk_app(Expr::Var(2), Expr::Var(1))))
    /// )
    /// ```
    pub fn abstract_evar(&self, ev: EVar) -> Expr {
        Expr::mk_lam(true, self.__abstract_evar(0, ev))
    }

    /// ```
    /// use ipso_core::{EVar, Expr};
    ///
    /// let expr = Expr::mk_app(Expr::mk_app(Expr::EVar(EVar(0)), Expr::EVar(EVar(1))), Expr::EVar(EVar(2)));
    /// assert_eq!(expr.iter_evars().collect::<Vec<&EVar>>(), vec![&EVar(0), &EVar(1), &EVar(2)]);
    /// ```
    ///
    /// ```
    /// use ipso_core::{EVar, Expr};
    ///
    /// let expr = Expr::mk_app(Expr::EVar(EVar(0)), Expr::mk_app(Expr::EVar(EVar(1)), Expr::EVar(EVar(2))));
    /// assert_eq!(expr.iter_evars().collect::<Vec<&EVar>>(), vec![&EVar(0), &EVar(1), &EVar(2)]);
    /// ```
    ///
    /// ```
    /// use ipso_core::{Branch, EVar, Expr, Pattern};
    /// let expr = Expr::mk_lam (
    ///     true,
    ///     Expr::mk_case(
    ///         Expr::Var(0),
    ///         vec![
    ///             Branch {
    ///                 pattern: Pattern::Record {
    ///                     names: vec![
    ///                         Expr::EVar(EVar(0))
    ///                     ],
    ///                     rest: true,
    ///                 },
    ///                 body: Expr::Var(1)
    ///             },
    ///         ],
    ///     ),
    /// );
    /// assert_eq!(expr.iter_evars().collect::<Vec<&EVar>>(), vec![&EVar(0)]);
    /// ```
    pub fn iter_evars(&self) -> IterEVars {
        IterEVars { next: vec![self] }
    }
}

pub struct IterEVars<'a> {
    next: Vec<&'a Expr>,
}

impl<'a> Iterator for IterEVars<'a> {
    type Item = &'a EVar;

    fn next(&mut self) -> Option<Self::Item> {
        fn step_expr<'a>(expr: &'a Expr) -> Step<'a, Expr, &'a EVar> {
            match expr {
                Expr::Var(_) => Step::Skip,
                Expr::EVar(a) => Step::Yield(a),
                Expr::Placeholder(_) => Step::Skip,
                Expr::Name(_) => Step::Skip,
                Expr::Module(_, _) => Step::Skip,
                Expr::Builtin(_) => Step::Skip,
                Expr::App(a, b) => Step::Continue2(a, b),
                Expr::Lam { arg: _, body } => Step::Continue1(body),
                Expr::Let { value, rest, .. } => Step::Continue2(value, rest),
                Expr::True => Step::Skip,
                Expr::False => Step::Skip,
                Expr::IfThenElse(a, b, c) => Step::Continue3(a, b, c),
                Expr::Int(_) => Step::Skip,
                Expr::Binop(_, a, b) => Step::Continue2(a, b),
                Expr::Char(_) => Step::Skip,
                Expr::String(a) => Step::Continue(
                    a.iter()
                        .flat_map(|x| match x {
                            StringPart::String(_) => Vec::new().into_iter(),
                            StringPart::Expr(e) => vec![e].into_iter(),
                        })
                        .collect(),
                ),
                Expr::Array(xs) => Step::Continue(xs.iter().collect()),
                Expr::Extend(a, b, c) => Step::Continue3(a, b, c),
                Expr::Record(xs) => Step::Continue(
                    xs.iter()
                        .flat_map(|(a, b)| vec![a, b].into_iter())
                        .collect(),
                ),
                Expr::Project(a, b) => Step::Continue2(a, b),
                Expr::Variant(a) => Step::Continue1(a),
                Expr::Embed(a, b) => Step::Continue2(a, b),
                Expr::Case(a, b) => Step::Continue({
                    let mut xs: Vec<&'a Expr> = vec![a];
                    xs.extend(b.iter().flat_map(|b| {
                        let mut vals: Vec<&'a Expr> = match &b.pattern {
                            Pattern::Name => Vec::new(),
                            Pattern::Record { names, .. } => names.iter().collect(),
                            Pattern::Variant { tag } => vec![tag],
                            Pattern::Wildcard => Vec::new(),
                        };
                        vals.push(&b.body);
                        vals.into_iter()
                    }));
                    xs
                }),
                Expr::Unit => Step::Skip,
            }
        }

        loop {
            match self.next.pop() {
                None => {
                    return None;
                }
                Some(expr) => match step_expr(expr) {
                    Step::Yield(x) => {
                        return Some(x);
                    }
                    Step::Skip => {
                        continue;
                    }
                    Step::Continue1(item) => {
                        self.next.push(item);
                        continue;
                    }
                    Step::Continue2(item1, item2) => {
                        self.next.push(item2);
                        self.next.push(item1);
                        continue;
                    }
                    Step::Continue3(item1, item2, item3) => {
                        self.next.push(item3);
                        self.next.push(item2);
                        self.next.push(item1);
                        continue;
                    }
                    Step::Continue(xs) => {
                        self.next.extend(xs.into_iter().rev());
                        continue;
                    }
                },
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeSig {
    pub ty_vars: Vec<(Rc<str>, syntax::Kind)>,
    pub body: syntax::Type<usize>,
}

impl TypeSig {
    pub fn instantiate_many(&self, tys: &[syntax::Type<usize>]) -> TypeSig {
        let mut ty_vars = self.ty_vars.clone();
        for _ty in tys.iter().rev() {
            match ty_vars.pop() {
                None => panic!("instantiating too many variables"),
                Some((_ty_var_name, _ty_var_kind)) => {}
            }
        }
        TypeSig {
            ty_vars,
            body: self.body.instantiate_many(tys),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ClassMember {
    pub name: String,
    pub sig: TypeSig,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InstanceMember {
    pub name: String,
    pub body: Expr,
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
        body: Rc<Expr>,
    },
    TypeAlias {
        name: String,
        args: Vec<syntax::Kind>,
        body: syntax::Type<usize>,
    },
    Class(ClassDeclaration),
    Instance {
        ty_vars: Vec<(Rc<str>, syntax::Kind)>,
        superclass_constructors: Vec<Expr>,
        assumes: Vec<syntax::Type<usize>>,
        head: syntax::Type<usize>,
        members: Vec<InstanceMember>,
    },
}

impl Declaration {
    pub fn get_bindings(&self) -> HashMap<String, Rc<Expr>> {
        match self {
            Declaration::BuiltinType { .. } => HashMap::new(),
            Declaration::Definition { name, sig: _, body } => {
                let mut map = HashMap::new();
                map.insert(name.clone(), body.clone());
                map
            }
            Declaration::TypeAlias { .. } => HashMap::new(),
            Declaration::Class(decl) => decl
                .get_bindings()
                .into_iter()
                .map(|(a, b)| (a, b.1))
                .collect(),
            Declaration::Instance { .. } => HashMap::new(),
        }
    }

    pub fn get_signatures(&self) -> HashMap<String, TypeSig> {
        match self {
            Declaration::BuiltinType { .. } => HashMap::new(),
            Declaration::Definition { name, sig, body: _ } => {
                let mut map = HashMap::new();
                map.insert(name.clone(), sig.clone());
                map
            }
            Declaration::TypeAlias { .. } => HashMap::new(),
            Declaration::Class(decl) => decl
                .get_bindings()
                .into_iter()
                .map(|(a, b)| (a, b.0))
                .collect(),
            Declaration::Instance { .. } => HashMap::new(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ClassDeclaration {
    pub supers: Vec<syntax::Type<usize>>,
    pub name: Rc<str>,
    pub args: Vec<(Rc<str>, syntax::Kind)>,
    pub members: Vec<ClassMember>,
}

impl ClassDeclaration {
    pub fn get_bindings(&self) -> HashMap<String, (TypeSig, Rc<Expr>)> {
        let supers_len = self.supers.len();

        self.members
            .iter()
            .enumerate()
            .map(|(ix, member)| {
                // we need each argument in the applied type to account for the extra variables
                // bound by the member's signature
                //
                // e.g.
                //
                // class X a where
                //   x : a -> b -> ()
                //
                // the variable 'a' should recieve the de bruijn index '1', because 'b' is the innermost
                // bound variable
                //
                // this will panic if we allow ambiguous class members
                let adjustment = if !member.sig.ty_vars.is_empty() {
                    member.sig.ty_vars.len() - self.args.len()
                } else {
                    0
                };
                let applied_type = (adjustment..adjustment + self.args.len())
                    .into_iter()
                    .fold(Type::Name(self.name.clone()), |acc, el| {
                        Type::mk_app(acc, Type::Var(el))
                    });
                let sig = {
                    let mut body = member.sig.body.clone();
                    body = syntax::Type::mk_fatarrow(applied_type, body);

                    TypeSig {
                        ty_vars: member.sig.ty_vars.clone(),
                        body,
                    }
                };
                let body: Rc<Expr> = Rc::new(Expr::mk_lam(
                    true,
                    Expr::mk_project(Expr::Var(0), Expr::Int(supers_len as u32 + ix as u32)),
                ));

                (member.name.clone(), (sig, body))
            })
            .collect()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ModuleUsage {
    /// A module was given a particular name during importing
    Named(String),
    /// Specific names were imported from a module
    Items(Vec<String>),
    /// The entire contents of a module were imported
    All,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum ModulePath {
    Module {
        module_name: ModuleName,
        path: PathBuf,
    },
    File {
        path: PathBuf,
    },
}

impl ModulePath {
    pub fn from_module(dir: &Path, module_name: &ModuleName) -> Self {
        let mut path = module_name
            .iter()
            .fold(PathBuf::from(dir), |acc, el| acc.join(el));
        path.set_extension("ipso");
        ModulePath::Module {
            module_name: module_name.clone(),
            path,
        }
    }

    pub fn from_file(file: &Path) -> Self {
        ModulePath::File {
            path: PathBuf::from(file),
        }
    }

    pub fn as_path(&self) -> &Path {
        let path = match self {
            ModulePath::Module { path, .. } => path,
            ModulePath::File { path, .. } => path,
        };
        path.as_path()
    }

    pub fn to_str(&self) -> &str {
        self.as_path().to_str().unwrap()
    }

    pub fn get_module_name(&self) -> Option<&ModuleName> {
        match self {
            ModulePath::Module { module_name, .. } => Option::Some(module_name),
            ModulePath::File { .. } => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    /// Describes how each imported file is referenced by this module.
    pub module_mapping: HashMap<ModulePath, ModuleUsage>,
    pub decls: Vec<Declaration>,
}

impl Module {
    pub fn get_bindings(&self) -> HashMap<String, Rc<Expr>> {
        let bindings: HashMap<String, Rc<Expr>> = HashMap::new();
        self.decls.iter().fold(bindings, |mut acc, decl| {
            acc.extend(decl.get_bindings().into_iter());
            acc
        })
    }

    pub fn get_signatures(&self) -> HashMap<String, TypeSig> {
        let signatures: HashMap<String, TypeSig> = HashMap::new();
        self.decls.iter().fold(signatures, |mut acc, decl| {
            acc.extend(decl.get_signatures().into_iter());
            acc
        })
    }
}
