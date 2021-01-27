use crate::syntax;

#[cfg(test)]
mod test;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pattern {
    Name,
    Record { names: Vec<Expr>, rest: bool },
    Variant { tag: Box<Expr> },
    Wildcard,
}

impl Pattern {
    pub fn mk_variant(tag: Expr) -> Pattern {
        Pattern::Variant { tag: Box::new(tag) }
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
            Pattern::Variant { tag } => {
                tag.subst_placeholder(f).map(|tag| Pattern::mk_variant(tag))
            }
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
    pub fn subst_placeholder<E, F: FnMut(&Placeholder) -> Result<Expr, E>>(
        &self,
        f: &mut F,
    ) -> Result<Self, E> {
        match self {
            StringPart::String(s) => Ok(StringPart::String(s.clone())),
            StringPart::Expr(e) => e.subst_placeholder(f).map(|e| StringPart::Expr(e)),
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
    ReadLineStdin,
    EqInt,
    LtInt,
    FoldlArray,
    EqArray,
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

    Variant(Box<Expr>),
    Embed(Box<Expr>, Box<Expr>),
    Case(Box<Expr>, Vec<Branch>),
    Unit,
}

impl Expr {
    pub fn mk_app(a: Expr, b: Expr) -> Expr {
        match a {
            Expr::Lam { arg, body } => {
                if arg {
                    // this potentially increases the number of redexes in the term.
                    // todo: bind the arg with a let?
                    body.instantiate(&b)
                } else {
                    *body
                }
            }
            a => Expr::App(Box::new(a), Box::new(b)),
        }
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

    pub fn mk_variant(tag: Expr) -> Expr {
        Expr::Variant(Box::new(tag))
    }

    pub fn mk_embed(tag: Expr, rest: Expr) -> Expr {
        Expr::Embed(Box::new(tag), Box::new(rest))
    }

    pub fn mk_binop(op: syntax::Binop, a: Expr, b: Expr) -> Expr {
        match op {
            syntax::Binop::Add => match (&a, &b) {
                (Expr::Int(a), Expr::Int(b)) => {
                    return Expr::Int(*a + *b);
                }
                _ => {}
            },
            _ => {}
        }
        Expr::Binop(op, Box::new(a), Box::new(b))
    }

    pub fn mk_case(expr: Expr, branches: Vec<Branch>) -> Expr {
        Expr::Case(Box::new(expr), branches)
    }

    pub fn mk_evar(ev: usize) -> Expr {
        Expr::EVar(EVar(ev))
    }

    pub fn mk_placeholder(p: usize) -> Expr {
        Expr::Placeholder(Placeholder(p))
    }

    pub fn instantiate(&self, val: &Expr) -> Expr {
        self.__instantiate(0, val)
    }

    pub fn __instantiate(&self, depth: usize, val: &Expr) -> Expr {
        match self {
            Expr::Var(n) => {
                if *n == depth {
                    val.clone()
                } else {
                    Expr::Var(*n)
                }
            }
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
            Expr::Placeholder(v) => f(v),
            Expr::Name(n) => Ok(Expr::Name(n.clone())),
            Expr::Builtin(b) => Ok(Expr::Builtin(*b)),
            Expr::App(a, b) => a
                .subst_placeholder(f)
                .and_then(|a| b.subst_placeholder(f).map(|b| Expr::mk_app(a, b))),
            Expr::Lam { arg, body } => body
                .subst_placeholder(f)
                .map(|body| Expr::mk_lam(*arg, body)),
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
            Expr::Variant(a) => a.subst_placeholder(f).map(|a| Expr::mk_variant(a)),
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
            Expr::Placeholder(n) => Expr::Placeholder(*n),
            Expr::Builtin(b) => Expr::Builtin(*b),
            Expr::App(a, b) => {
                Expr::mk_app(a.__abstract_evar(depth, ev), b.__abstract_evar(depth, ev))
            }
            Expr::Lam { arg, body } => Expr::mk_lam(
                *arg,
                body.__abstract_evar(if *arg { depth + 1 } else { depth }, ev),
            ),
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

    pub fn abstract_evar(&self, ev: EVar) -> Expr {
        Expr::mk_lam(true, self.__abstract_evar(0, ev))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeSig {
    pub ty_vars: Vec<(String, syntax::Kind)>,
    pub body: syntax::Type<usize>,
}

impl TypeSig {
    pub fn instantiate_many(&self, tys: &Vec<syntax::Type<usize>>) -> TypeSig {
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
        names: syntax::Names,
    },
    Class(ClassDeclaration),
    Instance {
        ty_vars: Vec<(String, syntax::Kind)>,
        superclass_constructors: Vec<Expr>,
        assumes: Vec<syntax::Type<usize>>,
        head: syntax::Type<usize>,
        members: Vec<InstanceMember>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ClassDeclaration {
    pub supers: Vec<syntax::Type<usize>>,
    pub name: String,
    pub args: Vec<(String, syntax::Kind)>,
    pub members: Vec<ClassMember>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub decls: Vec<Declaration>,
}
