use std::{collections::HashMap, fmt::Display, hash::Hash, rc::Rc};

use crate::iter::Step;
use lazy_static::lazy_static;

#[cfg(test)]
mod test;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Spanned<A> {
    pub pos: usize,
    pub item: A,
}

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
    Type,
    Class,
    Instance,
    Where,
}

impl Keyword {
    pub fn matches(&self, actual: &str) -> bool {
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
            Keyword::Type => "type",
            Keyword::Class => "class",
            Keyword::Instance => "instance",
            Keyword::Where => "where",
        }
    }
}

lazy_static! {
    static ref KEYWORDS: Vec<&'static str> = vec![
        "case", "of", "if", "then", "else", "true", "false", "import", "as", "from", "where",
        "class", "instance"
    ];
}

pub fn is_keyword(val: &str) -> bool {
    KEYWORDS.contains(&val)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StringPart {
    String(String),
    Expr(Spanned<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pattern {
    Name(Spanned<String>),
    Record {
        names: Vec<Spanned<String>>,
        rest: Option<Spanned<String>>,
    },
    Variant {
        name: String,
        arg: Spanned<String>,
    },
    Wildcard,
}

pub struct IterNames<'a> {
    items: Vec<&'a Spanned<String>>,
    pattern: Option<&'a Pattern>,
}

impl<'a> Iterator for IterNames<'a> {
    type Item = &'a Spanned<String>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.pattern {
            None => match self.items.pop() {
                None => None,
                Some(item) => Some(item),
            },
            Some(pattern) => {
                self.pattern = None;
                match pattern {
                    Pattern::Name(n) => Some(n),
                    Pattern::Record { names, rest } => {
                        match rest {
                            Some(n) => {
                                self.items.push(n);
                            }
                            None => {}
                        }
                        self.items.extend(names.iter().rev());
                        self.next()
                    }
                    Pattern::Variant { name: _, arg } => Some(arg),
                    Pattern::Wildcard => None,
                }
            }
        }
    }
}

impl Pattern {
    pub fn iter_names(&self) -> IterNames {
        IterNames {
            items: Vec::new(),
            pattern: Some(self),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Branch {
    pub pattern: Spanned<Pattern>,
    pub body: Spanned<Expr>,
}
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ModuleName(pub Vec<String>);

impl ModuleName {
    pub fn iter(&self) -> std::slice::Iter<String> {
        self.0.iter()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Var(String),
    Module {
        name: ModuleName,
        item: String,
    },

    App(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Lam {
        args: Vec<Pattern>,
        body: Box<Spanned<Expr>>,
    },

    True,
    False,
    IfThenElse(Box<Spanned<Expr>>, Box<Spanned<Expr>>, Box<Spanned<Expr>>),

    Int(u32),

    Binop(Binop, Box<Spanned<Expr>>, Box<Spanned<Expr>>),

    Char(char),

    String(Vec<StringPart>),

    Array(Vec<Spanned<Expr>>),

    Record {
        fields: Vec<(String, Spanned<Expr>)>,
        rest: Option<Box<Spanned<Expr>>>,
    },
    Project(Box<Spanned<Expr>>, String),

    Variant(String),
    Embed(String, Box<Spanned<Expr>>),
    Case(Box<Spanned<Expr>>, Vec<Branch>),

    Unit,
}

impl Expr {
    pub fn mk_project(val: Spanned<Expr>, field: String) -> Expr {
        Expr::Project(Box::new(val), field)
    }

    pub fn mk_ifthenelse(cond: Spanned<Expr>, then: Spanned<Expr>, else_: Spanned<Expr>) -> Expr {
        Expr::IfThenElse(Box::new(cond), Box::new(then), Box::new(else_))
    }

    pub fn mk_var(v: &str) -> Expr {
        Expr::Var(String::from(v))
    }

    pub fn mk_lam(args: Vec<Pattern>, body: Spanned<Expr>) -> Expr {
        Expr::Lam {
            args,
            body: Box::new(body),
        }
    }

    pub fn mk_case(cond: Spanned<Expr>, branches: Vec<Branch>) -> Expr {
        Expr::Case(Box::new(cond), branches)
    }

    pub fn mk_app(a: Spanned<Expr>, b: Spanned<Expr>) -> Spanned<Expr> {
        Spanned {
            pos: a.pos,
            item: Expr::App(Box::new(a), Box::new(b)),
        }
    }

    pub fn mk_record(fields: Vec<(String, Spanned<Expr>)>, rest: Option<Spanned<Expr>>) -> Expr {
        Expr::Record {
            fields,
            rest: rest.map(Box::new),
        }
    }

    pub fn mk_embed(ctor: String, rest: Spanned<Expr>) -> Expr {
        Expr::Embed(ctor, Box::new(rest))
    }

    pub fn unwrap_projects(&self) -> (&Expr, Vec<&String>) {
        fn go<'a>(expr: &'a Expr, fields: &mut Vec<&'a String>) -> &'a Expr {
            match expr {
                Expr::Project(value, field) => {
                    let expr = go(&(*value).item, fields);
                    fields.push(field);
                    expr
                }
                _ => expr,
            }
        }
        let mut fields = Vec::new();
        let expr = go(self, &mut fields);
        (expr, fields)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type<A> {
    Name(String),
    Var(A),
    Bool,
    Int,
    Char,
    String,
    Bytes,
    Arrow,
    FatArrow,
    Constraints(Vec<Type<A>>),
    Array,
    Record,
    Variant,
    IO,
    App(Rc<Type<A>>, Rc<Type<A>>),
    RowNil,
    RowCons(String, Rc<Type<A>>, Rc<Type<A>>),
    HasField(String, Rc<Type<A>>),
    Unit,
    Meta(usize),
}

pub struct TypeIterMetas<'a, A> {
    items: Vec<&'a Type<A>>,
}

impl<'a, A> Iterator for TypeIterMetas<'a, A> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        fn step_kind<A>(ty: &Type<A>) -> Step<Type<A>, usize> {
            match ty {
                Type::Name(_) => Step::Skip,
                Type::Var(_) => Step::Skip,
                Type::Bool => Step::Skip,
                Type::Int => Step::Skip,
                Type::Char => Step::Skip,
                Type::String => Step::Skip,
                Type::Bytes => Step::Skip,
                Type::Arrow => Step::Skip,
                Type::FatArrow => Step::Skip,
                Type::Constraints(cs) => Step::Continue(cs.iter().collect()),
                Type::Array => Step::Skip,
                Type::Record => Step::Skip,
                Type::Variant => Step::Skip,
                Type::IO => Step::Skip,
                Type::App(a, b) => Step::Continue(vec![a, b]),
                Type::RowNil => Step::Skip,
                Type::RowCons(_, a, b) => Step::Continue(vec![a, b]),
                Type::HasField(_, a) => Step::Continue(vec![a]),
                Type::Unit => Step::Skip,
                Type::Meta(n) => Step::Yield(*n),
            }
        }

        let mut res = None;
        loop {
            match self.items.pop() {
                None => {
                    break;
                }
                Some(kind) => match step_kind(kind) {
                    Step::Yield(n) => {
                        res = Some(n);
                        break;
                    }
                    Step::Skip => {
                        continue;
                    }
                    Step::Continue(items) => {
                        self.items.extend(items.iter().rev());
                        continue;
                    }
                },
            }
        }
        res
    }
}

#[derive(Debug)]
pub struct IterVars<'a, A> {
    items: Vec<&'a Type<A>>,
}

impl<'a, A> Iterator for IterVars<'a, A> {
    type Item = &'a A;

    fn next(&mut self) -> Option<Self::Item> {
        fn step_type<A>(ty: &Type<A>) -> Step<Type<A>, &A> {
            match ty {
                Type::Name(_) => Step::Skip,
                Type::Var(n) => Step::Yield(n),
                Type::Bool => Step::Skip,
                Type::Int => Step::Skip,
                Type::Char => Step::Skip,
                Type::String => Step::Skip,
                Type::Bytes => Step::Skip,
                Type::Arrow => Step::Skip,
                Type::FatArrow => Step::Skip,
                Type::Constraints(cs) => Step::Continue(cs.iter().collect()),
                Type::Array => Step::Skip,
                Type::Record => Step::Skip,
                Type::Variant => Step::Skip,
                Type::IO => Step::Skip,
                Type::App(a, b) => Step::Continue(vec![&*a, &*b]),
                Type::RowNil => Step::Skip,
                Type::RowCons(_, a, b) => Step::Continue(vec![&*a, &*b]),
                Type::HasField(_, a) => Step::Continue(vec![&*a]),
                Type::Unit => Step::Skip,
                Type::Meta(_) => Step::Skip,
            }
        }

        let result;
        loop {
            match self.items.pop() {
                None => {
                    result = None;
                    break;
                }
                Some(current) => match step_type(current) {
                    Step::Skip => {
                        continue;
                    }
                    Step::Continue(tys) => {
                        self.items.extend(tys.iter().rev());
                        continue;
                    }
                    Step::Yield(n) => {
                        result = Some(n);
                        break;
                    }
                },
            }
        }
        result
    }
}

impl Type<usize> {
    pub fn subst_metas<F: Fn(usize) -> Type<usize>>(&self, f: &F) -> Type<usize> {
        match self {
            Type::Name(n) => Type::Name(n.clone()),
            Type::Var(n) => Type::Var(*n),
            Type::Bool => Type::Bool,
            Type::Int => Type::Int,
            Type::Char => Type::Char,
            Type::String => Type::String,
            Type::Bytes => Type::Bytes,
            Type::Arrow => Type::Arrow,
            Type::FatArrow => Type::FatArrow,
            Type::Constraints(cs) => {
                Type::Constraints(cs.iter().map(|c| c.subst_metas(f)).collect())
            }
            Type::Array => Type::Array,
            Type::Record => Type::Record,
            Type::Variant => Type::Variant,
            Type::IO => Type::IO,
            Type::App(a, b) => Type::mk_app(a.subst_metas(f), b.subst_metas(f)),
            Type::RowNil => Type::RowNil,
            Type::RowCons(field, ty, rest) => {
                Type::mk_rowcons(field.clone(), ty.subst_metas(f), rest.subst_metas(f))
            }
            Type::HasField(field, rest) => Type::mk_hasfield(field.clone(), rest.subst_metas(f)),
            Type::Unit => Type::Unit,
            Type::Meta(n) => f(*n),
        }
    }

    pub fn instantiate_many(&self, tys: &[Type<usize>]) -> Self {
        match self {
            Type::Name(n) => Type::Name(n.clone()),
            Type::Var(n) => {
                let tys_len = tys.len();
                if *n < tys_len {
                    tys[tys_len - 1 - n].clone()
                } else {
                    Type::Var(*n - tys_len)
                }
            }
            Type::Bool => Type::Bool,
            Type::Int => Type::Int,
            Type::Char => Type::Char,
            Type::String => Type::String,
            Type::Bytes => Type::Bytes,
            Type::Arrow => Type::Arrow,
            Type::FatArrow => Type::FatArrow,
            Type::Constraints(cs) => {
                Type::Constraints(cs.iter().map(|c| c.instantiate_many(tys)).collect())
            }
            Type::Array => Type::Array,
            Type::Record => Type::Record,
            Type::Variant => Type::Variant,
            Type::IO => Type::Variant,
            Type::App(a, b) => Type::mk_app(a.instantiate_many(tys), b.instantiate_many(tys)),
            Type::RowNil => Type::RowNil,
            Type::RowCons(a, b, c) => {
                Type::mk_rowcons(a.clone(), b.instantiate_many(tys), c.instantiate_many(tys))
            }
            Type::HasField(a, b) => Type::mk_hasfield(a.clone(), b.instantiate_many(tys)),
            Type::Unit => Type::Unit,
            Type::Meta(n) => Type::Meta(*n),
        }
    }
}

impl<A> Type<A> {
    pub fn flatten_constraints(&self) -> Vec<&Self> {
        fn go<'a, A>(constraints: &mut Vec<&'a Type<A>>, ty: &'a Type<A>) {
            match ty {
                Type::Constraints(cs) => {
                    for c in cs {
                        go(constraints, c);
                    }
                }
                _ => constraints.push(ty),
            }
        }
        let mut constraints = Vec::new();
        go(&mut constraints, self);
        constraints
    }

    pub fn unwrap_name(&self) -> Option<&String> {
        match self {
            Type::Name(n) => Some(n),
            _ => None,
        }
    }

    /// ```
    /// use ipso::syntax::Type;
    /// assert_eq!(
    ///     Type::mk_app(Type::mk_app(Type::mk_app(Type::Var(0), Type::Var(1)), Type::Var(2)), Type::Var(3)).unwrap_app(),
    ///     (&Type::Var(0), vec![&Type::Var(1), &Type::Var(2), &Type::Var(3)])
    /// )
    /// ```
    pub fn unwrap_app<'a>(&'a self) -> (&'a Type<A>, Vec<&'a Type<A>>) {
        let mut target = self;
        let mut args: Vec<&'a Type<A>> = Vec::new();
        while let Type::App(a, b) = target {
            args.push(b);
            target = a;
        }
        args.reverse();
        (target, args)
    }

    /// Abstract over the variables in the type. The last distinct variable encountered
    /// is recieves the index '0', and the first variable encountered recieves the index
    /// `positions.len() + <number of distinct variables in type>`.
    ///
    /// This gives the type De Bruijn indices such that it can be wrapped in `forall`s
    /// with the earlier variables bound first.
    ///
    /// i.e. `a -> f a` to `forall a f. a -> f a`
    ///
    /// ```
    /// use ipso::syntax::Type;
    /// let input = Type::mk_arrow(Type::Var(String::from("a")), Type::mk_app(Type::Var(String::from("f")), Type::Var(String::from("a"))));
    /// assert_eq!(
    ///     input.abstract_vars(&Vec::new()),
    ///     (
    ///         Type::mk_arrow(Type::Var(1), Type::mk_app(Type::Var(0), Type::Var(1))),
    ///         vec![String::from("a"), String::from("f")]
    ///     )
    /// )
    /// ```
    ///
    /// The `seen` vector can be used to influence the order of the binders.
    ///
    /// ```
    /// use ipso::syntax::Type;
    /// let input = Type::mk_arrow(
    ///     Type::Var(String::from("a")),
    ///     Type::mk_app(Type::Var(String::from("f")), Type::Var(String::from("a")))
    /// );
    /// assert_eq!(
    ///     input.abstract_vars(&vec![String::from("f"), String::from("a")]),
    ///     (
    ///         Type::mk_arrow(Type::Var(0), Type::mk_app(Type::Var(1), Type::Var(0))),
    ///         vec![String::from("f"), String::from("a")]
    ///     )
    /// )
    /// ```
    ///
    /// ```
    /// use ipso::syntax::Type;
    /// let input = Type::mk_arrow(
    ///     Type::mk_app(Type::Var(String::from("f")), Type::Var(String::from("a"))),
    ///     Type::mk_app(Type::Var(String::from("f")), Type::Var(String::from("b")))
    /// );
    /// assert_eq!(
    ///     input.abstract_vars(&vec![String::from("a"), String::from("b")]),
    ///     (
    ///         Type::mk_arrow(
    ///             Type::mk_app(Type::Var(0), Type::Var(2)),
    ///             Type::mk_app(Type::Var(0), Type::Var(1))
    ///     ),
    ///         vec![String::from("a"), String::from("b"), String::from("f")]
    ///     )
    /// )
    /// ```
    pub fn abstract_vars(&self, seen: &[A]) -> (Type<usize>, Vec<A>)
    where
        A: Eq + Hash + Clone,
    {
        let mut seen: HashMap<&A, usize> = seen.iter().enumerate().map(|(a, b)| (b, a)).collect();
        for var in self.iter_vars() {
            match seen.get(var) {
                None => {
                    seen.insert(var, seen.len());
                }
                Some(_) => {}
            }
        }

        let seen_len = seen.len();
        let all_vars: Vec<A> = {
            let mut all_vars: Vec<(A, usize)> =
                seen.iter().map(|(a, b)| ((*a).clone(), *b)).collect();
            all_vars.sort_by_key(|(_, a)| *a);
            all_vars.into_iter().map(|(a, _)| a).collect()
        };
        (
            self.map(&mut |var| seen_len - 1 - seen.get(var).unwrap()),
            all_vars,
        )
    }

    pub fn map<B, F: FnMut(&A) -> B>(&self, f: &mut F) -> Type<B> {
        match self {
            Type::Name(n) => Type::Name(n.clone()),
            Type::Var(x) => Type::Var(f(x)),
            Type::Bool => Type::Bool,
            Type::Int => Type::Int,
            Type::Char => Type::Char,
            Type::String => Type::String,
            Type::Bytes => Type::Bytes,
            Type::Arrow => Type::Arrow,
            Type::FatArrow => Type::FatArrow,
            Type::Constraints(cs) => Type::Constraints(cs.iter().map(|c| c.map(f)).collect()),
            Type::Array => Type::Array,
            Type::Record => Type::Record,
            Type::Variant => Type::Variant,
            Type::IO => Type::IO,
            Type::App(a, b) => Type::mk_app(a.map(f), b.map(f)),
            Type::RowNil => Type::RowNil,
            Type::RowCons(field, ty, rest) => {
                Type::mk_rowcons(field.clone(), ty.map(f), rest.map(f))
            }
            Type::HasField(field, rest) => Type::mk_hasfield(field.clone(), rest.map(f)),
            Type::Unit => Type::Unit,
            Type::Meta(n) => Type::Meta(*n),
        }
    }

    pub fn subst<B, F: Fn(&A) -> Type<B>>(&self, f: &F) -> Type<B> {
        match self {
            Type::Name(n) => Type::Name(n.clone()),
            Type::Var(n) => f(n),
            Type::Bool => Type::Bool,
            Type::Int => Type::Int,
            Type::Char => Type::Char,
            Type::String => Type::String,
            Type::Bytes => Type::Bytes,
            Type::Arrow => Type::Arrow,
            Type::FatArrow => Type::FatArrow,
            Type::Constraints(cs) => Type::Constraints(cs.iter().map(|c| c.subst(f)).collect()),
            Type::Array => Type::Array,
            Type::Record => Type::Record,
            Type::Variant => Type::Variant,
            Type::IO => Type::IO,
            Type::App(a, b) => Type::mk_app(a.subst(f), b.subst(f)),
            Type::RowNil => Type::RowNil,
            Type::RowCons(field, ty, rest) => {
                Type::mk_rowcons(field.clone(), ty.subst(f), rest.subst(f))
            }
            Type::HasField(field, rest) => Type::mk_hasfield(field.clone(), rest.subst(f)),
            Type::Unit => Type::Unit,
            Type::Meta(n) => Type::Meta(*n),
        }
    }

    pub fn iter_vars(&self) -> IterVars<A> {
        IterVars { items: vec![self] }
    }

    pub fn iter_metas(&self) -> TypeIterMetas<A> {
        TypeIterMetas { items: vec![self] }
    }

    pub fn unwrap_constraints(&self) -> (Vec<&Type<A>>, &Type<A>) {
        fn flatten_constraints<A>(ty: &Type<A>) -> Vec<&Type<A>> {
            match ty {
                Type::Constraints(cs) => cs.iter().flat_map(|c| flatten_constraints(c)).collect(),
                _ => vec![ty],
            }
        }

        pub fn go<'a, A>(constraints: &mut Vec<&'a Type<A>>, ty: &'a Type<A>) -> &'a Type<A> {
            match ty.unwrap_fatarrow() {
                None => ty,
                Some((c, ty)) => {
                    let more_constraints = flatten_constraints(c);
                    constraints.extend(more_constraints.iter());

                    go(constraints, ty)
                }
            }
        }
        let mut constraints = Vec::new();
        let ty = go(&mut constraints, self);
        (constraints, ty)
    }

    fn unwrap_arrow(&self) -> Option<(&Type<A>, &Type<A>)> {
        match self {
            Type::App(a, out_ty) => match **a {
                Type::App(ref c, ref in_ty) => match **c {
                    Type::Arrow => Some((in_ty, out_ty)),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    fn unwrap_fatarrow(&self) -> Option<(&Type<A>, &Type<A>)> {
        match self {
            Type::App(a, out_ty) => match **a {
                Type::App(ref c, ref in_ty) => match **c {
                    Type::FatArrow => Some((in_ty, out_ty)),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    pub fn unwrap_rows(&self) -> (Vec<(&String, &Type<A>)>, Option<&Type<A>>) {
        let mut current = self;
        let mut fields = Vec::new();
        loop {
            match current {
                Type::RowNil => return (fields, None),
                Type::RowCons(field, ty, rest) => {
                    fields.push((field, ty));
                    current = rest;
                }
                _ => return (fields, Some(current)),
            }
        }
    }

    pub fn unwrap_record(&self) -> Option<(Vec<(&String, &Type<A>)>, Option<&Type<A>>)> {
        match self {
            Type::App(a, b) => match **a {
                Type::Record => Some(b.unwrap_rows()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn unwrap_variant(&self) -> Option<(Vec<(&String, &Type<A>)>, Option<&Type<A>>)> {
        match self {
            Type::App(a, b) => match **a {
                Type::Variant => Some(b.unwrap_rows()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn mk_app(a: Type<A>, b: Type<A>) -> Type<A> {
        Type::App(Rc::new(a), Rc::new(b))
    }

    pub fn mk_arrow(a: Type<A>, b: Type<A>) -> Type<A> {
        Type::mk_app(Type::mk_app(Type::Arrow, a), b)
    }

    pub fn mk_fatarrow(a: Type<A>, b: Type<A>) -> Type<A> {
        Type::mk_app(Type::mk_app(Type::FatArrow, a), b)
    }

    pub fn mk_name(s: &str) -> Type<A> {
        Type::Name(String::from(s))
    }

    pub fn mk_rowcons(field: String, a: Type<A>, b: Type<A>) -> Type<A> {
        Type::RowCons(field, Rc::new(a), Rc::new(b))
    }

    pub fn mk_hasfield(field: String, rest: Type<A>) -> Type<A> {
        Type::HasField(field, Rc::new(rest))
    }

    pub fn mk_rows(fields: Vec<(String, Type<A>)>, rest: Option<Type<A>>) -> Type<A> {
        let mut ty = rest.unwrap_or(Type::RowNil);
        for (field, a) in fields.into_iter().rev() {
            ty = Type::mk_rowcons(field, a, ty)
        }
        ty
    }

    pub fn mk_record(fields: Vec<(String, Type<A>)>, rest: Option<Type<A>>) -> Type<A> {
        Type::mk_app(Type::Record, Type::mk_rows(fields, rest))
    }

    pub fn mk_variant(ctors: Vec<(String, Type<A>)>, rest: Option<Type<A>>) -> Type<A> {
        Type::mk_app(Type::Variant, Type::mk_rows(ctors, rest))
    }

    pub fn render(&self) -> String
    where
        A: Display,
    {
        let mut s = String::new();

        if let Some((fields, rest)) = self.unwrap_record() {
            s.push('{');
            let mut fields_iter = fields.iter();
            match fields_iter.next() {
                None => {}
                Some((first_field, first_ty)) => {
                    s.push(' ');
                    s.push_str(first_field.as_str());
                    s.push_str(" : ");
                    s.push_str(first_ty.render().as_str());
                    for (field, ty) in fields_iter {
                        s.push_str(", ");
                        s.push_str(field.as_str());
                        s.push_str(" : ");
                        s.push_str(ty.render().as_str());
                    }
                }
            }
            match rest {
                None => {
                    if !fields.is_empty() {
                        s.push(' ')
                    }
                }
                Some(ty) => {
                    if !fields.is_empty() {
                        s.push_str(", ")
                    }
                    s.push_str(ty.render().as_str());
                    s.push(' ');
                }
            }
            s.push('}');
            return s;
        }

        if let Some((fields, rest)) = self.unwrap_variant() {
            s.push('<');
            let mut fields_iter = fields.iter();
            match fields_iter.next() {
                None => {}
                Some((first_field, first_ty)) => {
                    s.push(' ');
                    s.push_str(first_field.as_str());
                    s.push_str(" : ");
                    s.push_str(first_ty.render().as_str());
                    for (field, ty) in fields_iter {
                        s.push_str(" | ");
                        s.push_str(field.as_str());
                        s.push_str(" : ");
                        s.push_str(ty.render().as_str());
                    }
                }
            }
            match rest {
                None => {
                    if !fields.is_empty() {
                        s.push(' ');
                    }
                }
                Some(ty) => {
                    if !fields.is_empty() {
                        s.push_str(" |")
                    }
                    s.push(' ');
                    s.push_str(ty.render().as_str());
                    s.push(' ');
                }
            }
            s.push('>');
            return s;
        }

        if let Some((a, b)) = self.unwrap_arrow() {
            if a.unwrap_arrow().is_some() {
                s.push('(')
            }
            s.push_str(a.render().as_str());
            if a.unwrap_arrow().is_some() {
                s.push(')')
            }
            s.push_str(" -> ");
            s.push_str(b.render().as_str());
            return s;
        }

        if let Some((a, b)) = self.unwrap_fatarrow() {
            if a.unwrap_arrow().is_some() {
                s.push('(')
            }
            s.push_str(a.render().as_str());
            if a.unwrap_arrow().is_some() {
                s.push(')')
            }
            s.push_str(" => ");
            s.push_str(b.render().as_str());
            return s;
        }

        match self {
            Type::Name(n) => s.push_str(n.clone().as_str()),
            Type::Var(n) => s.push_str(format!("{}", n).as_str()),
            Type::Bool => s.push_str("Bool"),
            Type::Int => s.push_str("Int"),
            Type::Char => s.push_str("Char"),
            Type::String => s.push_str("String"),
            Type::Bytes => s.push_str("Bytes"),
            Type::Arrow => s.push_str("(->)"),
            Type::FatArrow => s.push_str("(=>)"),
            Type::Constraints(cs) => {
                s.push('(');
                let mut cs_iter = cs.iter();
                match cs_iter.next() {
                    None => {}
                    Some(first) => {
                        s.push_str(first.render().as_str());
                        for c in cs_iter {
                            s.push_str(", ");
                            s.push_str(c.render().as_str());
                        }
                    }
                }
                s.push(')');
            }
            Type::Array => s.push_str("Array"),
            Type::Record => s.push_str("Record"),
            Type::Variant => s.push_str("Variant"),
            Type::IO => s.push_str("IO"),
            Type::Unit => s.push_str("()"),
            Type::Meta(n) => {
                s.push('?');
                s.push_str(format!("{}", n).as_str());
            }
            Type::RowNil => s.push_str("()"),
            Type::RowCons(field, ty, rest) => {
                s.push('(');
                s.push_str(field.as_str());
                s.push_str(" : ");
                s.push_str(ty.render().as_str());
                s.push_str(" | ");
                s.push_str(rest.render().as_str());
                s.push(')');
            }
            Type::HasField(a, b) => {
                s.push_str(format!("HasField {:?}", a).as_str());
                s.push(' ');

                match **b {
                    Type::App(_, _) | Type::HasField(_, _) => {
                        s.push('(');
                    }
                    _ => {}
                }
                s.push_str(b.render().as_str());
                match **b {
                    Type::App(_, _) | Type::HasField(_, _) => {
                        s.push(')');
                    }
                    _ => {}
                }
            }
            Type::App(a, b) => {
                s.push_str(a.render().as_str());
                s.push(' ');

                match **b {
                    Type::App(_, _) | Type::HasField(_, _) => {
                        s.push('(');
                    }
                    _ => {}
                }
                s.push_str(b.render().as_str());
                match **b {
                    Type::App(_, _) | Type::HasField(_, _) => {
                        s.push(')');
                    }
                    _ => {}
                }
            }
        }
        s
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Names {
    All,
    Names(Vec<String>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Declaration {
    Definition {
        name: String,
        ty: Type<String>,
        args: Vec<Pattern>,
        body: Spanned<Expr>,
    },
    Class {
        supers: Vec<Spanned<Type<String>>>,
        name: String,
        args: Vec<Spanned<String>>,
        members: Vec<(String, Type<String>)>,
    },
    Instance {
        assumes: Vec<Spanned<Type<String>>>,
        name: Spanned<String>,
        args: Vec<Type<String>>,
        members: Vec<(Spanned<String>, Vec<Pattern>, Spanned<Expr>)>,
    },
    TypeAlias {
        name: String,
        args: Vec<String>,
        body: Type<String>,
    },
    Import {
        module: Spanned<String>,
        name: Option<Spanned<String>>,
    },
    FromImport {
        module: Spanned<String>,
        names: Names,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub decls: Vec<Spanned<Declaration>>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Kind {
    Type,
    Row,
    Constraint,
    Arrow(Box<Kind>, Box<Kind>),
    Meta(usize),
}

pub struct KindIterMetas<'a> {
    items: Vec<&'a Kind>,
}

impl<'a> Iterator for KindIterMetas<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        fn step_kind(kind: &Kind) -> Step<Kind, usize> {
            match kind {
                Kind::Type => Step::Skip,
                Kind::Row => Step::Skip,
                Kind::Constraint => Step::Skip,
                Kind::Arrow(a, b) => Step::Continue(vec![a, b]),
                Kind::Meta(n) => Step::Yield(*n),
            }
        }

        let mut res = None;
        loop {
            match self.items.pop() {
                None => {
                    break;
                }
                Some(kind) => match step_kind(kind) {
                    Step::Yield(n) => {
                        res = Some(n);
                        break;
                    }
                    Step::Skip => {
                        continue;
                    }
                    Step::Continue(items) => {
                        self.items.extend(items.iter().rev());
                        continue;
                    }
                },
            }
        }
        res
    }
}

impl Kind {
    pub fn iter_metas(&self) -> KindIterMetas {
        KindIterMetas { items: vec![self] }
    }

    pub fn mk_arrow(a: Kind, b: Kind) -> Kind {
        Kind::Arrow(Box::new(a), Box::new(b))
    }

    pub fn render(&self) -> String {
        match self {
            Kind::Arrow(a, b) => {
                let mut val = String::new();
                if let Kind::Arrow(_, _) = **a {
                    val.push('(')
                }
                val.push_str(a.render().as_str());
                if let Kind::Arrow(_, _) = **a {
                    val.push(')')
                }
                val.push_str(" -> ");
                val.push_str(b.render().as_str());
                val
            }
            Kind::Type => String::from("Type"),
            Kind::Row => String::from("Row"),
            Kind::Constraint => String::from("Constraint"),
            Kind::Meta(n) => format!("?{}", n),
        }
    }
}
