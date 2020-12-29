use std::fmt::Display;

use lazy_static::lazy_static;

#[cfg(test)]
mod test;

#[derive(Debug, PartialEq, Eq)]
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
            Keyword::Type => "type",
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
    Expr(Spanned<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct Branch {
    pub pattern: Spanned<Pattern>,
    pub body: Spanned<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Var(String),

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

    Variant(String, Box<Spanned<Expr>>),
    Case(Box<Spanned<Expr>>, Vec<Branch>),

    Unit,
}

impl Expr {
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
            rest: rest.map(|x| Box::new(x)),
        }
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
    Arrow,
    FatArrow,
    Constraints(Vec<Type<A>>),
    Array,
    Record,
    Variant,
    IO,
    App(Box<Type<A>>, Box<Type<A>>),
    RowNil,
    RowCons(String, Box<Type<A>>, Box<Type<A>>),
    Unit,
    Meta(usize),
}

#[derive(Debug)]
pub struct IterVars<'a, A> {
    items: Vec<&'a Type<A>>,
}

impl<'a, A: Clone> Iterator for IterVars<'a, A> {
    type Item = A;

    fn next(&mut self) -> Option<Self::Item> {
        #[derive(Debug)]
        enum Step<'a, A> {
            Yield(A),
            Skip,
            Continue(Vec<&'a Type<A>>),
        }

        fn step_type<'a, A>(ty: &'a Type<A>) -> Step<'a, A>
        where
            A: Clone,
        {
            match ty {
                Type::Name(_) => Step::Skip,
                Type::Var(n) => Step::Yield(n.clone()),
                Type::Bool => Step::Skip,
                Type::Int => Step::Skip,
                Type::Char => Step::Skip,
                Type::String => Step::Skip,
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
        return result;
    }
}

impl<A> Type<A> {
    pub fn map<B, F: FnMut(&A) -> B>(&self, f: &mut F) -> Type<B> {
        match self {
            Type::Name(n) => Type::Name(n.clone()),
            Type::Var(x) => Type::Var(f(x)),
            Type::Bool => Type::Bool,
            Type::Int => Type::Int,
            Type::Char => Type::Char,
            Type::String => Type::String,
            Type::Arrow => Type::Arrow,
            Type::FatArrow => Type::FatArrow,
            Type::Constraints(cs) => Type::Constraints(cs.iter().map(|c| c.map(f)).collect()),
            Type::Array => Type::Arrow,
            Type::Record => Type::Record,
            Type::Variant => Type::Variant,
            Type::IO => Type::IO,
            Type::App(a, b) => Type::mk_app(a.map(f), b.map(f)),
            Type::RowNil => Type::RowNil,
            Type::RowCons(field, ty, rest) => {
                Type::mk_rowcons(field.clone(), ty.map(f), rest.map(f))
            }
            Type::Unit => Type::Unit,
            Type::Meta(n) => Type::Meta(*n),
        }
    }

    pub fn iter_vars<'a>(&'a self) -> IterVars<'a, A> {
        IterVars { items: vec![&self] }
    }

    fn unwrap_arrow<'a>(&'a self) -> Option<(&'a Type<A>, &'a Type<A>)> {
        match self {
            Type::App(a, b) => match **a {
                Type::App(ref c, ref d) => match **c {
                    Type::Arrow => Some((b, d)),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    fn unwrap_fatarrow<'a>(&'a self) -> Option<(&'a Type<A>, &'a Type<A>)> {
        match self {
            Type::App(a, b) => match **a {
                Type::App(ref c, ref d) => match **c {
                    Type::FatArrow => Some((b, d)),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    pub fn unwrap_rows<'a>(&'a self) -> (Vec<(&'a String, &'a Type<A>)>, Option<&'a Type<A>>) {
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

    pub fn unwrap_record<'a>(
        &'a self,
    ) -> Option<(Vec<(&'a String, &'a Type<A>)>, Option<&'a Type<A>>)> {
        match self {
            Type::App(a, b) => match **a {
                Type::Record => Some(b.unwrap_rows()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn unwrap_variant<'a>(
        &'a self,
    ) -> Option<(Vec<(&'a String, &'a Type<A>)>, Option<&'a Type<A>>)> {
        match self {
            Type::App(a, b) => match **a {
                Type::Variant => Some(b.unwrap_rows()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn mk_app(a: Type<A>, b: Type<A>) -> Type<A> {
        Type::App(Box::new(a), Box::new(b))
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
        Type::RowCons(field, Box::new(a), Box::new(b))
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

        match self.unwrap_record() {
            Some((fields, rest)) => {
                s.push('{');
                let mut fields_iter = fields.iter();
                match fields_iter.next() {
                    None => {}
                    Some((first_field, first_ty)) => {
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
                    None => {}
                    Some(ty) => {
                        if fields.len() > 0 {
                            s.push_str(", ")
                        }
                        s.push_str(ty.render().as_str());
                    }
                }
                s.push('}');
                return s;
            }
            None => {}
        }

        match self.unwrap_variant() {
            Some((fields, rest)) => {
                s.push('<');
                let mut fields_iter = fields.iter();
                match fields_iter.next() {
                    None => {}
                    Some((first_field, first_ty)) => {
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
                    None => {}
                    Some(ty) => {
                        if fields.len() > 0 {
                            s.push_str(" | ")
                        }
                        s.push_str(ty.render().as_str());
                    }
                }
                s.push('>');
                return s;
            }
            None => {}
        }

        match self.unwrap_arrow() {
            Some((a, b)) => {
                match a.unwrap_arrow() {
                    Some(_) => s.push('('),
                    None => {}
                }
                s.push_str(a.render().as_str());
                match a.unwrap_arrow() {
                    Some(_) => s.push(')'),
                    None => {}
                }
                s.push_str(" -> ");
                s.push_str(b.render().as_str());
                return s;
            }
            None => {}
        }

        match self.unwrap_fatarrow() {
            Some((a, b)) => {
                match a.unwrap_arrow() {
                    Some(_) => s.push('('),
                    None => {}
                }
                s.push_str(a.render().as_str());
                match a.unwrap_arrow() {
                    Some(_) => s.push(')'),
                    None => {}
                }
                s.push_str(" => ");
                s.push_str(b.render().as_str());
                return s;
            }
            None => {}
        }

        match self {
            Type::Name(n) => s.push_str(n.clone().as_str()),
            Type::Var(n) => s.push_str(format!("#{}", n).as_str()),
            Type::Bool => s.push_str("Bool"),
            Type::Int => s.push_str("Int"),
            Type::Char => s.push_str("Char"),
            Type::String => s.push_str("String"),
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
            Type::App(a, b) => {
                s.push_str(a.render().as_str());
                s.push(' ');

                match **b {
                    Type::App(_, _) => {
                        s.push('(');
                    }
                    _ => {}
                }
                s.push_str(b.render().as_str());
                match **b {
                    Type::App(_, _) => {
                        s.push(')');
                    }
                    _ => {}
                }
            }
        }
        s
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
        ty: Type<String>,
        args: Vec<Pattern>,
        body: Spanned<Expr>,
    },
    TypeAlias {
        name: String,
        args: Vec<String>,
        body: Type<String>,
    },
    Import {
        module: String,
        name: Option<String>,
    },
    FromImport {
        module: String,
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

impl Kind {
    pub fn mk_arrow(a: Kind, b: Kind) -> Kind {
        Kind::Arrow(Box::new(a), Box::new(b))
    }

    pub fn render(&self) -> String {
        match self {
            Kind::Arrow(a, b) => {
                let mut val = String::new();
                match **a {
                    Kind::Arrow(_, _) => val.push('('),
                    _ => {}
                }
                val.push_str(a.render().as_str());
                match **a {
                    Kind::Arrow(_, _) => val.push(')'),
                    _ => {}
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
