use lazy_static::lazy_static;
use std::collections::HashMap;

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
    pub pattern: Pattern,
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
        fields: HashMap<String, Spanned<Expr>>,
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
    Meta(usize),
}

impl Type {
    fn unwrap_arrow<'a>(&'a self) -> Option<(&'a Type, &'a Type)> {
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

    fn unwrap_fatarrow<'a>(&'a self) -> Option<(&'a Type, &'a Type)> {
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

    fn unwrap_rows<'a>(&'a self) -> (Vec<(&'a String, &'a Type)>, Option<&'a Type>) {
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

    fn unwrap_record<'a>(&'a self) -> Option<(Vec<(&'a String, &'a Type)>, Option<&'a Type>)> {
        match self {
            Type::App(a, b) => match **a {
                Type::Record => Some(b.unwrap_rows()),
                _ => None,
            },
            _ => None,
        }
    }

    fn unwrap_variant<'a>(&'a self) -> Option<(Vec<(&'a String, &'a Type)>, Option<&'a Type>)> {
        match self {
            Type::App(a, b) => match **a {
                Type::Variant => Some(b.unwrap_rows()),
                _ => None,
            },
            _ => None,
        }
    }

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

    pub fn mk_rowcons(field: String, a: Type, b: Type) -> Type {
        Type::RowCons(field, Box::new(a), Box::new(b))
    }

    pub fn mk_record(fields: Vec<(String, Type)>, rest: Option<Type>) -> Type {
        let mut ty = rest.unwrap_or(Type::RowNil);
        for (field, a) in fields.into_iter().rev() {
            ty = Type::mk_rowcons(field, a, ty)
        }
        Type::mk_app(Type::Record, ty)
    }

    pub fn mk_variant(ctors: Vec<(String, Type)>, rest: Option<Type>) -> Type {
        let mut ty = rest.unwrap_or(Type::RowNil);
        for (field, arg) in ctors.into_iter().rev() {
            ty = Type::mk_rowcons(field, arg, ty)
        }
        Type::mk_app(Type::Variant, ty)
    }

    pub fn render(&self) -> String {
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
        ty: Type,
        args: Vec<Pattern>,
        body: Spanned<Expr>,
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
        names: Names,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub decls: Vec<Spanned<Declaration>>,
}
