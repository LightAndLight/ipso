pub mod kind;
#[cfg(test)]
mod test;
pub mod r#type;

use quickcheck::Arbitrary;
pub use r#type::Type;
use std::{
    cmp::Ordering,
    collections::HashMap,
    hash::Hash,
    path::{Path, PathBuf},
    rc::Rc,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Spanned<A> {
    pub pos: usize,
    pub item: A,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
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
    Let,
    In,
    Comp,
    Bind,
}

impl Arbitrary for Keyword {
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        *g.choose(&[
            Keyword::Case,
            Keyword::Of,
            Keyword::If,
            Keyword::Then,
            Keyword::Else,
            Keyword::True,
            Keyword::False,
            Keyword::Import,
            Keyword::As,
            Keyword::From,
            Keyword::Type,
            Keyword::Class,
            Keyword::Instance,
            Keyword::Where,
            Keyword::Let,
            Keyword::In,
            Keyword::Comp,
            Keyword::Bind,
        ])
        .unwrap()
    }
}

impl Keyword {
    pub fn num_variants() -> usize {
        18
    }

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
            Keyword::Let => "let",
            Keyword::In => "in",
            Keyword::Comp => "comp",
            Keyword::Bind => "bind",
        }
    }

    pub fn from_string(str: &str) -> Option<Self> {
        match str {
            "case" => Some(Keyword::Case),
            "of" => Some(Keyword::Of),
            "if" => Some(Keyword::If),
            "then" => Some(Keyword::Then),
            "else" => Some(Keyword::Else),
            "true" => Some(Keyword::True),
            "false" => Some(Keyword::False),
            "import" => Some(Keyword::Import),
            "as" => Some(Keyword::As),
            "from" => Some(Keyword::From),
            "type" => Some(Keyword::Type),
            "class" => Some(Keyword::Class),
            "instance" => Some(Keyword::Instance),
            "where" => Some(Keyword::Where),
            "let" => Some(Keyword::Let),
            "in" => Some(Keyword::In),
            "comp" => Some(Keyword::Comp),
            "bind" => Some(Keyword::Bind),
            _ => None,
        }
    }
}

const KEYWORDS: &[&str] = &[
    "case", "of", "if", "then", "else", "true", "false", "import", "as", "from", "where", "type",
    "class", "instance", "let", "in", "comp", "bind",
];

pub fn is_keyword(val: &str) -> bool {
    KEYWORDS.contains(&val)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Assoc {
    None,
    Left,
    Right,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
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

    LApply,
    RApply,
}

impl Binop {
    pub fn assoc(&self) -> Assoc {
        match self {
            Binop::Add => Assoc::Left,
            Binop::Multiply => Assoc::Left,
            Binop::Subtract => Assoc::Left,
            Binop::Divide => Assoc::Left,
            Binop::Append => Assoc::Left,
            Binop::Or => Assoc::Right,
            Binop::And => Assoc::Right,
            Binop::Eq => Assoc::None,
            Binop::Neq => Assoc::None,
            Binop::Gt => Assoc::None,
            Binop::Gte => Assoc::None,
            Binop::Lt => Assoc::None,
            Binop::Lte => Assoc::None,
            Binop::LApply => Assoc::Right,
            Binop::RApply => Assoc::Left,
        }
    }

    fn precedence(&self) -> u16 {
        match self {
            Binop::Multiply => 7,
            Binop::Divide => 7,

            Binop::Add => 6,
            Binop::Subtract => 6,
            Binop::Append => 6,

            Binop::Eq => 5,
            Binop::Neq => 5,
            Binop::Gt => 5,
            Binop::Gte => 5,
            Binop::Lt => 5,
            Binop::Lte => 5,

            Binop::Or => 4,
            Binop::And => 4,

            Binop::LApply => 3,
            Binop::RApply => 3,
        }
    }

    pub fn compare_precedence(&self, other: &Binop) -> Ordering {
        self.precedence().cmp(&other.precedence())
    }

    pub fn render(&self) -> &'static str {
        match self {
            Binop::Add => "+",
            Binop::Multiply => "*",
            Binop::Subtract => "-",
            Binop::Divide => "/",
            Binop::Append => "++",
            Binop::Or => "||",
            Binop::And => "&&",
            Binop::Eq => "==",
            Binop::Neq => "!=",
            Binop::Gt => ">",
            Binop::Gte => ">=",
            Binop::Lt => "<",
            Binop::Lte => "<=",
            Binop::LApply => "<|",
            Binop::RApply => "|>",
        }
    }
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
    Char(Spanned<char>),
    Int(Spanned<u32>),
    String(Spanned<Rc<str>>),
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
                    Pattern::Char(_) => None,
                    Pattern::Int(_) => None,
                    Pattern::String(_) => None,
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

    pub fn get_arg_names(&self) -> Vec<&Spanned<String>> {
        let mut arg_names = Vec::new();
        match self {
            Pattern::Name(n) => {
                arg_names.push(n);
            }
            Pattern::Record { names, rest } => {
                for name in names {
                    arg_names.push(name);
                }
                match rest {
                    None => {}
                    Some(n) => {
                        arg_names.push(n);
                    }
                }
            }
            Pattern::Variant { name: _, arg } => {
                arg_names.push(arg);
            }
            Pattern::Char(_) => {}
            Pattern::Int(_) => {}
            Pattern::String(_) => {}
            Pattern::Wildcard => {}
        }
        arg_names
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
pub enum CompLine {
    Expr(Spanned<Expr>),
    Bind(Rc<str>, Spanned<Expr>),
    Let(Rc<str>, Spanned<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CmdPart {
    Literal(Rc<str>),
    Expr(Spanned<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Var(String),
    Module {
        id: ModuleId,

        /**
        A chain of submodule accessors.

        e.g. `module.submodule1.submodule2`
        */
        path: Vec<String>,

        /**
        The referenced item.

        e.g. `module.submodule.item`
        */
        item: String,
    },

    App(Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),
    Lam {
        args: Vec<Spanned<Pattern>>,
        body: Rc<Spanned<Expr>>,
    },

    Let {
        name: Rc<str>,
        value: Rc<Spanned<Expr>>,
        rest: Rc<Spanned<Expr>>,
    },

    True,
    False,
    IfThenElse(Rc<Spanned<Expr>>, Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),

    Int(u32),

    Binop(Spanned<Binop>, Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),

    Char(char),

    String(Vec<StringPart>),

    Array(Vec<Spanned<Expr>>),

    Record {
        fields: Vec<(String, Spanned<Expr>)>,
        rest: Option<Rc<Spanned<Expr>>>,
    },
    Project(Rc<Spanned<Expr>>, String),

    Variant(Spanned<String>),
    Embed(Spanned<String>, Rc<Spanned<Expr>>),
    Case(Rc<Spanned<Expr>>, Vec<Branch>),

    Unit,

    Comp(Vec<CompLine>),

    Cmd(Vec<CmdPart>),
}

impl Expr {
    pub fn mk_project(val: Spanned<Expr>, field: String) -> Expr {
        Expr::Project(Rc::new(val), field)
    }

    pub fn mk_ifthenelse(cond: Spanned<Expr>, then: Spanned<Expr>, else_: Spanned<Expr>) -> Expr {
        Expr::IfThenElse(Rc::new(cond), Rc::new(then), Rc::new(else_))
    }

    pub fn mk_var(v: &str) -> Expr {
        Expr::Var(String::from(v))
    }

    pub fn mk_lam(args: Vec<Spanned<Pattern>>, body: Spanned<Expr>) -> Expr {
        Expr::Lam {
            args,
            body: Rc::new(body),
        }
    }

    pub fn mk_case(cond: Spanned<Expr>, branches: Vec<Branch>) -> Expr {
        Expr::Case(Rc::new(cond), branches)
    }

    pub fn mk_app(a: Spanned<Expr>, b: Spanned<Expr>) -> Spanned<Expr> {
        Spanned {
            pos: a.pos,
            item: Expr::App(Rc::new(a), Rc::new(b)),
        }
    }

    pub fn mk_binop(op: Spanned<Binop>, a: Spanned<Expr>, b: Spanned<Expr>) -> Spanned<Expr> {
        Spanned {
            pos: a.pos,
            item: Expr::Binop(op, Rc::new(a), Rc::new(b)),
        }
    }

    pub fn mk_record(fields: Vec<(String, Spanned<Expr>)>, rest: Option<Spanned<Expr>>) -> Expr {
        Expr::Record {
            fields,
            rest: rest.map(Rc::new),
        }
    }

    pub fn mk_embed(ctor: Spanned<String>, rest: Spanned<Expr>) -> Expr {
        Expr::Embed(ctor, Rc::new(rest))
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
pub enum Names {
    All,
    Names(Vec<Spanned<String>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Declaration {
    Definition {
        name: String,
        ty: Type<Rc<str>>,
        args: Vec<Spanned<Pattern>>,
        body: Spanned<Expr>,
    },
    Class {
        supers: Vec<Spanned<Type<Rc<str>>>>,
        name: Rc<str>,
        args: Vec<Spanned<Rc<str>>>,
        members: Vec<(String, Type<Rc<str>>)>,
    },
    Instance {
        assumes: Vec<Spanned<Type<Rc<str>>>>,
        name: Spanned<Rc<str>>,
        args: Vec<Spanned<Type<Rc<str>>>>,
        members: Vec<(Spanned<String>, Vec<Spanned<Pattern>>, Spanned<Expr>)>,
    },
    TypeAlias {
        name: String,
        args: Vec<String>,
        body: Type<Rc<str>>,
    },
    Import {
        resolved: Option<ModuleId>,
        module: Spanned<String>,
        as_name: Option<Spanned<String>>,
    },
    FromImport {
        resolved: Option<ModuleId>,
        module: Spanned<String>,
        names: Names,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub decls: Vec<Spanned<Declaration>>,
}

pub struct Modules<M> {
    data: Vec<M>,
    path_to_index: HashMap<PathBuf, usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(usize);

impl<M> Default for Modules<M> {
    fn default() -> Self {
        Self::new()
    }
}

impl<M> Modules<M> {
    pub fn new() -> Self {
        Modules {
            data: vec![],
            path_to_index: HashMap::new(),
        }
    }

    pub fn iter_paths(&self) -> impl Iterator<Item = (&Path, &M)> {
        let data = &self.data;
        self.path_to_index
            .iter()
            .map(move |(path, index)| (path.as_path(), &data[*index]))
    }

    pub fn iter_ids(&self) -> impl Iterator<Item = (ModuleId, &M)> {
        let data = &self.data;
        self.path_to_index
            .iter()
            .map(move |(_, index)| (ModuleId(*index), &data[*index]))
    }

    pub fn lookup(&self, id: ModuleId) -> &M {
        &self.data[id.0]
    }

    pub fn lookup_id(&self, path: &Path) -> Option<ModuleId> {
        self.path_to_index.get(path).map(|index| ModuleId(*index))
    }

    pub fn insert(&mut self, path: PathBuf, module: M) -> ModuleId {
        let index = self.data.len();
        self.data.push(module);
        self.path_to_index.insert(path, index);
        ModuleId(index)
    }
}
