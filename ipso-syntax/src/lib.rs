#![deny(unused_crate_dependencies)]

#[cfg(test)]
mod test;

pub mod desugar;
pub mod kind;
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

impl<A> Spanned<A> {
    pub fn map<B>(&self, f: impl FnOnce(&A) -> B) -> Spanned<B> {
        Spanned {
            pos: self.pos,
            item: f(&self.item),
        }
    }
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
    Name(Spanned<Rc<str>>),
    Record {
        names: Vec<Spanned<Rc<str>>>,
        rest: Option<Spanned<Rc<str>>>,
    },
    Variant {
        name: Rc<str>,
        arg: Spanned<Box<Pattern>>,
    },
    Char(Spanned<char>),
    Int(Spanned<i32>),
    String(Spanned<Rc<str>>),
    Unit,
    Wildcard,
}

pub struct IterNames<'a> {
    items: Vec<&'a Spanned<Rc<str>>>,
    pattern: Option<&'a Pattern>,
}

impl<'a> Iterator for IterNames<'a> {
    type Item = &'a Spanned<Rc<str>>;

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
                    Pattern::Variant { name: _, arg } => {
                        self.pattern = Some(&arg.item);
                        self.next()
                    }
                    Pattern::Char(_) => None,
                    Pattern::Int(_) => None,
                    Pattern::String(_) => None,
                    Pattern::Unit => None,
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
pub enum CompLine {
    Expr(Spanned<Expr>),
    Bind(Spanned<Rc<str>>, Spanned<Expr>),
    Let(Spanned<Rc<str>>, Spanned<Expr>),
}

/**
Part of a command.

## Examples

This command has 3 parts:

```bash
ls -l /a/b
^^ ^^ ^^^^
```

So does this command:

```bash
echo $a $b
^^^^ ^^ ^^
```

This command has 2 parts:

```bash
echo "$a $b"
^^^^ ^^^^^
```

This command also has 2 parts:

```bash
echo $a/$b
^^^^ ^^^^^
```
*/
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CmdPart {
    Arg(Vec<StringPart>),
    Args(Spanned<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ModuleRef {
    /// A module self-reference.
    This,

    /// A reference to another module.
    Id(ModuleId),
}

impl From<ModuleId> for ModuleRef {
    fn from(id: ModuleId) -> Self {
        Self::Id(id)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Var(String),
    Module {
        /// A reference to a module.
        id: ModuleRef,

        /**
        A chain of submodule accessors.

        e.g. `module.submodule1.submodule2`
        */
        path: Vec<Spanned<String>>,

        /**
        The referenced item.

        e.g. `module.submodule.item`
        */
        item: Spanned<String>,
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

    Int(i32),

    Binop(Spanned<Binop>, Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),

    Char(char),

    String(Vec<StringPart>),

    Array(Vec<Spanned<Expr>>),

    Record {
        fields: Vec<(String, Spanned<Expr>)>,
        rest: Option<Rc<Spanned<Expr>>>,
    },
    Project(Rc<Spanned<Expr>>, Spanned<String>),

    Variant(Spanned<String>),
    Embed(Spanned<String>, Rc<Spanned<Expr>>),
    Case(Rc<Spanned<Expr>>, Vec<Branch>),

    Unit,

    Comp(Vec<Spanned<CompLine>>),

    Cmd(Vec<CmdPart>),
}

impl Expr {
    pub fn mk_project(val: Spanned<Expr>, field: Spanned<String>) -> Expr {
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
                    fields.push(&field.item);
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
pub struct InstanceMember {
    pub name: Spanned<String>,
    pub args: Vec<Spanned<Pattern>>,
    pub body: Spanned<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Declaration {
    Definition {
        name: Spanned<Rc<str>>,
        ty: Spanned<Type<Rc<str>>>,
        args: Vec<Spanned<Pattern>>,
        body: Spanned<Expr>,
    },
    Class {
        supers: Vec<Spanned<Type<Rc<str>>>>,
        name: Rc<str>,
        args: Vec<Spanned<Rc<str>>>,
        members: Vec<(String, Spanned<Type<Rc<str>>>)>,
    },
    Instance {
        assumes: Vec<Spanned<Type<Rc<str>>>>,
        name: Spanned<Rc<str>>,
        args: Vec<Spanned<Type<Rc<str>>>>,
        members: Vec<InstanceMember>,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ModuleKey {
    Path(PathBuf),
    Name(String),
}

impl From<&Path> for ModuleKey {
    fn from(path: &Path) -> Self {
        Self::Path(PathBuf::from(path))
    }
}

impl From<&str> for ModuleKey {
    fn from(name: &str) -> Self {
        Self::Name(String::from(name))
    }
}

pub struct Modules<M> {
    data: Vec<M>,
    key_to_index: HashMap<ModuleKey, usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(usize);

impl ModuleId {
    pub fn new(value: usize) -> Self {
        ModuleId(value)
    }
}

impl<M> Default for Modules<M> {
    fn default() -> Self {
        Self::new()
    }
}

impl<M> Modules<M> {
    pub fn new() -> Self {
        Modules {
            data: vec![],
            key_to_index: HashMap::new(),
        }
    }

    pub fn iter_keys(&self) -> impl Iterator<Item = (&ModuleKey, &M)> {
        let data = &self.data;
        self.key_to_index
            .iter()
            .map(move |(key, index)| (key, &data[*index]))
    }

    pub fn iter_ids(&self) -> impl Iterator<Item = (ModuleId, &M)> {
        let data = &self.data;
        self.key_to_index
            .iter()
            .map(move |(_, index)| (ModuleId(*index), &data[*index]))
    }

    pub fn lookup(&self, id: ModuleId) -> &M {
        &self.data[id.0]
    }

    pub fn lookup_id(&self, key: &ModuleKey) -> Option<ModuleId> {
        self.key_to_index.get(key).map(|index| ModuleId(*index))
    }

    pub fn insert(&mut self, key: ModuleKey, module: M) -> ModuleId {
        let index = self.data.len();
        self.data.push(module);
        self.key_to_index.insert(key, index);
        ModuleId(index)
    }
}
