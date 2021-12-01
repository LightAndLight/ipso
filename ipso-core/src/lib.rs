#[cfg(test)]
mod test;

use ipso_syntax::{self as syntax, kind::Kind, r#type, ModuleName};
use ipso_util::iter::Step;
use std::{
    cmp,
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

/**
Well-kinded types.

Atomic types like `Int` and `Bool` are inherently well-kinded,
and their kind is always known. Compound types may be ill-kinded if they are constructed
incorrectly, e.g. by trying to form `x y : Type` where `x : Type` and `y : Type`.
When kind polymorphism isn't in play, the kind of a compound type is always known; it's
just well-formedness that needs to be checked.
*/
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Bool,
    Int,
    Char,
    String,
    Bytes,
    RowNil,
    Unit,
    Constraints(Vec<Type>),
    RowCons(Rc<str>, Rc<Type>, Rc<Type>),
    HasField(Rc<str>, Rc<Type>),
    Arrow(Kind),
    FatArrow(Kind),
    Array(Kind),
    Record(Kind),
    Variant(Kind),
    IO(Kind),
    Name(Kind, Rc<str>),
    Var(Kind, usize),
    App(Kind, Rc<Type>, Rc<Type>),
    Meta(Kind, usize),
}

pub struct CommonKinds {
    pub type_to_type: Kind,
    pub type_to_type_to_type: Kind,
    pub constraint_to_type_to_type: Kind,
    pub row_to_type: Kind,
}

impl Default for CommonKinds {
    fn default() -> Self {
        let type_to_type = Kind::mk_arrow(Kind::Type, Kind::Type);
        Self {
            type_to_type_to_type: Kind::mk_arrow(Kind::Type, type_to_type.clone()),
            constraint_to_type_to_type: Kind::mk_arrow(Kind::Constraint, type_to_type.clone()),
            type_to_type,
            row_to_type: Kind::mk_arrow(Kind::Row, Kind::Type),
        }
    }
}

pub struct RowParts<'a> {
    pub fields: Vec<(&'a Rc<str>, &'a Type)>,
    pub rest: Option<&'a Type>,
}

impl Type {
    pub fn to_syntax(&self) -> r#type::Type<usize> {
        match self {
            Type::Bool => r#type::Type::Bool,
            Type::Int => r#type::Type::Int,
            Type::Char => r#type::Type::Char,
            Type::String => r#type::Type::String,
            Type::Bytes => r#type::Type::Bytes,
            Type::Arrow(_) => r#type::Type::Arrow,
            Type::FatArrow(_) => r#type::Type::FatArrow,
            Type::Array(_) => r#type::Type::Array,
            Type::Record(_) => r#type::Type::Record,
            Type::Variant(_) => r#type::Type::Variant,
            Type::IO(_) => r#type::Type::IO,
            Type::RowNil => r#type::Type::RowNil,
            Type::Unit => r#type::Type::Unit,
            Type::Name(_, n) => r#type::Type::Name(n.clone()),
            Type::Var(_, v) => r#type::Type::Var(*v),
            Type::Constraints(cs) => {
                r#type::Type::Constraints(cs.iter().map(Type::to_syntax).collect())
            }
            Type::App(_, a, b) => r#type::Type::mk_app(a.to_syntax(), b.to_syntax()),
            Type::RowCons(field, a, b) => {
                r#type::Type::mk_rowcons(field.clone(), a.to_syntax(), b.to_syntax())
            }
            Type::HasField(field, ty) => r#type::Type::mk_hasfield(field.clone(), ty.to_syntax()),
            Type::Meta(_, m) => r#type::Type::Meta(*m),
        }
    }

    pub fn kind(&self) -> Kind {
        match self {
            Type::Bool => Kind::Type,
            Type::Int => Kind::Type,
            Type::Char => Kind::Type,
            Type::String => Kind::Type,
            Type::Bytes => Kind::Type,
            Type::Unit => Kind::Type,
            Type::RowNil => Kind::Row,
            Type::RowCons(_, _, _) => Kind::Row,
            Type::Constraints(_) => Kind::Constraint,
            Type::HasField(_, _) => Kind::Constraint,
            Type::Array(k) => k.clone(),
            Type::IO(k) => k.clone(),
            Type::Record(k) => k.clone(),
            Type::Variant(k) => k.clone(),
            Type::Arrow(k) => k.clone(),
            Type::FatArrow(k) => k.clone(),
            Type::Name(k, _) => k.clone(),
            Type::Var(k, _) => k.clone(),
            Type::App(k, _, _) => k.clone(),
            Type::Meta(k, _) => k.clone(),
        }
    }

    pub fn unsafe_mk_name(name: Rc<str>, kind: Kind) -> Self {
        Type::Name(kind, name)
    }

    pub fn unsafe_mk_var(ix: usize, kind: Kind) -> Self {
        Type::Var(kind, ix)
    }

    pub fn mk_app(a: Type, b: Type) -> Self {
        Type::App(
            match a.kind() {
                Kind::Ref(r) => match r.as_ref() {
                    syntax::kind::KindCompound::Arrow(_, b) => b.clone(),
                },
                r => panic!("{:?} has no return kind", r),
            },
            Rc::new(a),
            Rc::new(b),
        )
    }

    pub fn mk_arrow_ctor(common_kinds: &CommonKinds) -> Self {
        Type::Arrow(common_kinds.type_to_type_to_type.clone())
    }

    pub fn mk_arrow(common_kinds: &CommonKinds, a: Type, b: Type) -> Self {
        Type::mk_app(Type::mk_app(Type::mk_arrow_ctor(common_kinds), a), b)
    }

    pub fn mk_fatarrow_ctor(common_kinds: &CommonKinds) -> Self {
        Type::FatArrow(common_kinds.constraint_to_type_to_type.clone())
    }

    pub fn mk_fatarrow(common_kinds: &CommonKinds, a: Type, b: Type) -> Self {
        Type::mk_app(Type::mk_app(Type::mk_fatarrow_ctor(common_kinds), a), b)
    }

    pub fn mk_io(common_kinds: &CommonKinds) -> Self {
        Type::IO(common_kinds.type_to_type.clone())
    }

    pub fn mk_array(common_kinds: &CommonKinds) -> Self {
        Type::Array(common_kinds.type_to_type.clone())
    }

    pub fn mk_hasfield(name: Rc<str>, ty: Type) -> Self {
        Type::HasField(name, Rc::new(ty))
    }

    pub fn mk_rowcons(field: Rc<str>, a: Type, b: Type) -> Type {
        Type::RowCons(field, Rc::new(a), Rc::new(b))
    }

    pub fn mk_rows(fields: Vec<(Rc<str>, Type)>, rest: Option<Type>) -> Self {
        fields.into_iter().rev().fold(
            match rest {
                None => Type::RowNil,
                Some(rest) => rest,
            },
            |acc, (field, ty)| Type::mk_rowcons(field, ty, acc),
        )
    }

    pub fn mk_record_ctor(common_kinds: &CommonKinds) -> Self {
        Type::Record(common_kinds.row_to_type.clone())
    }

    pub fn mk_record(
        common_kinds: &CommonKinds,
        fields: Vec<(Rc<str>, Type)>,
        rest: Option<Type>,
    ) -> Self {
        Type::mk_app(
            Type::mk_record_ctor(common_kinds),
            Type::mk_rows(fields, rest),
        )
    }

    pub fn mk_variant_ctor(common_kinds: &CommonKinds) -> Self {
        Type::Variant(common_kinds.row_to_type.clone())
    }

    pub fn mk_variant(
        common_kinds: &CommonKinds,
        fields: Vec<(Rc<str>, Type)>,
        rest: Option<Type>,
    ) -> Self {
        Type::mk_app(
            Type::mk_variant_ctor(common_kinds),
            Type::mk_rows(fields, rest),
        )
    }

    pub fn subst_metas<F: Fn(&Kind, usize) -> Type>(&self, f: &F) -> Self {
        match self {
            Type::Name(k, n) => Type::Name(k.clone(), n.clone()),
            Type::Var(k, n) => Type::Var(k.clone(), *n),
            Type::Bool => Type::Bool,
            Type::Int => Type::Int,
            Type::Char => Type::Char,
            Type::String => Type::String,
            Type::Bytes => Type::Bytes,
            Type::Arrow(k) => Type::Arrow(k.clone()),
            Type::FatArrow(k) => Type::FatArrow(k.clone()),
            Type::Constraints(cs) => {
                Type::Constraints(cs.iter().map(|c| c.subst_metas(f)).collect())
            }
            Type::Array(k) => Type::Array(k.clone()),
            Type::Record(k) => Type::Record(k.clone()),
            Type::Variant(k) => Type::Variant(k.clone()),
            Type::IO(k) => Type::IO(k.clone()),
            Type::App(k, a, b) => Type::App(
                k.clone(),
                Rc::new(a.subst_metas(f)),
                Rc::new(b.subst_metas(f)),
            ),
            Type::RowNil => Type::RowNil,
            Type::RowCons(field, ty, rest) => {
                Type::mk_rowcons(field.clone(), ty.subst_metas(f), rest.subst_metas(f))
            }
            Type::HasField(field, rest) => Type::mk_hasfield(field.clone(), rest.subst_metas(f)),
            Type::Unit => Type::Unit,
            Type::Meta(k, n) => f(k, *n),
        }
    }

    pub fn subst<F: Fn(&usize) -> Type>(&self, f: &F) -> Self {
        match self {
            Type::Name(k, n) => Type::Name(k.clone(), n.clone()),
            Type::Var(_, n) => f(n),
            Type::Bool => Type::Bool,
            Type::Int => Type::Int,
            Type::Char => Type::Char,
            Type::String => Type::String,
            Type::Bytes => Type::Bytes,
            Type::Arrow(k) => Type::Arrow(k.clone()),
            Type::FatArrow(k) => Type::FatArrow(k.clone()),
            Type::Constraints(cs) => Type::Constraints(cs.iter().map(|c| c.subst(f)).collect()),
            Type::Array(k) => Type::Array(k.clone()),
            Type::Record(k) => Type::Record(k.clone()),
            Type::Variant(k) => Type::Variant(k.clone()),
            Type::IO(k) => Type::IO(k.clone()),
            Type::App(k, a, b) => Type::App(k.clone(), Rc::new(a.subst(f)), Rc::new(b.subst(f))),
            Type::RowNil => Type::RowNil,
            Type::RowCons(field, ty, rest) => {
                Type::mk_rowcons(field.clone(), ty.subst(f), rest.subst(f))
            }
            Type::HasField(field, rest) => Type::mk_hasfield(field.clone(), rest.subst(f)),
            Type::Unit => Type::Unit,
            Type::Meta(k, n) => Type::Meta(k.clone(), *n),
        }
    }

    pub fn instantiate_many(&self, tys: &[Type]) -> Self {
        match self {
            Type::Name(k, n) => Type::Name(k.clone(), n.clone()),
            Type::Var(k, n) => {
                let tys_len = tys.len();
                if *n < tys_len {
                    tys[tys_len - 1 - n].clone()
                } else {
                    Type::Var(k.clone(), *n - tys_len)
                }
            }
            Type::Bool => Type::Bool,
            Type::Int => Type::Int,
            Type::Char => Type::Char,
            Type::String => Type::String,
            Type::Bytes => Type::Bytes,
            Type::Arrow(k) => Type::Arrow(k.clone()),
            Type::FatArrow(k) => Type::FatArrow(k.clone()),
            Type::Constraints(cs) => {
                Type::Constraints(cs.iter().map(|c| c.instantiate_many(tys)).collect())
            }
            Type::Array(k) => Type::Array(k.clone()),
            Type::Record(k) => Type::Record(k.clone()),
            Type::Variant(k) => Type::Variant(k.clone()),
            Type::IO(k) => Type::IO(k.clone()),
            Type::App(k, a, b) => Type::App(
                k.clone(),
                Rc::new(a.instantiate_many(tys)),
                Rc::new(b.instantiate_many(tys)),
            ),
            Type::RowNil => Type::RowNil,
            Type::RowCons(a, b, c) => {
                Type::mk_rowcons(a.clone(), b.instantiate_many(tys), c.instantiate_many(tys))
            }
            Type::HasField(a, b) => Type::mk_hasfield(a.clone(), b.instantiate_many(tys)),
            Type::Unit => Type::Unit,
            Type::Meta(k, n) => Type::Meta(k.clone(), *n),
        }
    }

    pub fn iter_metas(&self) -> TypeIterMetas {
        TypeIterMetas::One(self)
    }

    pub fn unwrap_variant(&self) -> Option<RowParts> {
        match self {
            Type::App(_, a, b) => match **a {
                Type::Variant(_) => Some(b.unwrap_rows()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn unwrap_rows(&self) -> RowParts {
        let mut current = self;
        let mut fields = Vec::new();
        loop {
            match current {
                Type::RowNil => return RowParts { fields, rest: None },
                Type::RowCons(field, ty, rest) => {
                    fields.push((field, ty));
                    current = rest;
                }
                _ => {
                    return RowParts {
                        fields,
                        rest: Some(current),
                    }
                }
            }
        }
    }

    pub fn unwrap_fatarrow(&self) -> Option<(&Type, &Type)> {
        match self {
            Type::App(_, a, out_ty) => match a.as_ref() {
                Type::App(_, c, in_ty) => match c.as_ref() {
                    Type::FatArrow(_) => Some((in_ty, out_ty)),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    /**
    ```
    use std::rc::Rc;
    use ipso_core::{CommonKinds, Type};
    use ipso_syntax::kind::Kind;

    let common_kinds = CommonKinds::default();

    let expected_constraint = Type::mk_hasfield(Rc::from("x"), Type::Var(Kind::Row, 0));
    let expected = (vec![&expected_constraint], &Type::Var(Kind::Type, 1));

    let ty = Type::mk_fatarrow(&common_kinds, Type::mk_hasfield(Rc::from("x"), Type::Var(Kind::Row, 0)), Type::Var(Kind::Type, 1));
    let actual = ty.unwrap_constraints();

    assert_eq!(expected, actual)
    ```
    */
    pub fn unwrap_constraints(&self) -> (Vec<&Type>, &Type) {
        fn flatten_constraints(ty: &Type) -> Vec<&Type> {
            match ty {
                Type::Constraints(cs) => cs.iter().flat_map(|c| flatten_constraints(c)).collect(),
                _ => vec![ty],
            }
        }

        pub fn go<'a>(constraints: &mut Vec<&'a Type>, ty: &'a Type) -> &'a Type {
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
}
pub enum TypeIterMetas<'a> {
    Zero,
    One(&'a Type),
    Many { items: Vec<&'a Type> },
}

impl<'a> TypeIterMetas<'a> {
    fn pop(&mut self) -> Option<&'a Type> {
        let (m_new_self, result) = match self {
            TypeIterMetas::Zero => (None, None),
            TypeIterMetas::One(a) => (Some(TypeIterMetas::Zero), Some(*a)),
            TypeIterMetas::Many { items } => {
                let result = items.pop();
                (None, result)
            }
        };
        if let Some(new_self) = m_new_self {
            *self = new_self
        }
        result
    }

    fn push(&mut self, item: &'a Type) {
        let m_new_self = match self {
            TypeIterMetas::Zero => Some(TypeIterMetas::One(item)),
            TypeIterMetas::One(a) => Some(TypeIterMetas::Many {
                items: vec![a, item],
            }),
            TypeIterMetas::Many { items } => {
                items.push(item);
                None
            }
        };
        if let Some(new_self) = m_new_self {
            *self = new_self;
        }
    }
}

impl<'a> Iterator for TypeIterMetas<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        fn step_kind(ty: &Type) -> Step<Type, usize> {
            match ty {
                Type::Name(_, _) => Step::Skip,
                Type::Var(_, _) => Step::Skip,
                Type::Bool => Step::Skip,
                Type::Int => Step::Skip,
                Type::Char => Step::Skip,
                Type::String => Step::Skip,
                Type::Bytes => Step::Skip,
                Type::Arrow(_) => Step::Skip,
                Type::FatArrow(_) => Step::Skip,
                Type::Constraints(cs) => Step::Continue(cs.iter().collect()),
                Type::Array(_) => Step::Skip,
                Type::Record(_) => Step::Skip,
                Type::Variant(_) => Step::Skip,
                Type::IO(_) => Step::Skip,
                Type::App(_, a, b) => Step::Continue2(a, b),
                Type::RowNil => Step::Skip,
                Type::RowCons(_, a, b) => Step::Continue2(a, b),
                Type::HasField(_, a) => Step::Continue1(a),
                Type::Unit => Step::Skip,
                Type::Meta(_, n) => Step::Yield(*n),
            }
        }

        let mut res = None;
        loop {
            match self.pop() {
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
                    Step::Continue1(item) => {
                        self.push(item);
                        continue;
                    }
                    Step::Continue2(item1, item2) => {
                        self.push(item2);
                        self.push(item1);
                        continue;
                    }
                    Step::Continue3(item1, item2, item3) => {
                        self.push(item3);
                        self.push(item2);
                        self.push(item1);
                        continue;
                    }
                    Step::Continue(items) => {
                        for item in items.iter().rev() {
                            self.push(item)
                        }
                        continue;
                    }
                },
            }
        }
        res
    }
}

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
        &mut self,
        f: &mut F,
    ) -> Result<(), E> {
        match self {
            Pattern::Name => Ok(()),
            Pattern::Record { names, .. } => {
                for name in names {
                    match name.subst_placeholder(f) {
                        Err(err) => return Err(err),
                        Ok(()) => {}
                    }
                }
                Ok(())
            }
            Pattern::Variant { tag } => Rc::make_mut(tag).subst_placeholder(f),
            Pattern::Wildcard => Ok(()),
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
        &mut self,
        f: &mut F,
    ) -> Result<(), E> {
        self.body.subst_placeholder(f)?;
        self.pattern.subst_placeholder(f)
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
        &mut self,
        f: &mut F,
    ) -> Result<(), E> {
        match self {
            StringPart::String(_) => Ok(()),
            StringPart::Expr(e) => e.subst_placeholder(f),
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
        &mut self,
        f: &mut F,
    ) -> Result<(), E> {
        match self {
            Expr::Var(_) => Ok(()),
            Expr::EVar(_) => Ok(()),
            Expr::Module(_, _) => Ok(()),
            Expr::Placeholder(v) => {
                let new_value = f(v)?;
                *self = new_value;
                Ok(())
            }
            Expr::Name(_) => Ok(()),
            Expr::Builtin(_) => Ok(()),
            Expr::App(a, b) => {
                Rc::make_mut(a).subst_placeholder(f)?;
                Rc::make_mut(b).subst_placeholder(f)
            }
            Expr::Lam { body, .. } => Rc::make_mut(body).subst_placeholder(f),
            Expr::Let { value, rest } => {
                Rc::make_mut(value).subst_placeholder(f)?;
                Rc::make_mut(rest).subst_placeholder(f)
            }
            Expr::True => Ok(()),
            Expr::False => Ok(()),
            Expr::IfThenElse(a, b, c) => {
                Rc::make_mut(a).subst_placeholder(f)?;
                Rc::make_mut(b).subst_placeholder(f)?;
                Rc::make_mut(c).subst_placeholder(f)
            }
            Expr::Int(_) => Ok(()),
            Expr::Binop(_, b, c) => {
                Rc::make_mut(b).subst_placeholder(f)?;
                Rc::make_mut(c).subst_placeholder(f)
            }
            Expr::Char(_) => Ok(()),
            Expr::String(parts) => {
                for part in parts {
                    match part.subst_placeholder(f) {
                        Err(err) => {
                            return Err(err);
                        }
                        Ok(()) => {}
                    }
                }
                Ok(())
            }
            Expr::Array(items) => {
                for item in items {
                    match item.subst_placeholder(f) {
                        Err(err) => {
                            return Err(err);
                        }
                        Ok(()) => {}
                    }
                }
                Ok(())
            }
            Expr::Extend(a, b, c) => {
                Rc::make_mut(a).subst_placeholder(f)?;
                Rc::make_mut(b).subst_placeholder(f)?;
                Rc::make_mut(c).subst_placeholder(f)
            }
            Expr::Record(items) => {
                for (a, b) in items {
                    match a.subst_placeholder(f).and_then(|()| b.subst_placeholder(f)) {
                        Err(err) => {
                            return Err(err);
                        }
                        Ok(()) => {}
                    }
                }
                Ok(())
            }
            Expr::Project(a, b) => {
                Rc::make_mut(a).subst_placeholder(f)?;
                Rc::make_mut(b).subst_placeholder(f)
            }
            Expr::Variant(a) => Rc::make_mut(a).subst_placeholder(f),
            Expr::Embed(a, b) => {
                Rc::make_mut(a).subst_placeholder(f)?;
                Rc::make_mut(b).subst_placeholder(f)
            }
            Expr::Case(a, bs) => {
                Rc::make_mut(a).subst_placeholder(f)?;
                for b in bs {
                    match b.subst_placeholder(f) {
                        Err(err) => {
                            return Err(err);
                        }
                        Ok(()) => {}
                    }
                }
                Ok(())
            }

            Expr::Unit => Ok(()),
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
    pub ty_vars: Vec<(Rc<str>, Kind)>,
    pub body: Type,
}

impl TypeSig {
    pub fn instantiate_many(&self, tys: &[Type]) -> TypeSig {
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
        kind: Kind,
    },
    Definition {
        name: String,
        sig: TypeSig,
        body: Rc<Expr>,
    },
    TypeAlias {
        name: String,
        args: Vec<Kind>,
        body: Type,
    },
    Class(ClassDeclaration),
    Instance {
        ty_vars: Vec<(Rc<str>, Kind)>,
        superclass_constructors: Vec<Expr>,
        assumes: Vec<Type>,
        head: Type,
        members: Vec<InstanceMember>,
    },
}

impl Declaration {
    pub fn get_bindings(&self, common_kinds: &CommonKinds) -> HashMap<String, Rc<Expr>> {
        match self {
            Declaration::BuiltinType { .. } => HashMap::new(),
            Declaration::Definition { name, sig: _, body } => {
                let mut map = HashMap::new();
                map.insert(name.clone(), body.clone());
                map
            }
            Declaration::TypeAlias { .. } => HashMap::new(),
            Declaration::Class(decl) => decl
                .get_bindings(common_kinds)
                .into_iter()
                .map(|(a, b)| (a, b.1))
                .collect(),
            Declaration::Instance { .. } => HashMap::new(),
        }
    }

    pub fn get_signatures(&self, common_kinds: &CommonKinds) -> HashMap<String, TypeSig> {
        match self {
            Declaration::BuiltinType { .. } => HashMap::new(),
            Declaration::Definition { name, sig, body: _ } => {
                let mut map = HashMap::new();
                map.insert(name.clone(), sig.clone());
                map
            }
            Declaration::TypeAlias { .. } => HashMap::new(),
            Declaration::Class(decl) => decl
                .get_bindings(common_kinds)
                .into_iter()
                .map(|(a, b)| (a, b.0))
                .collect(),
            Declaration::Instance { .. } => HashMap::new(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ClassDeclaration {
    pub supers: Vec<Type>,
    pub name: Rc<str>,
    pub args: Vec<(Rc<str>, Kind)>,
    pub members: Vec<ClassMember>,
}

impl ClassDeclaration {
    pub fn get_bindings(&self, common_kinds: &CommonKinds) -> HashMap<String, (TypeSig, Rc<Expr>)> {
        let supers_len = self.supers.len();

        let name_kind = self
            .args
            .iter()
            .rev()
            .fold(Kind::Constraint, |acc, (_, arg_kind)| {
                Kind::mk_arrow(arg_kind.clone(), acc)
            });
        let name_ty = Type::unsafe_mk_name(self.name.clone(), name_kind);

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
                let applied_type = self
                    .args // (adjustment..adjustment + self.args.len())
                    .iter()
                    .enumerate()
                    .fold(name_ty.clone(), |acc, (arg_index, (_, arg_kind))| {
                        let arg_ty = Type::unsafe_mk_var(adjustment + arg_index, arg_kind.clone());
                        Type::mk_app(acc, arg_ty)
                    });
                let sig = {
                    let mut body = member.sig.body.clone();
                    body = Type::mk_fatarrow(common_kinds, applied_type, body);

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
    pub fn get_bindings(&self, common_kinds: &CommonKinds) -> HashMap<String, Rc<Expr>> {
        let bindings: HashMap<String, Rc<Expr>> = HashMap::new();
        self.decls.iter().fold(bindings, |mut acc, decl| {
            acc.extend(decl.get_bindings(common_kinds).into_iter());
            acc
        })
    }

    pub fn get_signatures(&self, common_kinds: &CommonKinds) -> HashMap<String, TypeSig> {
        let signatures: HashMap<String, TypeSig> = HashMap::new();
        self.decls.iter().fold(signatures, |mut acc, decl| {
            acc.extend(decl.get_signatures(common_kinds).into_iter());
            acc
        })
    }
}
