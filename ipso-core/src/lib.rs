#[cfg(test)]
mod test;

use ipso_syntax::{self as syntax, kind::Kind, r#type, ModuleRef};
use ipso_util::iter::Stack;
use std::{
    cmp,
    collections::{HashMap, HashSet},
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
    Cmd,
    DebugRecordFields,
    DebugVariantCtor,
}

pub struct CommonKinds {
    pub type_to_type: Kind,
    pub type_to_type_to_type: Kind,
    pub constraint_to_type_to_type: Kind,
    pub row_to_type: Kind,
}

impl Default for CommonKinds {
    fn default() -> Self {
        let type_to_type = Kind::mk_arrow(&Kind::Type, &Kind::Type);
        Self {
            type_to_type_to_type: Kind::mk_arrow(&Kind::Type, &type_to_type),
            constraint_to_type_to_type: Kind::mk_arrow(&Kind::Constraint, &type_to_type),
            type_to_type,
            row_to_type: Kind::mk_arrow(&Kind::Row, &Kind::Type),
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
            Type::Cmd => r#type::Type::Cmd,
            Type::DebugRecordFields => r#type::Type::Name(Rc::from("DebugRecordFields")),
            Type::DebugVariantCtor => r#type::Type::Name(Rc::from("DebugVariantCtor")),
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
            Type::Cmd => Kind::Type,
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
            Type::DebugRecordFields => Kind::mk_arrow(&Kind::Row, &Kind::Constraint),
            Type::DebugVariantCtor => Kind::mk_arrow(&Kind::Row, &Kind::Constraint),
        }
    }

    pub fn unsafe_mk_name(name: Rc<str>, kind: Kind) -> Self {
        Type::Name(kind, name)
    }

    pub fn unsafe_mk_var(ix: usize, kind: Kind) -> Self {
        Type::Var(kind, ix)
    }

    pub fn mk_app(a: &Type, b: &Type) -> Self {
        Self::app(a.clone(), b.clone())
    }

    pub fn app(a: Type, b: Type) -> Self {
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

    pub fn mk_arrow(common_kinds: &CommonKinds, a: &Type, b: &Type) -> Self {
        Type::app(
            Type::app(Type::mk_arrow_ctor(common_kinds), a.clone()),
            b.clone(),
        )
    }

    pub fn arrow(common_kinds: &CommonKinds, a: Type, b: Type) -> Self {
        Type::app(Type::app(Type::mk_arrow_ctor(common_kinds), a), b)
    }

    pub fn mk_fatarrow_ctor(common_kinds: &CommonKinds) -> Self {
        Type::FatArrow(common_kinds.constraint_to_type_to_type.clone())
    }

    pub fn mk_fatarrow(common_kinds: &CommonKinds, a: Type, b: Type) -> Self {
        Type::app(Type::app(Type::mk_fatarrow_ctor(common_kinds), a), b)
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
        Type::app(
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
        Type::app(
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
            Type::Cmd => Type::Cmd,
            Type::DebugRecordFields => Type::DebugRecordFields,
            Type::DebugVariantCtor => Type::DebugVariantCtor,
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
            Type::Cmd => Type::Cmd,
            Type::DebugRecordFields => Type::DebugRecordFields,
            Type::DebugVariantCtor => Type::DebugVariantCtor,
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
            Type::Cmd => Type::Cmd,
            Type::DebugRecordFields => Type::DebugRecordFields,
            Type::DebugVariantCtor => Type::DebugVariantCtor,
        }
    }

    pub fn iter_metas(&self) -> impl Iterator<Item = usize> + '_ {
        let mut stack = Stack::one(self);
        std::iter::from_fn(move || loop {
            match stack.pop() {
                None => {
                    return None;
                }
                Some(current) => match current {
                    Type::Name(_, _)
                    | Type::Var(_, _)
                    | Type::Bool
                    | Type::Int
                    | Type::Char
                    | Type::String
                    | Type::Bytes
                    | Type::Arrow(_)
                    | Type::Array(_)
                    | Type::Record(_)
                    | Type::Variant(_)
                    | Type::IO(_)
                    | Type::FatArrow(_)
                    | Type::RowNil
                    | Type::Unit
                    | Type::Cmd
                    | Type::DebugRecordFields
                    | Type::DebugVariantCtor => {}
                    Type::HasField(_, a) => {
                        stack.push(a);
                    }
                    Type::App(_, a, b) => {
                        stack.push(b);
                        stack.push(a);
                    }
                    Type::RowCons(_, a, b) => {
                        stack.push(b);
                        stack.push(a);
                    }
                    Type::Constraints(cs) => {
                        stack.extend(cs.iter().rev());
                    }
                    Type::Meta(_, n) => {
                        return Some(*n);
                    }
                },
            }
        })
    }

    pub fn iter_vars(&self) -> impl Iterator<Item = usize> + '_ {
        let mut stack = Stack::one(self);
        std::iter::from_fn(move || loop {
            match stack.pop() {
                None => {
                    return None;
                }
                Some(current) => match current {
                    Type::Bool
                    | Type::Int
                    | Type::Char
                    | Type::String
                    | Type::Bytes
                    | Type::Arrow(_)
                    | Type::FatArrow(_)
                    | Type::Array(_)
                    | Type::Record(_)
                    | Type::Variant(_)
                    | Type::IO(_)
                    | Type::RowNil
                    | Type::Unit
                    | Type::Meta(_, _)
                    | Type::Cmd
                    | Type::DebugRecordFields
                    | Type::DebugVariantCtor
                    | Type::Name(_, _) => {}
                    Type::HasField(_, a) => {
                        stack.push(a);
                    }
                    Type::App(_, a, b) => {
                        stack.push(b);
                        stack.push(a);
                    }
                    Type::RowCons(_, a, b) => {
                        stack.push(b);
                        stack.push(a);
                    }
                    Type::Constraints(cs) => {
                        stack.extend(cs.iter().rev());
                    }
                    Type::Var(_, v) => {
                        return Some(*v);
                    }
                },
            }
        })
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
                Type::Constraints(cs) => cs.iter().flat_map(flatten_constraints).collect(),
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pattern<E> {
    Name,
    Record { names: Vec<E>, rest: bool },
    Variant { tag: Rc<E> },
    Char(char),
    Int(i32),
    String(Rc<str>),
    Wildcard,
}

impl Pattern<Expr> {
    pub fn bound_vars(&self) -> usize {
        match self {
            Pattern::Name => 1,
            Pattern::Record { names, rest } => names.len() + if *rest { 1 } else { 0 },
            Pattern::Variant { tag: _ } => 1,
            Pattern::Char(_) | Pattern::Int(_) | Pattern::String(_) | Pattern::Wildcard => 0,
        }
    }

    pub fn map_expr<F: Fn(&Expr) -> Expr>(&self, f: F) -> Self {
        match self {
            Pattern::Name => Pattern::Name,
            Pattern::Record { names, rest } => Pattern::Record {
                names: names.iter().map(f).collect(),
                rest: *rest,
            },
            Pattern::Variant { tag } => Pattern::mk_variant(f(tag)),
            Pattern::Char(c) => Pattern::Char(*c),
            Pattern::Int(n) => Pattern::Int(*n),
            Pattern::String(s) => Pattern::String(s.clone()),
            Pattern::Wildcard => Pattern::Wildcard,
        }
    }

    pub fn mk_variant(tag: Expr) -> Pattern<Expr> {
        Pattern::Variant { tag: Rc::new(tag) }
    }

    pub fn subst_placeholder<E, F: FnMut(&Placeholder) -> Result<Expr, E>>(
        &mut self,
        f: &mut F,
    ) -> Result<(), E> {
        match self {
            Pattern::Name => Ok(()),
            Pattern::Record { names, .. } => names
                .iter_mut()
                .try_for_each(|name| name.subst_placeholder(f)),
            Pattern::Variant { tag } => Rc::make_mut(tag).subst_placeholder(f),
            Pattern::Char(_) => Ok(()),
            Pattern::Int(_) => Ok(()),
            Pattern::String(_) => Ok(()),
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
            Pattern::Char(c) => Pattern::Char(*c),
            Pattern::Int(n) => Pattern::Int(*n),
            Pattern::String(s) => Pattern::String(s.clone()),
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
            Pattern::Char(c) => Pattern::Char(*c),
            Pattern::Int(n) => Pattern::Int(*n),
            Pattern::String(s) => Pattern::String(s.clone()),
            Pattern::Wildcard => Pattern::Wildcard,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Branch<E> {
    pub pattern: Pattern<E>,
    pub body: E,
}

impl Branch<Expr> {
    pub fn map_expr<F: Fn(&Expr) -> Expr>(&self, f: F) -> Branch<Expr> {
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
                        Pattern::Char(_) => 0,
                        Pattern::Int(_) => 0,
                        Pattern::String(_) => 0,
                        // TODO: should this be 0?
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
                        Pattern::Char(_) => 0,
                        Pattern::Int(_) => 0,
                        Pattern::String(_) => 0,
                        Pattern::Wildcard => 0,
                    },
                ev,
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StringPart<E> {
    String(String),
    Expr(E),
}

impl StringPart<Expr> {
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

impl<A> StringPart<A> {
    pub fn map_expr<F: Fn(&A) -> A>(&self, f: F) -> Self {
        match self {
            StringPart::String(s) => StringPart::String(s.clone()),
            StringPart::Expr(e) => StringPart::Expr(f(e)),
        }
    }
}

impl<A, B> From<A> for StringPart<B>
where
    String: From<A>,
{
    fn from(s: A) -> Self {
        StringPart::String(String::from(s))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Builtin {
    MapIO,
    Pure,
    BindIO,
    Trace,
    ToUtf8,
    Println,
    Print,
    Readln,
    EqInt,
    CompareInt,
    IntToString,
    FoldlArray,
    EqArray,
    CompareArray,
    GenerateArray,
    LengthArray,
    IndexArray,
    SliceArray,
    EqString,
    CompareString,
    FilterString,
    EqChar,
    CompareChar,
    StringSplit,
    StringSplitc,
    JoinString,
    FoldlString,
    SnocArray,
    Run,
    EqBool,
    Lines,
    ShowCmd,
    DebugCmd,
    FlatMap,
    MapArray,
    CharToString,
    DebugString,
    ArrayUnfoldr,
    IntMod,
    PathExists,
    EnvArgs,
    EnvGetvar,
    EnvSetvar,
    ExitSuccess,
    ExitFailure,
    ExitWith,
    CmdRead,
    FileRead,
    FileWrite,
    FileAppend,
    ArrayEach_,
    CmdEachline_,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct EVar(pub usize);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Placeholder(pub usize);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub enum Binop {
    Add,
    Multiply,
    Subtract,
    Divide,

    Append,

    Or,
    And,

    LApply,
    RApply,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CmdPart<E> {
    Literal(Rc<str>),
    Expr(E),
    MultiPart {
        first: StringPart<E>,
        second: StringPart<E>,
        rest: Vec<StringPart<E>>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Name {
    Evidence(Rc<str>),
    Definition(Rc<str>),
}

impl Name {
    pub fn definition<T: Into<Rc<str>>>(name: T) -> Self {
        Self::Definition(name.into())
    }

    pub fn evidence<T: Into<Rc<str>>>(name: T) -> Self {
        Self::Evidence(name.into())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Var(usize),
    EVar(EVar),
    Placeholder(Placeholder),
    Name(Name),
    Module {
        /// A reference to a module.
        id: ModuleRef,

        /**
        A chain of submodule accessors.

        e.g. `module.submodule1.submodule2`
        */
        path: Vec<String>,

        /**
        The referenced item.

        e.g. `module.submodule.item`
        */
        item: Name,
    },
    Builtin(Builtin),

    App(Rc<Expr>, Rc<Expr>),
    Lam {
        arg: bool,
        body: Rc<Expr>,
    },

    Let {
        value: Rc<Expr>,
        rest: Rc<Expr>,
    },

    True,
    False,
    IfThenElse(Rc<Expr>, Rc<Expr>, Rc<Expr>),

    Int(i32),

    Binop(Binop, Rc<Expr>, Rc<Expr>),

    Char(char),

    String(Vec<StringPart<Expr>>),

    Array(Vec<Expr>),

    // TODO: use struct arguments - { index, value, record }
    Extend(Rc<Expr>, Rc<Expr>, Rc<Expr>),
    Record(Vec<(Expr, Expr)>),
    // TODO: use struct arguments - { record, index }
    Project(Rc<Expr>, Rc<Expr>),

    // TODO: use struct arguments - { tag }
    Variant(Rc<Expr>),
    // TODO: use struct arguments - { tag, variant }
    Embed(Rc<Expr>, Rc<Expr>),
    Case(Rc<Expr>, Vec<Branch<Expr>>),
    Unit,

    Cmd(Vec<CmdPart<Expr>>),
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

    pub fn app(a: Rc<Expr>, b: Rc<Expr>) -> Expr {
        match a.as_ref() {
            Expr::Lam { arg, body } => {
                if *arg {
                    // this potentially increases the number of redexes in the term.
                    // todo: bind the arg with a let?
                    body.instantiate(&b)
                } else {
                    body.as_ref().clone()
                }
            }
            _ => Expr::App(a, b),
        }
    }

    pub fn mk_lam(arg: bool, body: Expr) -> Expr {
        Expr::Lam {
            arg,
            body: Rc::new(body),
        }
    }

    pub fn mk_let(value: Expr, rest: Expr) -> Expr {
        Expr::Let {
            value: Rc::new(value),
            rest: Rc::new(rest),
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

    pub fn mk_binop(op: Binop, a: Expr, b: Expr) -> Expr {
        if op == Binop::Add {
            if let (Expr::Int(a), Expr::Int(b)) = (&a, &b) {
                return Expr::Int(*a + *b);
            }
        }
        Expr::Binop(op, Rc::new(a), Rc::new(b))
    }

    pub fn mk_binop_l(op: Binop, a: Expr, b: Rc<Expr>) -> Expr {
        if op == Binop::Add {
            if let (Expr::Int(a), Expr::Int(b)) = (&a, b.as_ref()) {
                return Expr::Int(*a + *b);
            }
        }
        Expr::Binop(op, Rc::new(a), b)
    }

    pub fn mk_case(expr: Expr, branches: Vec<Branch<Expr>>) -> Expr {
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
                Expr::Module { id, path, item } => Expr::Module {
                    id: *id,
                    path: path.clone(),
                    item: item.clone(),
                },
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
                            Pattern::Char(_) => b.map_expr(|e| go(e, f)),
                            Pattern::Int(_) => b.map_expr(|e| go(e, f)),
                            Pattern::String(_) => b.map_expr(|e| go(e, f)),
                            Pattern::Wildcard => b.map_expr(|e| go(e, f)),
                        })
                        .collect(),
                ),
                Expr::Unit => Expr::Unit,
                Expr::Cmd(parts) => Expr::Cmd(
                    parts
                        .iter()
                        .map(|part| match part {
                            CmdPart::Literal(value) => CmdPart::Literal(value.clone()),
                            CmdPart::Expr(expr) => CmdPart::Expr(go(expr, f)),
                            CmdPart::MultiPart {
                                first,
                                second,
                                rest,
                            } => CmdPart::MultiPart {
                                first: first.map_expr(|expr| go(expr, f)),
                                second: second.map_expr(|expr| go(expr, f)),
                                rest: rest
                                    .iter()
                                    .map(|string_part| string_part.map_expr(|e| go(e, f)))
                                    .collect(),
                            },
                        })
                        .collect(),
                ),
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
            Expr::Module { id, path, item } => Expr::Module {
                id: *id,
                path: path.clone(),
                item: item.clone(),
            },
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
            Expr::Cmd(parts) => Expr::Cmd(
                parts
                    .iter()
                    .map(|part| match part {
                        CmdPart::Literal(value) => CmdPart::Literal(value.clone()),
                        CmdPart::Expr(expr) => CmdPart::Expr(expr.__instantiate(depth, val)),
                        CmdPart::MultiPart {
                            first,
                            second,
                            rest,
                        } => CmdPart::MultiPart {
                            first: first.__instantiate(depth, val),
                            second: second.__instantiate(depth, val),
                            rest: rest
                                .iter()
                                .map(|part| part.__instantiate(depth, val))
                                .collect(),
                        },
                    })
                    .collect(),
            ),
        }
    }

    pub fn subst_placeholder<E, F: FnMut(&Placeholder) -> Result<Expr, E>>(
        &mut self,
        f: &mut F,
    ) -> Result<(), E> {
        match self {
            Expr::Var(_) => Ok(()),
            Expr::EVar(_) => Ok(()),
            Expr::Module { .. } => Ok(()),
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
            Expr::String(parts) => parts
                .iter_mut()
                .try_for_each(|part| part.subst_placeholder(f)),
            Expr::Array(items) => items
                .iter_mut()
                .try_for_each(|item| item.subst_placeholder(f)),
            Expr::Extend(a, b, c) => {
                Rc::make_mut(a).subst_placeholder(f)?;
                Rc::make_mut(b).subst_placeholder(f)?;
                Rc::make_mut(c).subst_placeholder(f)
            }
            Expr::Record(items) => items.iter_mut().try_for_each(|(a, b)| {
                a.subst_placeholder(f).and_then(|()| b.subst_placeholder(f))
            }),
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
                bs.iter_mut().try_for_each(|b| b.subst_placeholder(f))
            }

            Expr::Unit => Ok(()),

            Expr::Cmd(parts) => parts.iter_mut().try_for_each(|part| match part {
                CmdPart::Literal(_) => Ok(()),
                CmdPart::Expr(expr) => expr.subst_placeholder(f),
                CmdPart::MultiPart {
                    first,
                    second,
                    rest,
                } => {
                    first.subst_placeholder(f)?;
                    second.subst_placeholder(f)?;
                    rest.iter_mut()
                        .try_for_each(|string_part| string_part.subst_placeholder(f))?;
                    Ok(())
                }
            }),
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
            Expr::Module { id, path, item } => Expr::Module {
                id: *id,
                path: path.clone(),
                item: item.clone(),
            },
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
            Expr::Cmd(parts) => Expr::Cmd(
                parts
                    .iter()
                    .map(|part| match part {
                        CmdPart::Literal(value) => CmdPart::Literal(value.clone()),
                        CmdPart::Expr(expr) => CmdPart::Expr(expr.__abstract_evar(depth, ev)),
                        CmdPart::MultiPart {
                            first,
                            second,
                            rest,
                        } => CmdPart::MultiPart {
                            first: first.__abstract_evar(depth, ev),
                            second: second.__abstract_evar(depth, ev),
                            rest: rest
                                .iter()
                                .map(|string_part| string_part.__abstract_evar(depth, ev))
                                .collect(),
                        },
                    })
                    .collect(),
            ),
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
    pub fn iter_evars(&self) -> impl Iterator<Item = &EVar> {
        let mut stack = Stack::one(self);
        std::iter::from_fn(move || loop {
            match stack.pop() {
                None => {
                    return None;
                }
                Some(current) => match current {
                    Expr::Var(_)
                    | Expr::Placeholder(_)
                    | Expr::Name(_)
                    | Expr::Module { .. }
                    | Expr::True
                    | Expr::False
                    | Expr::Int(_)
                    | Expr::Char(_)
                    | Expr::Unit
                    | Expr::Builtin(_) => {}

                    Expr::Lam { arg: _, body: a } | Expr::Variant(a) => {
                        stack.push(a);
                    }

                    Expr::App(a, b)
                    | Expr::Let {
                        value: a, rest: b, ..
                    }
                    | Expr::Binop(_, a, b)
                    | Expr::Project(a, b)
                    | Expr::Embed(a, b) => {
                        stack.push(b);
                        stack.push(a);
                    }

                    Expr::IfThenElse(a, b, c) | Expr::Extend(a, b, c) => {
                        stack.push(c);
                        stack.push(b);
                        stack.push(a);
                    }

                    Expr::String(a) => {
                        stack.extend(a.iter().rev().filter_map(|string_part| match string_part {
                            StringPart::String(_) => None,
                            StringPart::Expr(e) => Some(e),
                        }));
                    }
                    Expr::Array(xs) => {
                        stack.extend(xs);
                    }
                    Expr::Record(xs) => {
                        xs.iter().rev().for_each(|(a, b)| {
                            stack.push(b);
                            stack.push(a);
                        });
                    }
                    Expr::Case(a, b) => {
                        b.iter().rev().for_each(|branch| {
                            stack.push(&branch.body);
                            match &branch.pattern {
                                Pattern::Name
                                | Pattern::Char(_)
                                | Pattern::Int(_)
                                | Pattern::String(_)
                                | Pattern::Wildcard => {}
                                Pattern::Variant { tag } => {
                                    stack.push(tag);
                                }
                                Pattern::Record { names, .. } => {
                                    stack.extend(names.iter().rev());
                                }
                            };
                        });
                        stack.push(a);
                    }
                    Expr::Cmd(parts) => {
                        parts.iter().rev().for_each(|cmd_part| match cmd_part {
                            CmdPart::Literal(_) => {}
                            CmdPart::Expr(expr) => {
                                stack.push(expr);
                            }
                            CmdPart::MultiPart {
                                first,
                                second,
                                rest,
                            } => {
                                rest.iter().rev().for_each(|string_part| match string_part {
                                    StringPart::String(_) => {}
                                    StringPart::Expr(expr) => {
                                        stack.push(expr);
                                    }
                                });

                                match second {
                                    StringPart::String(_) => {}
                                    StringPart::Expr(expr) => {
                                        stack.push(expr);
                                    }
                                }

                                match first {
                                    StringPart::String(_) => {}
                                    StringPart::Expr(expr) => {
                                        stack.push(expr);
                                    }
                                }
                            }
                        });
                    }
                    Expr::EVar(a) => {
                        return Some(a);
                    }
                },
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeSig {
    pub ty_vars: Vec<(Rc<str>, Kind)>,
    pub body: Type,
}

impl TypeSig {
    pub fn new(ty_vars: Vec<(Rc<str>, Kind)>, body: Type) -> Self {
        debug_assert!(
            ty_vars
                .iter()
                .map(|(name, _)| name.as_ref())
                .collect::<HashSet<&str>>()
                .len()
                == ty_vars.len(),
            "duplicate type variables in {:?}",
            ty_vars
        );

        TypeSig { ty_vars, body }
    }

    pub fn instantiate_many(&self, tys: &[Type]) -> TypeSig {
        let ty_vars = if tys.len() >= self.ty_vars.len() {
            Vec::new()
        } else {
            self.ty_vars.iter().skip(tys.len()).cloned().collect()
        };
        TypeSig::new(ty_vars, self.body.instantiate_many(tys))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ClassMember {
    pub name: String,
    pub sig: TypeSig,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Declaration {
    BuiltinType {
        name: String,
        kind: Kind,
    },
    Definition {
        name: Rc<str>,
        sig: TypeSig,
        body: Rc<Expr>,
    },
    TypeAlias {
        name: String,
        args: Vec<Kind>,
        body: Type,
    },
    Class(ClassDeclaration),
    Evidence {
        name: Rc<str>,
        body: Rc<Expr>,
    },
    Instance {
        ty_vars: Vec<(Rc<str>, Kind)>,
        assumes: Vec<Type>,
        head: Type,
        evidence: Rc<str>,
    },
    Module {
        name: String,
        decls: Vec<Rc<Declaration>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Binding {
    Expr(Rc<Expr>),
    Module(HashMap<Name, Binding>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Signature {
    TypeSig(TypeSig),
    Module(HashMap<String, Signature>),
}

impl Declaration {
    pub fn get_bindings(&self, common_kinds: &CommonKinds) -> HashMap<Name, Binding> {
        match self {
            Declaration::BuiltinType { .. } => HashMap::new(),
            Declaration::Definition { name, sig: _, body } => {
                let mut map = HashMap::new();
                map.insert(Name::Definition(name.clone()), Binding::Expr(body.clone()));
                map
            }
            Declaration::Evidence { name, body } => {
                let mut map = HashMap::new();
                map.insert(Name::Evidence(name.clone()), Binding::Expr(body.clone()));
                map
            }
            Declaration::TypeAlias { .. } => HashMap::new(),
            Declaration::Class(decl) => decl
                .get_bindings(common_kinds)
                .into_iter()
                .map(|(a, b)| (Name::Definition(Rc::from(a.as_str())), Binding::Expr(b.1)))
                .collect(),
            Declaration::Instance { .. } => HashMap::new(),
            Declaration::Module { name, decls } => HashMap::from([(
                Name::Definition(Rc::from(name.as_ref())),
                Binding::Module(
                    decls
                        .iter()
                        .flat_map(|decl| decl.get_bindings(common_kinds).into_iter())
                        .collect(),
                ),
            )]),
        }
    }

    pub fn get_signatures(&self, common_kinds: &CommonKinds) -> HashMap<String, Signature> {
        match self {
            Declaration::BuiltinType { .. } => HashMap::new(),
            Declaration::Definition { name, sig, body: _ } => {
                let mut map = HashMap::new();
                map.insert(String::from(name.as_ref()), Signature::TypeSig(sig.clone()));
                map
            }
            Declaration::Evidence { .. } => HashMap::new(),
            Declaration::TypeAlias { .. } => HashMap::new(),
            Declaration::Class(decl) => decl
                .get_bindings(common_kinds)
                .into_iter()
                .map(|(a, b)| (a, Signature::TypeSig(b.0)))
                .collect(),
            Declaration::Instance { .. } => HashMap::new(),
            Declaration::Module { name, decls } => HashMap::from([(
                name.clone(),
                Signature::Module(
                    decls
                        .iter()
                        .flat_map(|decl| decl.get_signatures(common_kinds))
                        .collect(),
                ),
            )]),
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
                Kind::mk_arrow(arg_kind, &acc)
            });
        let name_ty = Type::unsafe_mk_name(self.name.clone(), name_kind);

        self.members
            .iter()
            .enumerate()
            .map(|(ix, member)| {
                /*
                Generates a type signature for each class member.

                For example, in

                ```
                class X a where
                  x : a -> b -> ()
                ```

                the signature `x : forall a b. X a => a -> b -> ()` is generated.

                The `X a` constraint in the signature consists of type class name applied
                to all its variables. When constructing the constraint, each variable
                needs to account for the variables bound by the member signature.
                */
                let offset = member.sig.ty_vars.len();
                let constraint_type = self.args.iter().rev().enumerate().fold(
                    name_ty.clone(),
                    |acc, (arg_index, (_, arg_kind))| {
                        Type::app(
                            acc,
                            Type::unsafe_mk_var(arg_index + offset, arg_kind.clone()),
                        )
                    },
                );

                let sig = {
                    let ty_vars = {
                        let mut ty_vars = self.args.clone();
                        ty_vars.extend(member.sig.ty_vars.iter().cloned());
                        ty_vars
                    };

                    let body =
                        Type::mk_fatarrow(common_kinds, constraint_type, member.sig.body.clone());

                    TypeSig::new(ty_vars, body)
                };
                let body: Rc<Expr> = Rc::new(Expr::mk_lam(
                    true,
                    Expr::mk_project(Expr::Var(0), Expr::Int(supers_len as i32 + ix as i32)),
                ));

                (member.name.clone(), (sig, body))
            })
            .collect()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Module {
    pub decls: Vec<Declaration>,
}

impl Module {
    pub fn get_bindings(&self, common_kinds: &CommonKinds) -> HashMap<Name, Binding> {
        let bindings: HashMap<Name, Binding> = HashMap::new();
        self.decls.iter().fold(bindings, |mut acc, decl| {
            acc.extend(decl.get_bindings(common_kinds).into_iter());
            acc
        })
    }

    pub fn get_signatures(&self, common_kinds: &CommonKinds) -> HashMap<String, Signature> {
        let signatures: HashMap<String, Signature> = HashMap::new();
        self.decls.iter().fold(signatures, |mut acc, decl| {
            acc.extend(decl.get_signatures(common_kinds).into_iter());
            acc
        })
    }
}
