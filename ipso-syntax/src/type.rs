use crate::Spanned;
use ipso_util::iter::Stack;
use std::fmt::Display;
use std::hash::Hash;
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type<A> {
    Name(Rc<str>),
    Var(A),
    Bool,
    Int,
    Char,
    String,
    Bytes,
    Arrow,
    Function(Rc<Spanned<Type<A>>>, Rc<Spanned<Type<A>>>),
    FatArrow,
    Constraints(Vec<Type<A>>),
    Array,
    Record,
    Variant,
    IO,
    App(Rc<Type<A>>, Rc<Type<A>>),
    RowNil,
    RowCons(Rc<str>, Rc<Type<A>>, Rc<Type<A>>),
    HasField(Rc<str>, Rc<Type<A>>),
    Unit,
    Meta(usize),
    Cmd,
}

pub struct RowParts<'a, A> {
    pub fields: Vec<(&'a Rc<str>, &'a Type<A>)>,
    pub rest: Option<&'a Type<A>>,
}

impl<A> Type<A> {
    pub fn unwrap_name(&self) -> Option<&Rc<str>> {
        match self {
            Type::Name(n) => Some(n),
            _ => None,
        }
    }

    /// ```
    /// use ipso_syntax::r#type::Type;
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
    /// use ipso_syntax::r#type::Type;
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
    /// use ipso_syntax::r#type::Type;
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
    /// use ipso_syntax::r#type::Type;
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
            Type::Function(a, b) => Type::Function(
                Rc::new(a.as_ref().map(|ty| ty.map(f))),
                Rc::new(b.as_ref().map(|ty| ty.map(f))),
            ),
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
            Type::Cmd => Type::Cmd,
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
            Type::Function(a, b) => Type::Function(
                Rc::new(a.as_ref().map(|ty| ty.subst(f))),
                Rc::new(b.as_ref().map(|ty| ty.subst(f))),
            ),
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
            Type::Cmd => Type::Cmd,
        }
    }

    pub fn iter_vars(&self) -> impl Iterator<Item = &A> {
        let mut stack = Stack::one(self);
        std::iter::from_fn(move || loop {
            match stack.pop() {
                None => {
                    return None;
                }
                Some(current) => match current {
                    Type::Name(_)
                    | Type::Bool
                    | Type::Int
                    | Type::Char
                    | Type::String
                    | Type::Bytes
                    | Type::Arrow
                    | Type::FatArrow
                    | Type::Array
                    | Type::Record
                    | Type::Variant
                    | Type::IO
                    | Type::RowNil
                    | Type::Unit
                    | Type::Meta(_)
                    | Type::Cmd => {}
                    Type::HasField(_, a) => {
                        stack.push(a);
                    }
                    Type::App(a, b) | Type::RowCons(_, a, b) => {
                        stack.push(b);
                        stack.push(a);
                    }
                    Type::Function(a, b) => {
                        stack.push(&b.item);
                        stack.push(&a.item);
                    }
                    Type::Constraints(cs) => {
                        stack.extend(cs.iter().rev());
                    }
                    Type::Var(n) => {
                        return Some(n);
                    }
                },
            }
        })
    }

    pub fn iter_metas(&self) -> impl Iterator<Item = usize> + '_ {
        let mut stack = Stack::one(self);
        std::iter::from_fn(move || loop {
            match stack.pop() {
                None => {
                    return None;
                }
                Some(current) => match current {
                    Type::Name(_)
                    | Type::Var(_)
                    | Type::Bool
                    | Type::Int
                    | Type::Char
                    | Type::String
                    | Type::Bytes
                    | Type::Arrow
                    | Type::FatArrow
                    | Type::Array
                    | Type::Record
                    | Type::Variant
                    | Type::IO
                    | Type::RowNil
                    | Type::Unit
                    | Type::Cmd => {}
                    Type::HasField(_, a) => {
                        stack.push(a);
                    }
                    Type::App(a, b) | Type::RowCons(_, a, b) => {
                        stack.push(b);
                        stack.push(a);
                    }
                    Type::Function(a, b) => {
                        stack.push(&b.item);
                        stack.push(&a.item);
                    }
                    Type::Constraints(cs) => {
                        stack.extend(cs.iter().rev());
                    }
                    Type::Meta(n) => {
                        return Some(*n);
                    }
                },
            }
        })
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

    pub fn unwrap_fatarrow(&self) -> Option<(&Type<A>, &Type<A>)> {
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

    pub fn unwrap_rows(&self) -> RowParts<A> {
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

    pub fn unwrap_record(&self) -> Option<RowParts<A>> {
        match self {
            Type::App(a, b) => match **a {
                Type::Record => Some(b.unwrap_rows()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn unwrap_variant(&self) -> Option<RowParts<A>> {
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
        Type::Name(Rc::from(s))
    }

    pub fn mk_rowcons(field: Rc<str>, a: Type<A>, b: Type<A>) -> Type<A> {
        Type::RowCons(field, Rc::new(a), Rc::new(b))
    }

    pub fn mk_hasfield(field: Rc<str>, rest: Type<A>) -> Type<A> {
        Type::HasField(field, Rc::new(rest))
    }

    pub fn mk_rows(fields: Vec<(Rc<str>, Type<A>)>, rest: Option<Type<A>>) -> Type<A> {
        let mut ty = rest.unwrap_or(Type::RowNil);
        for (field, a) in fields.into_iter().rev() {
            ty = Type::mk_rowcons(field, a, ty)
        }
        ty
    }

    pub fn mk_record(fields: Vec<(Rc<str>, Type<A>)>, rest: Option<Type<A>>) -> Type<A> {
        Type::mk_app(Type::Record, Type::mk_rows(fields, rest))
    }

    pub fn mk_variant(ctors: Vec<(Rc<str>, Type<A>)>, rest: Option<Type<A>>) -> Type<A> {
        Type::mk_app(Type::Variant, Type::mk_rows(ctors, rest))
    }

    pub fn render(&self) -> String
    where
        A: Display,
    {
        let mut s = String::new();

        if let Some(row_parts) = self.unwrap_record() {
            s.push('{');
            let mut fields_iter = row_parts.fields.iter();
            match fields_iter.next() {
                None => {}
                Some((first_field, first_ty)) => {
                    s.push(' ');
                    s.push_str(first_field);
                    s.push_str(" : ");
                    s.push_str(first_ty.render().as_str());
                    for (field, ty) in fields_iter {
                        s.push_str(", ");
                        s.push_str(field);
                        s.push_str(" : ");
                        s.push_str(ty.render().as_str());
                    }
                }
            }
            match row_parts.rest {
                None => {
                    if !row_parts.fields.is_empty() {
                        s.push(' ')
                    }
                }
                Some(ty) => {
                    if !row_parts.fields.is_empty() {
                        s.push_str(", ")
                    }
                    s.push_str(ty.render().as_str());
                    s.push(' ');
                }
            }
            s.push('}');
            return s;
        }

        if let Some(row_parts) = self.unwrap_variant() {
            s.push_str("(|");
            let mut fields_iter = row_parts.fields.iter();
            match fields_iter.next() {
                None => {}
                Some((first_field, first_ty)) => {
                    s.push(' ');
                    s.push_str(first_field);
                    s.push_str(" : ");
                    s.push_str(first_ty.render().as_str());
                    for (field, ty) in fields_iter {
                        s.push_str(", ");
                        s.push_str(field);
                        s.push_str(" : ");
                        s.push_str(ty.render().as_str());
                    }
                }
            }
            match row_parts.rest {
                None => {
                    if !row_parts.fields.is_empty() {
                        s.push(' ');
                    }
                }
                Some(ty) => {
                    if !row_parts.fields.is_empty() {
                        s.push(',')
                    }
                    s.push(' ');
                    s.push_str(ty.render().as_str());
                    s.push(' ');
                }
            }
            s.push_str("|)");
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
            if a.unwrap_fatarrow().is_some() {
                s.push('(')
            }
            s.push_str(a.render().as_str());
            if a.unwrap_fatarrow().is_some() {
                s.push(')')
            }
            s.push_str(" => ");
            s.push_str(b.render().as_str());
            return s;
        }

        match self {
            Type::Name(n) => s.push_str(n),
            Type::Var(n) => s.push_str(format!("{}", n).as_str()),
            Type::Bool => s.push_str("Bool"),
            Type::Int => s.push_str("Int"),
            Type::Char => s.push_str("Char"),
            Type::String => s.push_str("String"),
            Type::Bytes => s.push_str("Bytes"),
            Type::Arrow => s.push_str("(->)"),
            Type::Function(a, b) => {
                if matches!(a.as_ref().item, Type::Function(_, _)) {
                    s.push('(');
                }
                s.push_str(&a.as_ref().item.render());
                if matches!(a.as_ref().item, Type::Function(_, _)) {
                    s.push(')');
                }
                s.push_str(" -> ");
                s.push_str(&b.as_ref().item.render());
            }
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
                s.push_str(field);
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
            Type::Cmd => s.push_str("Cmd"),
        }
        s
    }
}
