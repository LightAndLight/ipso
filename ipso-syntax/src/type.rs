use ipso_util::iter::Step;
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

    pub fn iter_vars(&self) -> IterVars<A> {
        IterVars { items: vec![self] }
    }

    pub fn iter_metas(&self) -> TypeIterMetas<A> {
        TypeIterMetas::One(self)
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
            Type::Name(n) => s.push_str(n),
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

pub enum TypeIterMetas<'a, A> {
    Zero,
    One(&'a Type<A>),
    Many { items: Vec<&'a Type<A>> },
}

impl<'a, A> TypeIterMetas<'a, A> {
    fn pop(&mut self) -> Option<&'a Type<A>> {
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

    fn push(&mut self, item: &'a Type<A>) {
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
                Type::App(a, b) => Step::Continue2(a, b),
                Type::RowNil => Step::Skip,
                Type::RowCons(_, a, b) => Step::Continue2(a, b),
                Type::HasField(_, a) => Step::Continue1(a),
                Type::Unit => Step::Skip,
                Type::Meta(n) => Step::Yield(*n),
                Type::Cmd => Step::Skip,
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
                Type::App(a, b) => Step::Continue2(a, b),
                Type::RowNil => Step::Skip,
                Type::RowCons(_, a, b) => Step::Continue2(a, b),
                Type::HasField(_, a) => Step::Continue1(a),
                Type::Unit => Step::Skip,
                Type::Meta(_) => Step::Skip,
                Type::Cmd => Step::Skip,
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
                    Step::Continue1(item) => {
                        self.items.push(item);
                        continue;
                    }
                    Step::Continue2(item1, item2) => {
                        self.items.push(item2);
                        self.items.push(item1);
                        continue;
                    }
                    Step::Continue3(item1, item2, item3) => {
                        self.items.push(item3);
                        self.items.push(item2);
                        self.items.push(item1);
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
