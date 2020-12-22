use crate::core;
use crate::diagnostic;
use crate::syntax;
use crate::syntax::Spanned;
use std::collections::HashMap;
use std::collections::HashSet;

#[cfg(test)]
mod test;

#[derive(Clone, Debug, PartialEq, Eq)]
struct ContextEntry {
    index: usize,
    ty: syntax::Type,
}

#[derive(Debug, PartialEq, Eq)]
struct Context(HashMap<String, Vec<ContextEntry>>);

impl Context {
    fn new() -> Self {
        Context(HashMap::new())
    }

    fn lookup(&self, name: &String) -> Option<ContextEntry> {
        self.0
            .get(name)
            .and_then(|entries| entries.last().map(|entry| entry.clone()))
    }

    fn insert(&mut self, vars: &Vec<(&String, syntax::Type)>) {
        debug_assert!(
            {
                let mut seen: HashSet<&String> = HashSet::new();
                vars.iter().fold(true, |acc, el: &(&String, syntax::Type)| {
                    let acc = acc && !seen.contains(&el.0);
                    seen.insert(&el.0);
                    acc
                })
            },
            "duplicate name in {:?}",
            vars
        );
        let num_vars = vars.len();
        for (_, entries) in &mut self.0 {
            for mut entry in entries {
                entry.index += num_vars;
            }
        }
        for (index, (var, ty)) in vars.iter().rev().enumerate() {
            match self.0.get_mut(*var) {
                None => {
                    self.0.insert(
                        (*var).clone(),
                        vec![ContextEntry {
                            index,
                            ty: ty.clone(),
                        }],
                    );
                }
                Some(entries) => {
                    entries.push(ContextEntry {
                        index,
                        ty: ty.clone(),
                    });
                }
            };
        }
    }

    fn delete(&mut self, vars: &Vec<&String>) {
        debug_assert!(
            {
                let mut seen: HashSet<&String> = HashSet::new();
                vars.iter().fold(true, |acc, el: &&String| {
                    let acc = acc && !seen.contains(el);
                    seen.insert(&el);
                    acc
                })
            },
            "duplicate name in {:?}",
            vars
        );
        let num_vars = vars.len();
        for var in vars {
            match self.0.get_mut(*var) {
                Some(entries) if entries.len() > 0 => {
                    entries.pop();
                }
                _ => {}
            }
        }
        for item in &mut self.0 {
            for entry in item.1 {
                entry.index -= num_vars;
            }
        }
    }
}

pub struct Typechecker {
    kind_solutions: Vec<Option<Kind>>,
    type_solutions: Vec<Option<syntax::Type>>,
    typevar_kinds: Vec<Kind>,
    context: Context,
    position: Option<usize>,
}

macro_rules! with_position {
    ($self:expr, $pos:expr, $val:expr) => {{
        let old = $self.position;
        $self.position = Some($pos);
        let res = $val;
        $self.position = old;
        res
    }};
}

#[derive(PartialEq, Eq, Debug)]
pub enum TypeError {
    DuplicateArgument {
        pos: usize,
        name: String,
    },
    NotInScope {
        pos: usize,
        name: String,
    },
    KindMismatch {
        pos: usize,
        expected: Kind,
        actual: Kind,
    },
    TypeMismatch {
        pos: usize,
        expected: syntax::Type,
        actual: syntax::Type,
    },
}

impl syntax::Pattern {
    fn get_arg_names(&self) -> Vec<&Spanned<String>> {
        let mut arg_names = Vec::new();
        match self {
            syntax::Pattern::Name(n) => {
                arg_names.push(n);
            }
            syntax::Pattern::Record { names, rest } => {
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
            syntax::Pattern::Variant { name: _, arg } => {
                arg_names.push(arg);
            }
            syntax::Pattern::Wildcard => {}
        }
        arg_names
    }
}

impl TypeError {
    pub fn position(&self) -> usize {
        match self {
            TypeError::KindMismatch {
                pos,
                expected: _,
                actual: _,
            } => *pos,
            TypeError::TypeMismatch {
                pos,
                expected: _,
                actual: _,
            } => *pos,
            TypeError::NotInScope { pos, name: _ } => *pos,
            TypeError::DuplicateArgument { pos, name: _ } => *pos,
        }
    }

    pub fn message(&self) -> String {
        match self {
            TypeError::KindMismatch {
                pos: _,
                expected,
                actual,
            } => {
                let mut message = String::from("expected kind ");
                message.push('"');
                message.push_str(expected.render().as_str());
                message.push('"');
                message.push_str(", got kind ");
                message.push('"');
                message.push_str(actual.render().as_str());
                message.push('"');
                message
            }
            TypeError::TypeMismatch {
                pos: _,
                expected,
                actual,
            } => {
                let mut message = String::from("expected type ");
                message.push('"');
                message.push_str(expected.render().as_str());
                message.push('"');
                message.push_str(", got type ");
                message.push('"');
                message.push_str(actual.render().as_str());
                message.push('"');
                message
            }
            TypeError::NotInScope { pos: _, name: _ } => String::from("not in scope"),
            TypeError::DuplicateArgument { pos: _, name: _ } => String::from("duplicate argument"),
        }
    }

    pub fn report(&self, diagnostic: &mut diagnostic::Diagnostic) {
        diagnostic.item(diagnostic::Item {
            pos: self.position(),
            message: self.message(),
        })
    }
}

#[derive(PartialEq, Eq, Debug)]
enum Rope<'a, A> {
    Empty,
    Leaf(&'a [A]),
    Branch(usize, Box<Rope<'a, A>>, Box<Rope<'a, A>>),
}

impl<'a, A> Rope<'a, A> {
    pub fn from_vec(v: &'a Vec<A>) -> Rope<'a, A> {
        Rope::Leaf(v)
    }

    pub fn size(&self) -> usize {
        match self {
            Rope::Empty => 0,
            Rope::Leaf(slice) => slice.len(),
            Rope::Branch(size, _, _) => *size,
        }
    }

    pub fn delete_first<P: Fn(&A) -> bool>(
        self,
        predicate: &P,
    ) -> Result<Rope<'a, A>, Rope<'a, A>> {
        match self {
            Rope::Empty => Err(Rope::Empty),
            Rope::Leaf(slice) => match slice.iter().position(predicate) {
                Some(ix) => {
                    let (prefix, suffix) = slice.split_at(ix);
                    let suffix = &suffix[1..];
                    let prefix_len = prefix.len();
                    let suffix_len = suffix.len();
                    if prefix_len == 0 {
                        Ok(Rope::Leaf(suffix))
                    } else if suffix_len == 0 {
                        Ok(Rope::Leaf(prefix))
                    } else {
                        Ok(Rope::Branch(
                            prefix_len + suffix_len,
                            Box::new(Rope::Leaf(prefix)),
                            Box::new(Rope::Leaf(suffix)),
                        ))
                    }
                }
                None => Err(self),
            },
            Rope::Branch(size, mut left, mut right) => match left.delete_first(predicate) {
                Err(new_left) => {
                    *left = new_left;
                    match right.delete_first(predicate) {
                        Err(new_right) => {
                            *right = new_right;
                            Err(Rope::Branch(size, left, right))
                        }
                        Ok(new_right) => {
                            *right = new_right;
                            let left_size = left.size();
                            let right_size = right.size();
                            if right.size() == 0 {
                                Ok(*left)
                            } else {
                                Ok(Rope::Branch(left_size + right_size, left, right))
                            }
                        }
                    }
                }
                Ok(new_left) => {
                    *left = new_left;
                    let left_size = left.size();
                    let right_size = right.size();
                    if left.size() == 0 {
                        Ok(*right)
                    } else {
                        Ok(Rope::Branch(left_size + right_size, left, right))
                    }
                }
            },
        }
    }

    pub fn delete(self, ix: usize) -> Result<Rope<'a, A>, Rope<'a, A>> {
        match self {
            Rope::Empty => Err(Rope::Empty),
            Rope::Leaf(slice) => {
                if ix < slice.len() {
                    let (prefix, suffix) = slice.split_at(ix);
                    let suffix = &suffix[1..];
                    let prefix_len = prefix.len();
                    let suffix_len = suffix.len();
                    if prefix_len == 0 {
                        if suffix_len == 0 {
                            Ok(Rope::Empty)
                        } else {
                            Ok(Rope::Leaf(suffix))
                        }
                    } else if suffix_len == 0 {
                        Ok(Rope::Leaf(prefix))
                    } else {
                        Ok(Rope::Branch(
                            prefix_len + suffix_len,
                            Box::new(Rope::Leaf(prefix)),
                            Box::new(Rope::Leaf(suffix)),
                        ))
                    }
                } else {
                    Err(Rope::Leaf(slice))
                }
            }
            Rope::Branch(size, mut left, mut right) => {
                let left_size = left.size();
                if ix >= left_size {
                    match (*right).delete(ix - left_size) {
                        Err(new) => {
                            *right = new;
                            Err(Rope::Branch(size, left, right))
                        }
                        Ok(new) => {
                            if new.size() == 0 {
                                Ok(*left)
                            } else {
                                *right = new;
                                Ok(Rope::Branch(size - 1, left, right))
                            }
                        }
                    }
                } else {
                    match (*left).delete(ix) {
                        Err(new) => {
                            *left = new;
                            Err(Rope::Branch(size, left, right))
                        }
                        Ok(new) => {
                            if new.size() == 0 {
                                Ok(*right)
                            } else {
                                *left = new;
                                Ok(Rope::Branch(size - 1, left, right))
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn iter<'b>(&'b self) -> RopeIter<'b, 'a, A> {
        RopeIter {
            next: vec![self],
            current: [].iter(),
        }
    }
}

struct RopeIter<'b, 'a, A> {
    next: Vec<&'b Rope<'a, A>>,
    current: std::slice::Iter<'a, A>,
}

impl<'b, 'a, A> Iterator for RopeIter<'b, 'a, A> {
    type Item = &'a A;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current.next() {
            Some(val) => Some(val),
            None => match self.next.pop() {
                None => None,
                Some(next) => {
                    let mut next = next;
                    loop {
                        match next {
                            Rope::Empty => match self.next.pop() {
                                None => return None,
                                Some(val) => {
                                    next = val;
                                }
                            },
                            Rope::Branch(_, a, b) => {
                                next = &*a;
                                self.next.push(&*b);
                            }
                            Rope::Leaf(slice) => {
                                let mut slice_iter = slice.iter();
                                let next = slice_iter.next();
                                self.current = slice_iter;
                                return next;
                            }
                        }
                    }
                }
            },
        }
    }
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
    fn mk_arrow(a: Kind, b: Kind) -> Kind {
        Kind::Arrow(Box::new(a), Box::new(b))
    }

    fn render(&self) -> String {
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

impl Typechecker {
    pub fn new() -> Self {
        Typechecker {
            kind_solutions: vec![],
            type_solutions: vec![],
            typevar_kinds: vec![],
            context: Context::new(),
            position: None,
        }
    }

    pub fn check_module(&mut self, module: syntax::Module) -> Result<core::Module, TypeError> {
        let decls = module.decls.into_iter().fold(Ok(vec![]), |acc, decl| {
            acc.and_then(|mut decls| {
                self.check_declaration(decl).and_then(|decl| {
                    decls.push(decl);
                    Ok(decls)
                })
            })
        })?;
        Ok(core::Module { decls })
    }

    fn check_declaration(
        &mut self,
        decl: syntax::Spanned<syntax::Declaration>,
    ) -> Result<core::Declaration, TypeError> {
        match decl.item {
            syntax::Declaration::Definition {
                name,
                ty,
                args,
                body,
            } => {
                let body = self.check_expr(
                    syntax::Spanned {
                        pos: decl.pos,
                        item: syntax::Expr::mk_lam(args, body),
                    },
                    ty.clone(),
                )?;
                Ok(core::Declaration::Definition { name, ty, body })
            }
            syntax::Declaration::TypeAlias { name, args, body } => {
                todo!()
            }
            syntax::Declaration::Import { module, name } => {
                todo!()
            }
            syntax::Declaration::FromImport { module, names } => {
                todo!()
            }
        }
    }

    fn current_position(&self) -> usize {
        match self.position {
            None => 0,
            Some(n) => n,
        }
    }

    fn lookup_expr(&self, name: String) -> Result<ContextEntry, TypeError> {
        match self.context.lookup(&name) {
            Some(entry) => Ok(entry),
            None => Err(TypeError::NotInScope {
                pos: self.current_position(),
                name,
            }),
        }
    }

    fn zonk_type(&self, ty: syntax::Type) -> Option<syntax::Type> {
        match ty {
            syntax::Type::Name(n) => Some(syntax::Type::Name(n)),
            syntax::Type::Bool => Some(syntax::Type::Bool),
            syntax::Type::Int => Some(syntax::Type::Int),
            syntax::Type::Char => Some(syntax::Type::Char),
            syntax::Type::String => Some(syntax::Type::String),
            syntax::Type::Arrow => Some(syntax::Type::Arrow),
            syntax::Type::FatArrow => Some(syntax::Type::FatArrow),
            syntax::Type::Constraints(cs) => {
                let mut new_cs = Vec::new();
                for c in cs {
                    match self.zonk_type(c) {
                        None => return None,
                        Some(new_c) => {
                            new_cs.push(new_c);
                        }
                    }
                }
                Some(syntax::Type::Constraints(new_cs))
            }
            syntax::Type::Array => Some(syntax::Type::Array),
            syntax::Type::Record => Some(syntax::Type::Record),
            syntax::Type::Variant => Some(syntax::Type::Variant),
            syntax::Type::IO => Some(syntax::Type::IO),
            syntax::Type::App(a, b) => {
                let a = self.zonk_type(*a)?;
                let b = self.zonk_type(*b)?;
                Some(syntax::Type::mk_app(a, b))
            }
            syntax::Type::RowNil => Some(syntax::Type::RowNil),
            syntax::Type::Unit => Some(syntax::Type::Unit),
            syntax::Type::RowCons(field, ty, rest) => {
                let ty = self.zonk_type(*ty)?;
                let rest = self.zonk_type(*rest)?;
                Some(syntax::Type::mk_rowcons(field, ty, rest))
            }
            syntax::Type::Meta(n) => self.type_solutions[n].clone(),
        }
    }

    fn zonk_kind(&self, kind: Kind) -> Option<Kind> {
        match kind {
            Kind::Type => Some(Kind::Type),
            Kind::Row => Some(Kind::Row),
            Kind::Constraint => Some(Kind::Constraint),
            Kind::Arrow(a, b) => {
                let a_zonked = self.zonk_kind(*a)?;
                let b_zonked = self.zonk_kind(*b)?;
                Some(Kind::mk_arrow(a_zonked, b_zonked))
            }
            Kind::Meta(m) => self.kind_solutions[m].clone(),
        }
    }

    fn fresh_kindvar(&mut self) -> Kind {
        let n = self.kind_solutions.len();
        self.kind_solutions.push(None);
        Kind::Meta(n)
    }

    fn kind_mismatch<A>(&self, expected: Kind, actual: Kind) -> Result<A, TypeError> {
        Err(TypeError::KindMismatch {
            pos: self.current_position(),
            expected,
            actual,
        })
    }

    fn solve_kindvar_right(&mut self, expected: Kind, meta: usize) -> Result<(), TypeError> {
        match self.kind_solutions[meta].clone() {
            None => {
                self.kind_solutions[meta] = Some(expected);
                Ok(())
            }
            Some(actual) => self.unify_kind(expected, actual),
        }
    }

    fn solve_kindvar_left(&mut self, meta: usize, actual: Kind) -> Result<(), TypeError> {
        match self.kind_solutions[meta].clone() {
            None => {
                self.kind_solutions[meta] = Some(actual);
                Ok(())
            }
            Some(expected) => self.unify_kind(expected, actual),
        }
    }

    fn type_mismatch<A>(
        &self,
        expected: syntax::Type,
        actual: syntax::Type,
    ) -> Result<A, TypeError> {
        Err(TypeError::TypeMismatch {
            pos: self.current_position(),
            expected,
            actual,
        })
    }

    fn solve_typevar_right(
        &mut self,
        expected: syntax::Type,
        meta: usize,
    ) -> Result<(), TypeError> {
        match self.type_solutions[meta].clone() {
            None => {
                self.type_solutions[meta] = Some(expected);
                Ok(())
            }
            Some(actual) => self.unify_type(expected, actual),
        }
    }

    fn solve_typevar_left(&mut self, meta: usize, actual: syntax::Type) -> Result<(), TypeError> {
        match self.type_solutions[meta].clone() {
            None => {
                self.type_solutions[meta] = Some(actual);
                Ok(())
            }
            Some(expected) => self.unify_type(expected, actual),
        }
    }

    fn unify_kind(&mut self, expected: Kind, actual: Kind) -> Result<(), TypeError> {
        match expected.clone() {
            Kind::Type => match actual {
                Kind::Type => Ok(()),
                Kind::Meta(m) => self.solve_kindvar_right(expected, m),
                _ => self.kind_mismatch(expected, actual),
            },
            Kind::Row => match actual {
                Kind::Row => Ok(()),
                Kind::Meta(m) => self.solve_kindvar_right(expected, m),
                _ => self.kind_mismatch(expected, actual),
            },
            Kind::Constraint => match actual {
                Kind::Constraint => Ok(()),
                Kind::Meta(m) => self.solve_kindvar_right(expected, m),
                _ => self.kind_mismatch(expected, actual),
            },
            Kind::Arrow(expected_a, expected_b) => match actual {
                Kind::Arrow(actual_a, actual_b) => {
                    self.unify_kind(*expected_a, *actual_a)?;
                    self.unify_kind(*expected_b, *actual_b)
                }
                Kind::Meta(m) => self.solve_kindvar_right(expected, m),
                _ => self.kind_mismatch(expected, actual),
            },
            Kind::Meta(expected_m) => match actual {
                Kind::Meta(actual_m) if expected_m == actual_m => Ok(()),
                actual => self.solve_kindvar_left(expected_m, actual),
            },
        }
    }

    fn check_kind(&mut self, ty: &syntax::Type, kind: Kind) -> Result<(), TypeError> {
        let expected = kind;
        let actual = self.infer_kind(ty)?;
        self.unify_kind(expected, actual)
    }

    fn lookup_typevar(&self, n: usize) -> Result<Kind, TypeError> {
        match self.typevar_kinds.get(n) {
            None => panic!("missing kind for type var: ?{}", n),
            Some(k) => Ok(k.clone()),
        }
    }

    fn infer_kind(&mut self, ty: &syntax::Type) -> Result<Kind, TypeError> {
        match ty {
            syntax::Type::Name(n) => {
                todo!()
            }
            syntax::Type::Bool => Ok(Kind::Type),
            syntax::Type::Int => Ok(Kind::Type),
            syntax::Type::Char => Ok(Kind::Type),
            syntax::Type::String => Ok(Kind::Type),
            syntax::Type::Arrow => Ok(Kind::mk_arrow(
                Kind::Type,
                Kind::mk_arrow(Kind::Type, Kind::Type),
            )),
            syntax::Type::FatArrow => Ok(Kind::mk_arrow(
                Kind::Constraint,
                Kind::mk_arrow(Kind::Type, Kind::Type),
            )),
            syntax::Type::Constraints(constraints) => {
                let () = constraints.iter().fold(Ok(()), |acc, constraint| {
                    acc.and_then(|()| self.check_kind(constraint, Kind::Constraint))
                })?;
                Ok(Kind::Constraint)
            }
            syntax::Type::Array => Ok(Kind::mk_arrow(Kind::Type, Kind::Type)),
            syntax::Type::Record => Ok(Kind::mk_arrow(Kind::Row, Kind::Type)),
            syntax::Type::Variant => Ok(Kind::mk_arrow(Kind::Row, Kind::Type)),
            syntax::Type::IO => Ok(Kind::mk_arrow(Kind::Type, Kind::Type)),
            syntax::Type::App(a, b) => {
                let a_kind = self.infer_kind(a)?;
                let in_kind = self.fresh_kindvar();
                let out_kind = self.fresh_kindvar();
                self.unify_kind(Kind::mk_arrow(in_kind.clone(), out_kind.clone()), a_kind)?;
                self.check_kind(b, in_kind)?;
                Ok(out_kind)
            }
            syntax::Type::RowNil => Ok(Kind::Row),
            syntax::Type::RowCons(_, ty, rest) => {
                self.check_kind(ty, Kind::Type)?;
                self.check_kind(rest, Kind::Row)?;
                Ok(Kind::Row)
            }
            syntax::Type::Unit => Ok(Kind::Type),
            syntax::Type::Meta(n) => self.lookup_typevar(*n),
        }
    }

    fn fresh_typevar(&mut self, kind: Kind) -> syntax::Type {
        let n = self.type_solutions.len();
        self.type_solutions.push(None);
        self.typevar_kinds.push(kind);
        syntax::Type::Meta(n)
    }

    fn unify_type(
        &mut self,
        expected: syntax::Type,
        actual: syntax::Type,
    ) -> Result<(), TypeError> {
        let expected_kind = self.infer_kind(&expected)?;
        let actual_kind = self.infer_kind(&actual)?;
        self.unify_kind(expected_kind, actual_kind)?;
        match expected {
            syntax::Type::App(a1, b1) => match actual {
                syntax::Type::App(a2, b2) => {
                    self.unify_type(*a1, *a2)?;
                    self.unify_type(*b1, *b2)?;
                    Ok(())
                }
                syntax::Type::Meta(n) => self.solve_typevar_right(syntax::Type::App(a1, b1), n),
                actual => self.type_mismatch(syntax::Type::App(a1, b1), actual),
            },
            syntax::Type::Name(n) => match actual {
                syntax::Type::Name(nn) if n == nn => Ok(()),
                syntax::Type::Meta(nn) => self.solve_typevar_right(syntax::Type::Name(n), nn),
                actual => self.type_mismatch(syntax::Type::Name(n), actual),
            },
            syntax::Type::Bool => match actual {
                syntax::Type::Bool => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(expected, n),
                _ => self.type_mismatch(expected, actual),
            },
            syntax::Type::Int => match actual {
                syntax::Type::Int => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(expected, n),
                _ => self.type_mismatch(expected, actual),
            },
            syntax::Type::Char => match actual {
                syntax::Type::Char => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(expected, n),
                _ => self.type_mismatch(expected, actual),
            },
            syntax::Type::String => match actual {
                syntax::Type::String => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(expected, n),
                _ => self.type_mismatch(expected, actual),
            },
            syntax::Type::Array => match actual {
                syntax::Type::Array => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(expected, n),
                _ => self.type_mismatch(expected, actual),
            },
            syntax::Type::Arrow => match actual {
                syntax::Type::Arrow => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(expected, n),
                _ => self.type_mismatch(expected, actual),
            },
            syntax::Type::FatArrow => match actual {
                syntax::Type::FatArrow => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(expected, n),
                _ => self.type_mismatch(expected, actual),
            },
            syntax::Type::Constraints(constraints1) => match actual {
                syntax::Type::Constraints(constraints2) => {
                    for (c1, c2) in constraints1.into_iter().zip(constraints2.into_iter()) {
                        match self.unify_type(c1, c2) {
                            Err(err) => return Err(err),
                            Ok(_) => {}
                        }
                    }
                    Ok(())
                }
                syntax::Type::Meta(n) => {
                    self.solve_typevar_right(syntax::Type::Constraints(constraints1), n)
                }
                actual => self.type_mismatch(syntax::Type::Constraints(constraints1), actual),
            },
            syntax::Type::Record => match actual {
                syntax::Type::Record => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(expected, n),
                _ => self.type_mismatch(expected, actual),
            },
            syntax::Type::Variant => match actual {
                syntax::Type::Variant => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(expected, n),
                _ => self.type_mismatch(expected, actual),
            },
            syntax::Type::IO => match actual {
                syntax::Type::IO => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(expected, n),
                _ => self.type_mismatch(expected, actual),
            },
            syntax::Type::RowNil => match actual {
                syntax::Type::RowNil => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(expected, n),
                _ => self.type_mismatch(expected, actual),
            },
            syntax::Type::RowCons(field1, ty1, rest1) => match actual {
                syntax::Type::RowCons(field2, ty2, rest2) => {
                    let expected = syntax::Type::RowCons(field1, ty1, rest1);
                    let actual = syntax::Type::RowCons(field2, ty2, rest2);
                    let (rows1, rest1) = expected.unwrap_rows();
                    let (rows2, rest2) = actual.unwrap_rows();

                    let mut rows2_remaining = Rope::from_vec(&rows2);

                    let mut sames: Vec<(&String, &syntax::Type, &syntax::Type)> = Vec::new();
                    let mut not_in_rows2: Vec<(String, syntax::Type)> = Vec::new();

                    for (field1, ty1) in &rows1 {
                        match rows2_remaining.iter().find(|(field2, _)| field1 == field2) {
                            None => {
                                not_in_rows2.push(((**field1).clone(), (**ty1).clone()));
                            }
                            Some((_, ty2)) => {
                                rows2_remaining =
                                    match rows2_remaining.delete_first(&|(f, _)| f == field1) {
                                        Err(new) => new,
                                        Ok(new) => new,
                                    };
                                sames.push((field1, ty1, ty2));
                            }
                        }
                    }

                    // every field in rows1 that has a partner in rows2 has been deleted from rows2
                    // therefore whatever's left in rows2_remaining is necessarily not in rows_1
                    let mut not_in_rows1: Vec<(String, syntax::Type)> = rows2_remaining
                        .iter()
                        .map(|(a, b)| ((**a).clone(), (**b).clone()))
                        .collect();

                    // now we're working with: sames, not_in_rows1, not_in_rows2
                    //
                    // unify sames
                    for (field, ty1, ty2) in sames {
                        match self.unify_type((*ty1).clone(), (*ty2).clone()) {
                            Err(err) => return Err(err),
                            Ok(()) => {}
                        }
                    }

                    let rest3 = Some(self.fresh_typevar(Kind::Row));
                    self.unify_type(
                        match rest1 {
                            None => syntax::Type::RowNil,
                            Some(ty) => (*ty).clone(),
                        },
                        syntax::Type::mk_rows(not_in_rows1, rest3.clone()),
                    )?;
                    self.unify_type(
                        syntax::Type::mk_rows(not_in_rows2, rest3),
                        match rest2 {
                            None => syntax::Type::RowNil,
                            Some(ty) => (*ty).clone(),
                        },
                    )?;

                    Ok(())
                }
                syntax::Type::Meta(n) => {
                    self.solve_typevar_right(syntax::Type::RowCons(field1, ty1, rest1), n)
                }
                actual => self.type_mismatch(syntax::Type::RowCons(field1, ty1, rest1), actual),
            },
            syntax::Type::Unit => match actual {
                syntax::Type::Unit => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(expected, n),
                _ => self.type_mismatch(expected, actual),
            },
            syntax::Type::Meta(n) => match actual {
                syntax::Type::Meta(nn) if n == nn => Ok(()),
                _ => self.solve_typevar_left(n, actual),
            },
        }
    }

    fn check_duplicate_args(&self, args: &Vec<&Spanned<String>>) -> Result<(), TypeError> {
        let mut seen: HashSet<&String> = HashSet::new();
        for arg in args {
            if seen.contains(&arg.item) {
                return Err(TypeError::DuplicateArgument {
                    pos: arg.pos,
                    name: arg.item.clone(),
                });
            } else {
                seen.insert(&arg.item);
            }
        }
        Ok(())
    }

    fn infer_pattern<'a, 'b>(
        &'a mut self,
        arg: &'b syntax::Pattern,
    ) -> (core::Pattern, syntax::Type, Vec<(&'b String, syntax::Type)>) {
        match arg {
            syntax::Pattern::Wildcard => (
                core::Pattern::Wildcard,
                self.fresh_typevar(Kind::Type),
                Vec::new(),
            ),
            syntax::Pattern::Name(n) => {
                let ty = self.fresh_typevar(Kind::Type);
                (core::Pattern::Name, ty.clone(), vec![(&n.item, ty)])
            }
            syntax::Pattern::Record { names, rest } => {
                let mut names_tys: Vec<(&String, syntax::Type)> = names
                    .iter()
                    .map(|name| (&name.item, self.fresh_typevar(Kind::Type)))
                    .collect();
                let rest_ty: Option<(&String, syntax::Type)> = match rest {
                    None => None,
                    Some(name) => Some((&name.item, self.fresh_typevar(Kind::Type))),
                };
                let ty = syntax::Type::mk_record(
                    names_tys
                        .iter()
                        .map(|(name, ty)| ((*name).clone(), ty.clone()))
                        .collect(),
                    rest_ty.clone().map(|x| x.1),
                );
                rest_ty.map(|(rest_name, rest_ty)| {
                    names_tys.push((
                        rest_name,
                        syntax::Type::mk_record(Vec::new(), Some(rest_ty)),
                    ))
                });
                (
                    core::Pattern::Record {
                        names: names.len(),
                        rest: match rest {
                            None => false,
                            Some(_) => true,
                        },
                    },
                    ty,
                    names_tys,
                )
            }
            syntax::Pattern::Variant { name, arg } => {
                let arg_ty: syntax::Type = self.fresh_typevar(Kind::Type);
                let rest_ty = Some(self.fresh_typevar(Kind::Type));
                let ty = syntax::Type::mk_variant(vec![(name.clone(), arg_ty.clone())], rest_ty);
                (
                    core::Pattern::Variant { name: name.clone() },
                    ty,
                    vec![(&arg.item, arg_ty)],
                )
            }
        }
    }

    fn check_string_part(
        &mut self,
        part: syntax::StringPart,
    ) -> Result<core::StringPart, TypeError> {
        match part {
            syntax::StringPart::String(s) => Ok(core::StringPart::String(s)),
            syntax::StringPart::Expr(e) => {
                let e_core = self.check_expr(e, syntax::Type::String)?;
                Ok(core::StringPart::Expr(e_core))
            }
        }
    }

    fn infer_expr(
        &mut self,
        expr: syntax::Spanned<syntax::Expr>,
    ) -> Result<(core::Expr, syntax::Type), TypeError> {
        with_position!(
            self,
            expr.pos,
            match expr.item {
                syntax::Expr::Var(name) => {
                    let entry = self.lookup_expr(name)?;
                    Ok((core::Expr::Var(entry.index), entry.ty))
                }
                syntax::Expr::App(f, x) => {
                    let (f_core, f_ty) = self.infer_expr(*f)?;
                    let in_ty = self.fresh_typevar(Kind::Type);
                    let out_ty = self.fresh_typevar(Kind::Type);
                    self.unify_type(syntax::Type::mk_app(in_ty.clone(), out_ty.clone()), f_ty)?;
                    let x_core = self.check_expr(*x, in_ty)?;
                    Ok((core::Expr::mk_app(f_core, x_core), out_ty))
                }
                syntax::Expr::Lam { args, body } => {
                    {
                        let mut arg_names_spanned: Vec<&Spanned<String>> = Vec::new();
                        for arg in &args {
                            arg_names_spanned.extend(arg.get_arg_names());
                        }
                        self.check_duplicate_args(&arg_names_spanned)?;
                    }

                    let mut args_core = Vec::new();
                    let mut args_tys = Vec::new();
                    let mut args_names_tys = Vec::new();
                    for arg in &args {
                        let (arg_core, arg_tys, arg_names_tys) = self.infer_pattern(&arg);
                        args_core.push(arg_core);
                        args_tys.push(arg_tys);
                        args_names_tys.extend(arg_names_tys);
                    }

                    self.context.insert(&args_names_tys);
                    let (body_core, body_ty) = self.infer_expr(*body)?;
                    self.context
                        .delete(&args_names_tys.iter().map(|el| el.0).collect());

                    let mut expr_core = body_core;
                    for arg_core in args_core.into_iter().rev() {
                        expr_core = core::Expr::mk_lam(arg_core, expr_core);
                    }
                    let mut expr_ty = body_ty;
                    for arg_ty in args_tys.into_iter().rev() {
                        expr_ty = syntax::Type::mk_arrow(arg_ty, expr_ty);
                    }
                    Ok((expr_core, expr_ty))
                }
                syntax::Expr::True => Ok((core::Expr::True, syntax::Type::Bool)),
                syntax::Expr::False => Ok((core::Expr::True, syntax::Type::Bool)),
                syntax::Expr::IfThenElse(cond, then_, else_) => {
                    let cond_core = self.check_expr(*cond, syntax::Type::Bool)?;
                    let (then_core, then_ty) = self.infer_expr(*then_)?;
                    let else_core = self.check_expr(*else_, then_ty.clone())?;
                    Ok((
                        core::Expr::mk_ifthenelse(cond_core, then_core, else_core),
                        then_ty,
                    ))
                }
                syntax::Expr::Unit => Ok((core::Expr::Unit, syntax::Type::Unit)),
                syntax::Expr::Int(n) => Ok((core::Expr::Int(n), syntax::Type::Int)),
                syntax::Expr::Char(c) => Ok((core::Expr::Char(c), syntax::Type::Char)),
                syntax::Expr::String(parts) => {
                    let mut parts_core = Vec::new();
                    for part in parts {
                        match self.check_string_part(part) {
                            Err(err) => return Err(err),
                            Ok(part_core) => parts_core.push(part_core),
                        }
                    }
                    Ok((core::Expr::String(parts_core), syntax::Type::String))
                }
                syntax::Expr::Array(items) => {
                    let mut items_iter = items.into_iter();
                    match items_iter.next() {
                        Some(first) => {
                            let (first_core, first_ty) = self.infer_expr(first)?;
                            let mut items_core = vec![first_core];
                            for item in items_iter {
                                let item_core = self.check_expr(item, first_ty.clone())?;
                                items_core.push(item_core);
                            }
                            Ok((
                                core::Expr::Array(items_core),
                                syntax::Type::mk_app(syntax::Type::Array, first_ty),
                            ))
                        }
                        None => Ok((
                            core::Expr::Array(Vec::new()),
                            syntax::Type::mk_app(
                                syntax::Type::Array,
                                self.fresh_typevar(Kind::Type),
                            ),
                        )),
                    }
                }
                syntax::Expr::Record { fields, rest } => {
                    let mut fields_core = Vec::new();
                    let mut fields_rows = Vec::new();
                    for (field, expr) in fields {
                        match self.infer_expr(expr) {
                            Err(err) => return Err(err),
                            Ok((expr_core, expr_ty)) => {
                                fields_core.push(expr_core);
                                fields_rows.push((field, expr_ty));
                            }
                        }
                    }

                    let mut rest_core = None;
                    let mut rest_row = None;
                    for expr in rest {
                        let rest_tyvar = self.fresh_typevar(Kind::Row);
                        match self.check_expr(
                            *expr,
                            syntax::Type::mk_app(syntax::Type::Record, rest_tyvar.clone()),
                        ) {
                            Err(err) => return Err(err),
                            Ok(expr_core) => {
                                rest_core = Some(expr_core);
                                rest_row = Some(rest_tyvar);
                            }
                        }
                    }

                    Ok((
                        core::Expr::mk_record(fields_core, rest_core),
                        syntax::Type::mk_record(fields_rows, rest_row),
                    ))
                }
                syntax::Expr::Project(_, _) => {
                    todo!();
                }
                syntax::Expr::Variant(_, _) => {
                    todo!();
                }
                syntax::Expr::Binop(_, _, _) => {
                    todo!();
                }
                syntax::Expr::Case(_, _) => {
                    todo!();
                }
            }
        )
    }

    fn check_expr(
        &mut self,
        expr: syntax::Spanned<syntax::Expr>,
        ty: syntax::Type,
    ) -> Result<core::Expr, TypeError> {
        with_position!(self, expr.pos, {
            let expected = ty;
            let (expr, actual) = self.infer_expr(expr)?;
            self.unify_type(expected, actual)?;
            Ok(expr)
        })
    }
}
