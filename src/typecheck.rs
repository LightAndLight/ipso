use crate::diagnostic;
use crate::rope::Rope;
use crate::syntax;
use crate::syntax::Spanned;
use crate::{builtins, evidence::Evidence};
use crate::{core, evidence};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

#[cfg(test)]
mod test;

#[derive(Debug, PartialEq, Eq)]
struct BoundVars<A> {
    indices: HashMap<String, Vec<usize>>,
    info: Vec<(String, A)>,
}

impl<A> BoundVars<A> {
    fn new() -> Self {
        BoundVars {
            indices: HashMap::new(),
            info: Vec::new(),
        }
    }

    fn lookup_name(&self, name: &String) -> Option<(usize, &A)> {
        self.indices
            .get(name)
            .and_then(|entries| entries.last())
            .and_then(|&ix| self.lookup_index(ix).map(|(_, item)| (ix, item)))
    }

    fn lookup_index(&self, ix: usize) -> Option<&(String, A)> {
        self.info.get(self.info.len() - 1 - ix)
    }

    fn insert(&mut self, vars: &Vec<(String, A)>)
    where
        A: Debug + Clone,
    {
        debug_assert!(
            {
                let mut seen: HashSet<&String> = HashSet::new();
                vars.iter().fold(true, |acc, el: &(String, A)| {
                    let acc = acc && !seen.contains(&el.0);
                    seen.insert(&el.0);
                    acc
                })
            },
            "duplicate name in {:?}",
            vars
        );
        let num_vars = vars.len();
        for (_, entries) in &mut self.indices {
            for entry in entries {
                *entry += num_vars;
            }
        }
        for (index, (var, _)) in vars.iter().rev().enumerate() {
            match self.indices.get_mut(var) {
                None => {
                    self.indices.insert((*var).clone(), vec![index]);
                }
                Some(entries) => {
                    entries.push(index);
                }
            };
        }
        self.info.extend(
            vars.iter()
                .map(|(name, item)| ((*name).clone(), item.clone())),
        );
    }

    fn delete(&mut self, count: usize) {
        for _ in 0..count {
            match self.info.pop() {
                None => panic!("unexpected empty context"),
                Some((name, _)) => {
                    let should_delete = match self.indices.get_mut(&name) {
                        None => panic!("context missing entry {:?}", name),
                        Some(ixs) => match ixs.pop() {
                            None => panic!("context ran out of indices in {:?}", name),
                            Some(_) => ixs.len() == 0,
                        },
                    };
                    if should_delete {
                        self.indices.remove(&name);
                    }
                }
            }
        }
        for item in &mut self.indices {
            for entry in item.1 {
                *entry -= count;
            }
        }
    }
}

pub struct Typechecker {
    kind_solutions: Vec<Option<syntax::Kind>>,
    pub type_solutions: Vec<(syntax::Kind, Option<syntax::Type<usize>>)>,
    pub evidence: Evidence<core::Expr>,
    type_context: HashMap<String, syntax::Kind>,
    context: HashMap<String, core::TypeSig>,
    bound_vars: BoundVars<syntax::Type<usize>>,
    bound_tyvars: BoundVars<syntax::Kind>,
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum UnifyKindContext {
    Checking {
        ty: syntax::Type<String>,
        has_kind: syntax::Kind,
    },
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UnifyTypeContext<A> {
    pub expected: syntax::Type<A>,
    pub actual: syntax::Type<A>,
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
        context: UnifyKindContext,
        expected: syntax::Kind,
        actual: syntax::Kind,
    },
    TypeMismatch {
        pos: usize,
        context: UnifyTypeContext<String>,
        expected: syntax::Type<String>,
        actual: syntax::Type<String>,
    },
    RedundantPattern {
        pos: usize,
    },
    KindOccurs {
        pos: usize,
        meta: usize,
        kind: syntax::Kind,
    },
    TypeOccurs {
        pos: usize,
        meta: usize,
        ty: syntax::Type<String>,
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
                context: _,
                expected: _,
                actual: _,
            } => *pos,
            TypeError::TypeMismatch {
                pos,
                context: _,
                expected: _,
                actual: _,
            } => *pos,
            TypeError::NotInScope { pos, name: _ } => *pos,
            TypeError::DuplicateArgument { pos, name: _ } => *pos,
            TypeError::RedundantPattern { pos } => *pos,
            TypeError::KindOccurs { pos, .. } => *pos,
            TypeError::TypeOccurs { pos, .. } => *pos,
        }
    }

    pub fn message(&self) -> String {
        match self {
            TypeError::KindMismatch {
                pos: _,
                context: _,
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
                context,
                expected: _,
                actual: _,
            } => {
                let mut message = String::from("expected type ");
                message.push('"');
                message.push_str(context.expected.render().as_str());
                message.push('"');
                message.push_str(", got type ");
                message.push('"');
                message.push_str(context.actual.render().as_str());
                message.push('"');
                message
            }
            TypeError::NotInScope { pos: _, name: _ } => String::from("not in scope"),
            TypeError::DuplicateArgument { pos: _, name: _ } => String::from("duplicate argument"),
            TypeError::RedundantPattern { pos: _ } => String::from("redundant pattern"),
            TypeError::KindOccurs { meta, kind, .. } => {
                format!(
                    "infinite kind from equating ?{} with \"{}\"",
                    meta,
                    kind.render()
                )
            }
            TypeError::TypeOccurs { meta, ty, .. } => {
                format!(
                    "infinite type from equating ?{} with \"{}\"",
                    meta,
                    ty.render()
                )
            }
        }
    }

    pub fn addendum(&self) -> Option<String> {
        match self {
            TypeError::KindMismatch {
                pos: _,
                context,
                expected: _,
                actual: _,
            } => match context {
                UnifyKindContext::Checking { ty, has_kind } => Some(format!(
                    "While checking that \"{}\" has kind \"{}\"",
                    ty.render(),
                    has_kind.render()
                )),
            },
            TypeError::DuplicateArgument { pos: _, name: _ } => None,
            TypeError::TypeMismatch {
                pos: _,
                context: _,
                expected: _,
                actual: _,
            } => None,
            TypeError::RedundantPattern { pos: _ } => None,
            TypeError::NotInScope { pos: _, name: _ } => None,
            TypeError::KindOccurs { .. } => None,
            TypeError::TypeOccurs { .. } => None,
        }
    }

    pub fn report(&self, diagnostic: &mut diagnostic::Diagnostic) {
        diagnostic.item(diagnostic::Item {
            pos: self.position(),
            message: self.message(),
            addendum: self.addendum(),
        })
    }
}

impl Typechecker {
    pub fn new() -> Self {
        Typechecker {
            kind_solutions: vec![],
            type_solutions: vec![],
            evidence: Evidence::new(),
            type_context: HashMap::new(),
            context: HashMap::new(),
            bound_vars: BoundVars::new(),
            bound_tyvars: BoundVars::new(),
            position: None,
        }
    }

    pub fn new_with_builtins() -> Self {
        let mut tc = Self::new();
        tc.from_import(&builtins::BUILTINS, &syntax::Names::All);
        tc
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

    fn from_import(&mut self, module: &core::Module, names: &syntax::Names) {
        let should_import = |name: &String| -> bool {
            match names {
                syntax::Names::All => true,
                syntax::Names::Names(names) => names.contains(name),
            }
        };
        for decl in &module.decls {
            match decl {
                core::Declaration::BuiltinType { name, kind } => {
                    if should_import(name) {
                        self.type_context.insert(name.clone(), kind.clone());
                    }
                }
                core::Declaration::Definition { name, sig, body: _ } => {
                    if should_import(name) {
                        self.context.insert(name.clone(), sig.clone());
                    }
                }
                core::Declaration::TypeAlias { name, args, body } => {
                    if should_import(name) {
                        todo!("import type alias {:?}", (name, args, body))
                    }
                }
                core::Declaration::Import { module: _, name: _ } => {}
                core::Declaration::FromImport { module: _, name: _ } => {}
                core::Declaration::Class {
                    ty_vars,
                    supers,
                    name,
                    args,
                    members,
                } => todo!(
                    "import type class {:?}",
                    (ty_vars, supers, name, args, members)
                ),
                core::Declaration::Instance {
                    ty_vars,
                    assumes,
                    head,
                    dict,
                } => todo!(
                    "import type class instance {:?}",
                    (ty_vars, assumes, head, dict)
                ),
            }
        }
    }

    pub fn check_kind(
        &mut self,
        ty: &syntax::Type<usize>,
        kind: syntax::Kind,
    ) -> Result<syntax::Type<usize>, TypeError> {
        let expected = kind;
        let (ty, actual) = self.infer_kind(ty)?;
        let context = UnifyKindContext::Checking {
            ty: self.fill_ty_names(self.zonk_type(ty.clone())),
            has_kind: self.zonk_kind(expected.clone()),
        };
        self.unify_kind(context, expected, actual)?;
        Ok(ty)
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
                let (body_ty, ty_var_kinds) = {
                    let mut vars = HashMap::new();
                    let mut kinds = Vec::new();
                    let ty = ty.map(&mut |x: &String| match vars.get(x) {
                        None => {
                            let len = vars.len();
                            let kind = self.fresh_kindvar();
                            vars.insert(x.clone(), len);
                            kinds.push((x.clone(), kind));
                            len
                        }
                        Some(&n) => n,
                    });
                    (ty, kinds)
                };

                let ty_var_kinds_len = ty_var_kinds.len();
                self.bound_tyvars.insert(&ty_var_kinds);

                let body_ty = self.check_kind(&body_ty, syntax::Kind::Type)?;

                let ty_var_kinds = ty_var_kinds
                    .into_iter()
                    .map(|(_, kind)| match self.zonk_kind(kind) {
                        syntax::Kind::Meta(_) => syntax::Kind::Type,
                        kind => kind,
                    })
                    .collect();

                let sig: core::TypeSig = core::TypeSig {
                    ty_vars: ty_var_kinds,
                    body: body_ty,
                };

                let body = self.check_expr(
                    syntax::Spanned {
                        pos: decl.pos,
                        item: syntax::Expr::mk_lam(args, body),
                    },
                    sig.body.clone(),
                )?;

                self.bound_tyvars.delete(ty_var_kinds_len);

                Ok(core::Declaration::Definition { name, sig, body })
            }
            syntax::Declaration::TypeAlias { name, args, body } => {
                todo!("check type alias {:?}", (name, args, body))
            }
            syntax::Declaration::Import { module, name } => {
                todo!("check import {:?}", (module, name))
            }
            syntax::Declaration::FromImport { module, names } => {
                todo!("check from-import {:?}", (module, names))
            }
        }
    }

    fn current_position(&self) -> usize {
        match self.position {
            None => 0,
            Some(n) => n,
        }
    }

    fn lookup_var(&self, name: &String) -> Option<(usize, syntax::Type<usize>)> {
        self.bound_vars
            .lookup_name(name)
            .map(|(ix, ty)| (ix, ty.clone()))
    }

    fn lookup_name(&self, name: &String) -> Option<core::TypeSig> {
        self.context.get(name).map(|sig| sig.clone())
    }

    fn zonk_type(&self, ty: syntax::Type<usize>) -> syntax::Type<usize> {
        match ty {
            syntax::Type::Name(n) => syntax::Type::Name(n),
            syntax::Type::Var(n) => syntax::Type::Var(n),
            syntax::Type::Bool => syntax::Type::Bool,
            syntax::Type::Int => syntax::Type::Int,
            syntax::Type::Char => syntax::Type::Char,
            syntax::Type::String => syntax::Type::String,
            syntax::Type::Bytes => syntax::Type::Bytes,
            syntax::Type::Arrow => syntax::Type::Arrow,
            syntax::Type::FatArrow => syntax::Type::FatArrow,
            syntax::Type::Constraints(cs) => {
                syntax::Type::Constraints(cs.iter().map(|c| self.zonk_type(c.clone())).collect())
            }
            syntax::Type::Array => syntax::Type::Array,
            syntax::Type::Record => syntax::Type::Record,
            syntax::Type::Variant => syntax::Type::Variant,
            syntax::Type::IO => syntax::Type::IO,
            syntax::Type::App(a, b) => syntax::Type::mk_app(self.zonk_type(*a), self.zonk_type(*b)),
            syntax::Type::RowNil => syntax::Type::RowNil,
            syntax::Type::Unit => syntax::Type::Unit,
            syntax::Type::RowCons(field, ty, rest) => {
                syntax::Type::mk_rowcons(field, self.zonk_type(*ty), self.zonk_type(*rest))
            }
            syntax::Type::Meta(n) => match self.type_solutions[n].1 {
                None => syntax::Type::Meta(n),
                Some(ref ty) => self.zonk_type(ty.clone()),
            },
        }
    }

    fn zonk_kind(&self, kind: syntax::Kind) -> syntax::Kind {
        match kind {
            syntax::Kind::Type => syntax::Kind::Type,
            syntax::Kind::Row => syntax::Kind::Row,
            syntax::Kind::Constraint => syntax::Kind::Constraint,
            syntax::Kind::Arrow(a, b) => {
                syntax::Kind::mk_arrow(self.zonk_kind(*a), self.zonk_kind(*b))
            }
            syntax::Kind::Meta(m) => match self.kind_solutions[m].clone() {
                None => syntax::Kind::Meta(m),
                Some(kind) => self.zonk_kind(kind),
            },
        }
    }

    fn fresh_kindvar(&mut self) -> syntax::Kind {
        let n = self.kind_solutions.len();
        self.kind_solutions.push(None);
        syntax::Kind::Meta(n)
    }

    fn kind_mismatch<A>(
        &self,
        context: UnifyKindContext,
        expected: syntax::Kind,
        actual: syntax::Kind,
    ) -> Result<A, TypeError> {
        Err(TypeError::KindMismatch {
            pos: self.current_position(),
            context,
            expected,
            actual,
        })
    }

    fn unify_kind(
        &mut self,
        context: UnifyKindContext,
        expected: syntax::Kind,
        actual: syntax::Kind,
    ) -> Result<(), TypeError> {
        match expected.clone() {
            syntax::Kind::Type => match actual {
                syntax::Kind::Type => Ok(()),
                syntax::Kind::Meta(m) => self.solve_kindvar_right(context, expected, m),
                _ => self.kind_mismatch(context, expected, actual),
            },
            syntax::Kind::Row => match actual {
                syntax::Kind::Row => Ok(()),
                syntax::Kind::Meta(m) => self.solve_kindvar_right(context, expected, m),
                _ => self.kind_mismatch(context, expected, actual),
            },
            syntax::Kind::Constraint => match actual {
                syntax::Kind::Constraint => Ok(()),
                syntax::Kind::Meta(m) => self.solve_kindvar_right(context, expected, m),
                _ => self.kind_mismatch(context, expected, actual),
            },
            syntax::Kind::Arrow(expected_a, expected_b) => match actual {
                syntax::Kind::Arrow(actual_a, actual_b) => {
                    self.unify_kind(context.clone(), *expected_a, *actual_a)?;
                    self.unify_kind(context, *expected_b, *actual_b)
                }
                syntax::Kind::Meta(m) => self.solve_kindvar_right(context, expected, m),
                _ => self.kind_mismatch(context, expected, actual),
            },
            syntax::Kind::Meta(expected_m) => match actual {
                syntax::Kind::Meta(actual_m) if expected_m == actual_m => Ok(()),
                actual => self.solve_kindvar_left(context, expected_m, actual),
            },
        }
    }

    fn occurs_kind(&self, meta: usize, kind: &syntax::Kind) -> Result<(), TypeError> {
        match kind.iter_metas().find(|&other| {
            meta == other
                || match &self.kind_solutions[other] {
                    None => false,
                    Some(kind) => self.occurs_kind(meta, kind).is_err(),
                }
        }) {
            None => Ok(()),
            Some(_) => Err(TypeError::KindOccurs {
                pos: self.current_position(),
                meta,
                kind: self.zonk_kind(kind.clone()),
            }),
        }
    }

    fn solve_kindvar_right(
        &mut self,
        context: UnifyKindContext,
        expected: syntax::Kind,
        meta: usize,
    ) -> Result<(), TypeError> {
        match self.kind_solutions[meta].clone() {
            None => {
                let _ = self.occurs_kind(meta, &expected)?;
                self.kind_solutions[meta] = Some(expected);
                Ok(())
            }
            Some(actual) => self.unify_kind(context, expected, actual),
        }
    }

    fn solve_kindvar_left(
        &mut self,
        context: UnifyKindContext,
        meta: usize,
        actual: syntax::Kind,
    ) -> Result<(), TypeError> {
        match self.kind_solutions[meta].clone() {
            None => {
                let _ = self.occurs_kind(meta, &actual)?;
                self.kind_solutions[meta] = Some(actual);
                Ok(())
            }
            Some(expected) => self.unify_kind(context, expected, actual),
        }
    }

    fn not_in_scope<A>(&self, name: &String) -> Result<A, TypeError> {
        Err(TypeError::NotInScope {
            pos: self.current_position(),
            name: name.clone(),
        })
    }

    pub fn fill_ty_names(&self, ty: syntax::Type<usize>) -> syntax::Type<String> {
        ty.map(&mut |&ix| self.bound_tyvars.lookup_index(ix).unwrap().0.clone())
    }

    fn type_mismatch<A>(
        &self,
        context: &UnifyTypeContext<usize>,
        expected: syntax::Type<usize>,
        actual: syntax::Type<usize>,
    ) -> Result<A, TypeError> {
        let context = UnifyTypeContext {
            expected: self.fill_ty_names(self.zonk_type(context.expected.clone())),
            actual: self.fill_ty_names(self.zonk_type(context.actual.clone())),
        };
        Err(TypeError::TypeMismatch {
            pos: self.current_position(),
            context,
            expected: self.fill_ty_names(expected),
            actual: self.fill_ty_names(actual),
        })
    }

    fn occurs_type(&self, meta: usize, ty: &syntax::Type<usize>) -> Result<(), TypeError> {
        match ty.iter_metas().find(|&other| {
            meta == other
                || match &self.type_solutions[other].1 {
                    None => false,
                    Some(ty) => self.occurs_type(meta, ty).is_err(),
                }
        }) {
            None => Ok(()),
            Some(_) => Err(TypeError::TypeOccurs {
                pos: self.current_position(),
                meta,
                ty: self.fill_ty_names(self.zonk_type(ty.clone())),
            }),
        }
    }

    fn solve_typevar_right(
        &mut self,
        context: &UnifyTypeContext<usize>,
        expected: syntax::Type<usize>,
        meta: usize,
    ) -> Result<(), TypeError> {
        match self.type_solutions[meta].1.clone() {
            None => {
                let _ = self.occurs_type(meta, &expected)?;
                self.type_solutions[meta].1 = Some(expected);
                Ok(())
            }
            Some(actual) => self.unify_type(context, expected, actual),
        }
    }

    fn solve_typevar_left(
        &mut self,
        context: &UnifyTypeContext<usize>,
        meta: usize,
        actual: syntax::Type<usize>,
    ) -> Result<(), TypeError> {
        match self.type_solutions[meta].1.clone() {
            None => {
                let _ = self.occurs_type(meta, &actual)?;
                self.type_solutions[meta].1 = Some(actual);
                Ok(())
            }
            Some(expected) => self.unify_type(context, expected, actual),
        }
    }

    fn lookup_typevar(&self, n: usize) -> Result<syntax::Kind, TypeError> {
        match self.type_solutions.get(n) {
            None => panic!("missing kind for type var: ?{}", n),
            Some((k, _)) => Ok(k.clone()),
        }
    }

    fn infer_kind(
        &mut self,
        ty: &syntax::Type<usize>,
    ) -> Result<(syntax::Type<usize>, syntax::Kind), TypeError> {
        match ty {
            syntax::Type::Name(n) => match self.type_context.get(n) {
                None => self.not_in_scope(n),
                Some(kind) => Ok((syntax::Type::Name(n.clone()), kind.clone())),
            },
            syntax::Type::Var(ix) => match self.bound_tyvars.lookup_index(*ix) {
                None => panic!("missing tyvar {:?}", ix),
                Some((_, kind)) => Ok((syntax::Type::Var(*ix), kind.clone())),
            },
            syntax::Type::Bool => Ok((syntax::Type::Bool, syntax::Kind::Type)),
            syntax::Type::Int => Ok((syntax::Type::Int, syntax::Kind::Type)),
            syntax::Type::Char => Ok((syntax::Type::Char, syntax::Kind::Type)),
            syntax::Type::String => Ok((syntax::Type::String, syntax::Kind::Type)),
            syntax::Type::Bytes => Ok((syntax::Type::Bytes, syntax::Kind::Type)),
            syntax::Type::Arrow => Ok((
                syntax::Type::Arrow,
                syntax::Kind::mk_arrow(
                    syntax::Kind::Type,
                    syntax::Kind::mk_arrow(syntax::Kind::Type, syntax::Kind::Type),
                ),
            )),
            syntax::Type::FatArrow => Ok((
                syntax::Type::FatArrow,
                syntax::Kind::mk_arrow(
                    syntax::Kind::Constraint,
                    syntax::Kind::mk_arrow(syntax::Kind::Type, syntax::Kind::Type),
                ),
            )),
            syntax::Type::Constraints(constraints) => {
                let mut new_constraints = Vec::new();
                for constraint in constraints {
                    match self.check_kind(constraint, syntax::Kind::Constraint) {
                        Err(err) => return Err(err),
                        Ok(new_constraint) => {
                            new_constraints.push(new_constraint);
                        }
                    }
                }
                Ok((
                    syntax::Type::Constraints(new_constraints),
                    syntax::Kind::Constraint,
                ))
            }
            syntax::Type::Array => Ok((
                syntax::Type::Array,
                syntax::Kind::mk_arrow(syntax::Kind::Type, syntax::Kind::Type),
            )),
            syntax::Type::Record => Ok((
                syntax::Type::Record,
                syntax::Kind::mk_arrow(syntax::Kind::Row, syntax::Kind::Type),
            )),
            syntax::Type::Variant => Ok((
                syntax::Type::Variant,
                syntax::Kind::mk_arrow(syntax::Kind::Row, syntax::Kind::Type),
            )),
            syntax::Type::IO => Ok((
                syntax::Type::IO,
                syntax::Kind::mk_arrow(syntax::Kind::Type, syntax::Kind::Type),
            )),
            syntax::Type::App(a, b) => {
                let in_kind = self.fresh_kindvar();
                let out_kind = self.fresh_kindvar();
                let a =
                    self.check_kind(a, syntax::Kind::mk_arrow(in_kind.clone(), out_kind.clone()))?;
                let b = self.check_kind(b, in_kind)?;
                Ok((syntax::Type::mk_app(a, b), out_kind))
            }
            syntax::Type::RowNil => Ok((syntax::Type::RowNil, syntax::Kind::Row)),
            syntax::Type::RowCons(field, ty, rest) => {
                let ty = self.check_kind(ty, syntax::Kind::Type)?;
                let rest = self.check_kind(rest, syntax::Kind::Row)?;
                Ok((
                    syntax::Type::mk_rowcons(field.clone(), ty, rest),
                    syntax::Kind::Row,
                ))
            }
            syntax::Type::Unit => Ok((syntax::Type::Unit, syntax::Kind::Type)),
            syntax::Type::Meta(n) => {
                let kind = self.lookup_typevar(*n)?;
                Ok((syntax::Type::Meta(*n), kind))
            }
        }
    }

    pub fn fresh_typevar<A>(&mut self, kind: syntax::Kind) -> syntax::Type<A> {
        let n = self.type_solutions.len();
        self.type_solutions.push((kind, None));
        syntax::Type::Meta(n)
    }

    pub fn unify_type(
        &mut self,
        context: &UnifyTypeContext<usize>,
        expected: syntax::Type<usize>,
        actual: syntax::Type<usize>,
    ) -> Result<(), TypeError> {
        let (_, expected_kind) = self.infer_kind(&expected)?;
        let _ = self.check_kind(&actual, expected_kind)?;
        match expected {
            syntax::Type::App(a1, b1) => match actual {
                syntax::Type::App(a2, b2) => {
                    self.unify_type(context, *a1, *a2)?;
                    self.unify_type(context, *b1, *b2)?;
                    Ok(())
                }
                syntax::Type::Meta(n) => {
                    self.solve_typevar_right(context, syntax::Type::App(a1, b1), n)
                }
                actual => self.type_mismatch(context, syntax::Type::App(a1, b1), actual),
            },
            syntax::Type::Name(n) => match actual {
                syntax::Type::Name(nn) if n == nn => Ok(()),
                syntax::Type::Meta(nn) => {
                    self.solve_typevar_right(context, syntax::Type::Name(n), nn)
                }
                actual => self.type_mismatch(context, syntax::Type::Name(n), actual),
            },
            syntax::Type::Var(n) => match actual {
                syntax::Type::Var(nn) if n == nn => Ok(()),
                syntax::Type::Meta(nn) => {
                    self.solve_typevar_right(context, syntax::Type::Var(n), nn)
                }
                actual => self.type_mismatch(context, syntax::Type::Var(n), actual),
            },
            syntax::Type::Bool => match actual {
                syntax::Type::Bool => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::Int => match actual {
                syntax::Type::Int => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::Char => match actual {
                syntax::Type::Char => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::String => match actual {
                syntax::Type::String => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::Bytes => match actual {
                syntax::Type::Bytes => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::Array => match actual {
                syntax::Type::Array => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::Arrow => match actual {
                syntax::Type::Arrow => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::FatArrow => match actual {
                syntax::Type::FatArrow => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::Constraints(constraints1) => match actual {
                syntax::Type::Constraints(constraints2) => {
                    for (c1, c2) in constraints1.into_iter().zip(constraints2.into_iter()) {
                        match self.unify_type(context, c1, c2) {
                            Err(err) => return Err(err),
                            Ok(_) => {}
                        }
                    }
                    Ok(())
                }
                syntax::Type::Meta(n) => {
                    self.solve_typevar_right(context, syntax::Type::Constraints(constraints1), n)
                }
                actual => {
                    self.type_mismatch(context, syntax::Type::Constraints(constraints1), actual)
                }
            },
            syntax::Type::Record => match actual {
                syntax::Type::Record => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::Variant => match actual {
                syntax::Type::Variant => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::IO => match actual {
                syntax::Type::IO => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::RowNil => match actual {
                syntax::Type::RowNil => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::RowCons(field1, ty1, rest1) => match actual {
                syntax::Type::RowCons(field2, ty2, rest2) => {
                    let expected = syntax::Type::RowCons(field1, ty1, rest1);
                    let actual = syntax::Type::RowCons(field2, ty2, rest2);
                    let (rows1, rest1) = expected.unwrap_rows();
                    let (rows2, rest2) = actual.unwrap_rows();

                    let mut rows2_remaining = Rope::from_vec(&rows2);

                    let mut sames: Vec<(&String, &syntax::Type<usize>, &syntax::Type<usize>)> =
                        Vec::new();
                    let mut not_in_rows2: Vec<(String, syntax::Type<usize>)> = Vec::new();

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
                    let not_in_rows1: Vec<(String, syntax::Type<usize>)> = rows2_remaining
                        .iter()
                        .map(|(a, b)| ((**a).clone(), (**b).clone()))
                        .collect();

                    // now we're working with: sames, not_in_rows1, not_in_rows2
                    //
                    // unify sames
                    for (_field, ty1, ty2) in sames {
                        match self.unify_type(context, (*ty1).clone(), (*ty2).clone()) {
                            Err(err) => return Err(err),
                            Ok(()) => {}
                        }
                    }

                    let rest3 = Some(self.fresh_typevar(syntax::Kind::Row));
                    self.unify_type(
                        context,
                        match rest1 {
                            None => syntax::Type::RowNil,
                            Some(ty) => (*ty).clone(),
                        },
                        syntax::Type::mk_rows(not_in_rows1, rest3.clone()),
                    )?;
                    self.unify_type(
                        context,
                        syntax::Type::mk_rows(not_in_rows2, rest3),
                        match rest2 {
                            None => syntax::Type::RowNil,
                            Some(ty) => (*ty).clone(),
                        },
                    )?;

                    Ok(())
                }
                syntax::Type::Meta(n) => {
                    self.solve_typevar_right(context, syntax::Type::RowCons(field1, ty1, rest1), n)
                }
                actual => {
                    self.type_mismatch(context, syntax::Type::RowCons(field1, ty1, rest1), actual)
                }
            },
            syntax::Type::Unit => match actual {
                syntax::Type::Unit => Ok(()),
                syntax::Type::Meta(n) => self.solve_typevar_right(context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            syntax::Type::Meta(n) => match actual {
                syntax::Type::Meta(nn) if n == nn => Ok(()),
                _ => self.solve_typevar_left(context, n, actual),
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
    ) -> (
        core::Pattern,
        syntax::Type<usize>,
        Vec<(String, syntax::Type<usize>)>,
    ) {
        match arg {
            syntax::Pattern::Wildcard => (
                core::Pattern::Wildcard,
                self.fresh_typevar(syntax::Kind::Type),
                Vec::new(),
            ),
            syntax::Pattern::Name(n) => {
                let ty = self.fresh_typevar(syntax::Kind::Type);
                (core::Pattern::Name, ty.clone(), vec![(n.item.clone(), ty)])
            }
            syntax::Pattern::Record { names, rest } => {
                let mut names_tys: Vec<(String, syntax::Type<usize>)> = names
                    .iter()
                    .map(|name| (name.item.clone(), self.fresh_typevar(syntax::Kind::Type)))
                    .collect();
                let rest_ty: Option<(&String, syntax::Type<usize>)> = match rest {
                    None => None,
                    Some(name) => Some((&name.item, self.fresh_typevar(syntax::Kind::Type))),
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
                        rest_name.clone(),
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
                let arg_ty: syntax::Type<usize> = self.fresh_typevar(syntax::Kind::Type);
                let rest_ty = Some(self.fresh_typevar(syntax::Kind::Row));
                let ty = syntax::Type::mk_variant(vec![(name.clone(), arg_ty.clone())], rest_ty);
                (
                    core::Pattern::Variant { name: name.clone() },
                    ty,
                    vec![(arg.item.clone(), arg_ty)],
                )
            }
        }
    }

    fn check_pattern<'a, 'b>(
        &'a mut self,
        arg: &'b syntax::Pattern,
        expected: syntax::Type<usize>,
    ) -> Result<(core::Pattern, Vec<(String, syntax::Type<usize>)>), TypeError> {
        let (pat, actual, binds) = self.infer_pattern(arg);
        let context = UnifyTypeContext {
            expected: expected.clone(),
            actual: actual.clone(),
        };
        self.unify_type(&context, expected, actual)?;
        Ok((pat, binds))
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
    ) -> Result<(core::Expr, syntax::Type<usize>), TypeError> {
        with_position!(
            self,
            expr.pos,
            match expr.item {
                syntax::Expr::Var(name) => {
                    match self.lookup_var(&name) {
                        Some(entry) => Ok((core::Expr::Var(entry.0), entry.1)),
                        None => match self.lookup_name(&name) {
                            Some(sig) => {
                                let metas: Vec<syntax::Type<usize>> = sig
                                    .ty_vars
                                    .into_iter()
                                    .map(|kind| self.fresh_typevar(kind))
                                    .collect();
                                let ty = sig.body.subst(&|&ix| metas[ix].clone());
                                Ok((core::Expr::Name(name), ty))
                            }
                            None => self.not_in_scope(&name),
                        },
                    }
                }
                syntax::Expr::App(f, x) => {
                    let (f_core, f_ty) = self.infer_expr(*f)?;
                    let in_ty = self.fresh_typevar(syntax::Kind::Type);
                    let out_ty = self.fresh_typevar(syntax::Kind::Type);
                    let expected = syntax::Type::mk_arrow(in_ty.clone(), out_ty.clone());
                    let actual = f_ty;
                    let context = UnifyTypeContext {
                        expected: expected.clone(),
                        actual: actual.clone(),
                    };
                    self.unify_type(&context, expected, actual)?;
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
                    let mut args_names_tys: Vec<(String, syntax::Type<usize>)> = Vec::new();
                    for arg in &args {
                        let (arg_core, arg_tys, arg_names_tys) = self.infer_pattern(&arg);
                        args_core.push(arg_core);
                        args_tys.push(arg_tys);
                        args_names_tys.extend(arg_names_tys);
                    }

                    self.bound_vars.insert(&args_names_tys);
                    let (body_core, body_ty) = self.infer_expr(*body)?;
                    self.bound_vars.delete(args_names_tys.len());

                    let mut expr_core = body_core;
                    for arg_core in args_core.into_iter().rev() {
                        match arg_core {
                            core::Pattern::Name => {
                                expr_core = core::Expr::mk_lam(true, expr_core);
                            }
                            core::Pattern::Wildcard => {
                                expr_core = core::Expr::mk_lam(false, expr_core);
                            }
                            arg_core => {
                                expr_core = core::Expr::mk_lam(
                                    true,
                                    core::Expr::mk_case(
                                        core::Expr::Var(0),
                                        vec![core::Branch {
                                            pattern: arg_core,
                                            body: expr_core,
                                        }],
                                    ),
                                );
                            }
                        }
                    }
                    let mut expr_ty = body_ty;
                    for arg_ty in args_tys.into_iter().rev() {
                        expr_ty = syntax::Type::mk_arrow(arg_ty, expr_ty);
                    }
                    Ok((expr_core, expr_ty))
                }
                syntax::Expr::True => Ok((core::Expr::True, syntax::Type::Bool)),
                syntax::Expr::False => Ok((core::Expr::False, syntax::Type::Bool)),
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
                                self.fresh_typevar(syntax::Kind::Type),
                            ),
                        )),
                    }
                }
                syntax::Expr::Record { fields, rest } => {
                    let mut fields_result: Vec<(String, core::Expr, syntax::Type<usize>)> =
                        Vec::new();
                    let mut fields_rows = Vec::new();
                    for (field, expr) in fields {
                        match self.infer_expr(expr) {
                            Err(err) => return Err(err),
                            Ok((expr_core, expr_ty)) => {
                                fields_result.push((field.clone(), expr_core, expr_ty.clone()));
                                fields_rows.push((field.clone(), expr_ty));
                            }
                        }
                    }

                    let rest_row_var = self.fresh_typevar(syntax::Kind::Row);
                    let mut extending_row = rest_row_var.clone();
                    let mut fields_core = Vec::with_capacity(fields_result.len());
                    for (field, expr_core, expr_ty) in fields_result.into_iter().rev() {
                        let index = self.evidence.fresh_evar(evidence::Constraint::HasField {
                            field: field.clone(),
                            rest: extending_row.clone(),
                        });
                        fields_core.push((core::Expr::EVar(index), expr_core));
                        extending_row = syntax::Type::mk_rowcons(field, expr_ty, extending_row);
                    }
                    // we did a right fold to build extending_row, but we want fields_core too look like we did a left fold
                    fields_core.reverse();

                    let mut rest_core = None;
                    let mut rest_row = None;
                    for expr in rest {
                        match self.check_expr(
                            *expr,
                            syntax::Type::mk_app(syntax::Type::Record, rest_row_var.clone()),
                        ) {
                            Err(err) => return Err(err),
                            Ok(expr_core) => {
                                rest_core = Some(expr_core);
                                rest_row = Some(rest_row_var.clone());
                            }
                        }
                    }

                    Ok((
                        core::Expr::mk_record(fields_core, rest_core),
                        syntax::Type::mk_record(fields_rows, rest_row),
                    ))
                }
                syntax::Expr::Project(expr, field) => {
                    let out_ty = self.fresh_typevar(syntax::Kind::Type);
                    let rest = self.fresh_typevar(syntax::Kind::Row);
                    let rows =
                        syntax::Type::mk_rows(vec![(field.clone(), out_ty.clone())], Some(rest));
                    let expr_core = self.check_expr(
                        *expr,
                        syntax::Type::mk_app(syntax::Type::Record, rows.clone()),
                    )?;
                    let offset = self
                        .evidence
                        .fresh_evar(evidence::Constraint::HasField { field, rest: rows });
                    Ok((
                        core::Expr::mk_project(expr_core, core::Expr::EVar(offset)),
                        out_ty,
                    ))
                }
                syntax::Expr::Variant(ctor, arg) => {
                    let (arg_core, arg_ty) = self.infer_expr(*arg)?;
                    let rest = self.fresh_typevar(syntax::Kind::Row);
                    let tag = self.evidence.fresh_evar(evidence::Constraint::HasField {
                        field: ctor.clone(),
                        rest: syntax::Type::mk_rows(vec![], Some(rest.clone())),
                    });
                    Ok((
                        core::Expr::mk_variant(core::Expr::EVar(tag), arg_core),
                        syntax::Type::mk_app(
                            syntax::Type::Variant,
                            syntax::Type::mk_rows(vec![(ctor, arg_ty)], Some(rest)),
                        ),
                    ))
                }
                syntax::Expr::Binop(op, left, right) => {
                    match op {
                        syntax::Binop::Add => {
                            let left_core = self.check_expr(*left, syntax::Type::Int)?;
                            let right_core = self.check_expr(*right, syntax::Type::Int)?;
                            Ok((
                                core::Expr::mk_binop(op, left_core, right_core),
                                syntax::Type::Int,
                            ))
                        }
                        syntax::Binop::Multiply => {
                            let left_core = self.check_expr(*left, syntax::Type::Int)?;
                            let right_core = self.check_expr(*right, syntax::Type::Int)?;
                            Ok((
                                core::Expr::mk_binop(op, left_core, right_core),
                                syntax::Type::Int,
                            ))
                        }
                        syntax::Binop::Subtract => {
                            let left_core = self.check_expr(*left, syntax::Type::Int)?;
                            let right_core = self.check_expr(*right, syntax::Type::Int)?;
                            Ok((
                                core::Expr::mk_binop(op, left_core, right_core),
                                syntax::Type::Int,
                            ))
                        }
                        syntax::Binop::Divide => {
                            let left_core = self.check_expr(*left, syntax::Type::Int)?;
                            let right_core = self.check_expr(*right, syntax::Type::Int)?;
                            Ok((
                                core::Expr::mk_binop(op, left_core, right_core),
                                syntax::Type::Int,
                            ))
                        }

                        syntax::Binop::Append => {
                            let item_ty = self.fresh_typevar(syntax::Kind::Type);
                            let expected = syntax::Type::mk_app(syntax::Type::Array, item_ty);
                            let left_core = self.check_expr(*left, expected.clone())?;
                            let right_core = self.check_expr(*right, expected.clone())?;
                            Ok((core::Expr::mk_binop(op, left_core, right_core), expected))
                        }

                        syntax::Binop::Or => {
                            let left_core = self.check_expr(*left, syntax::Type::Bool)?;
                            let right_core = self.check_expr(*right, syntax::Type::Bool)?;
                            Ok((
                                core::Expr::mk_binop(op, left_core, right_core),
                                syntax::Type::Bool,
                            ))
                        }
                        syntax::Binop::And => {
                            let left_core = self.check_expr(*left, syntax::Type::Bool)?;
                            let right_core = self.check_expr(*right, syntax::Type::Bool)?;
                            Ok((
                                core::Expr::mk_binop(op, left_core, right_core),
                                syntax::Type::Bool,
                            ))
                        }

                        syntax::Binop::Eq => {
                            todo!("==")
                        }
                        syntax::Binop::Neq => {
                            todo!("!=")
                        }

                        syntax::Binop::Gt => {
                            todo!(">")
                        }
                        syntax::Binop::Gte => {
                            todo!(">=")
                        }
                        syntax::Binop::Lt => {
                            todo!("<")
                        }
                        syntax::Binop::Lte => {
                            todo!("<=")
                        }
                    }
                }
                syntax::Expr::Case(expr, branches) => {
                    #[derive(PartialEq, Eq, Hash)]
                    enum Seen {
                        Fallthrough,
                        Record,
                        Variant(String),
                    }

                    fn to_seen(pat: &syntax::Pattern) -> Seen {
                        match pat {
                            syntax::Pattern::Wildcard => Seen::Fallthrough,
                            syntax::Pattern::Name(_) => Seen::Fallthrough,
                            syntax::Pattern::Record { names: _, rest: _ } => Seen::Record,
                            syntax::Pattern::Variant { name, arg: _ } => {
                                Seen::Variant(name.clone())
                            }
                        }
                    }

                    let in_ty = self.fresh_typevar(syntax::Kind::Type);
                    let out_ty = self.fresh_typevar(syntax::Kind::Type);

                    let mut branches_core = Vec::new();
                    let mut seen: HashSet<Seen> = HashSet::new();
                    for branch in branches {
                        let current_seen = to_seen(&branch.pattern.item);
                        let saw_fallthrough = seen.contains(&Seen::Fallthrough);
                        if saw_fallthrough || seen.contains(&current_seen) {
                            return Err(TypeError::RedundantPattern {
                                pos: branch.pattern.pos,
                            });
                        }
                        match self.check_pattern(&branch.pattern.item, in_ty.clone()) {
                            Err(err) => return Err(err),
                            Ok((pattern_core, pattern_bind)) => {
                                self.bound_vars.insert(&pattern_bind);
                                match self.check_expr(branch.body, out_ty.clone()) {
                                    Err(err) => return Err(err),
                                    Ok(body_core) => {
                                        if !saw_fallthrough {
                                            branches_core.push(core::Branch {
                                                pattern: pattern_core,
                                                body: body_core,
                                            })
                                        };
                                    }
                                }
                                self.bound_vars.delete(pattern_bind.len());
                                seen.insert(current_seen);
                            }
                        }
                    }

                    match self.zonk_type(in_ty.clone()).unwrap_variant() {
                        Some((ctors, rest)) if !seen.contains(&Seen::Fallthrough) => match rest {
                            None => {}
                            Some(rest) => {
                                let ctors: Vec<(String, syntax::Type<usize>)> = ctors
                                    .into_iter()
                                    .map(|(a, b)| (a.clone(), b.clone()))
                                    .collect();
                                let context = UnifyTypeContext {
                                    expected: syntax::Type::mk_variant(ctors.clone(), None),
                                    actual: syntax::Type::mk_variant(ctors, Some(rest.clone())),
                                };
                                self.unify_type(&context, syntax::Type::RowNil, rest.clone())?;
                            }
                        },
                        _ => {}
                    }
                    let expr_core = self.check_expr(*expr, in_ty)?;

                    Ok((core::Expr::mk_case(expr_core, branches_core), out_ty))
                }
            }
        )
    }

    fn check_expr(
        &mut self,
        expr: syntax::Spanned<syntax::Expr>,
        ty: syntax::Type<usize>,
    ) -> Result<core::Expr, TypeError> {
        with_position!(self, expr.pos, {
            let expected = ty;
            let (expr, actual) = self.infer_expr(expr)?;
            let context = UnifyTypeContext {
                expected: expected.clone(),
                actual: actual.clone(),
            };
            self.unify_type(&context, expected, actual)?;
            Ok(expr)
        })
    }
}
