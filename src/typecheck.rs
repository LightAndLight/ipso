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

macro_rules! with_position {
    ($self:ident, $new:expr, $x:expr) => {{
        let current = self.position;
        self.position = $new;
        let res = $x;
        self.position = old;
        res
    }};
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
        decl: syntax::Declaration,
    ) -> Result<core::Declaration, TypeError> {
        match decl {
            syntax::Declaration::Definition {
                name,
                ty,
                args,
                body,
            } => {
                let body = self.check_expr(syntax::Expr::mk_lam(args, body), ty.clone())?;
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
            None => panic!("missing type var: ?{}", n),
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

    fn unify_type(
        &mut self,
        expected: syntax::Type,
        actual: syntax::Type,
    ) -> Result<(), TypeError> {
        let expected_kind = self.infer_kind(&expected)?;
        let actual_kind = self.infer_kind(&actual)?;
        self.unify_kind(expected_kind, actual_kind)?;
        match expected {
            syntax::Type::App(a, b) => {
                todo!()
            }
            syntax::Type::Name(n) => {
                todo!()
            }
            syntax::Type::Bool => {
                todo!()
            }
            syntax::Type::Int => {
                todo!()
            }
            syntax::Type::Char => {
                todo!()
            }
            syntax::Type::String => {
                todo!()
            }
            syntax::Type::Array => {
                todo!()
            }
            syntax::Type::Arrow => {
                todo!()
            }
            syntax::Type::FatArrow => {
                todo!()
            }
            syntax::Type::Constraints(constraints) => {
                todo!()
            }
            syntax::Type::Record => {
                todo!()
            }
            syntax::Type::Variant => {
                todo!()
            }
            syntax::Type::IO => {
                todo!()
            }
            syntax::Type::RowNil => {
                todo!()
            }
            syntax::Type::RowCons(field, ty, rest) => {
                todo!()
            }
            syntax::Type::Unit => {
                todo!()
            }
            syntax::Type::Meta(n) => {
                todo!()
            }
        }
    }

    fn fresh_typevar(&mut self) -> syntax::Type {
        let n = self.type_solutions.len();
        self.type_solutions.push(None);
        syntax::Type::Meta(n)
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
            syntax::Pattern::Wildcard => {
                (core::Pattern::Wildcard, self.fresh_typevar(), Vec::new())
            }
            syntax::Pattern::Name(n) => {
                let ty = self.fresh_typevar();
                (core::Pattern::Name, ty.clone(), vec![(&n.item, ty)])
            }
            syntax::Pattern::Record { names, rest } => {
                let mut names_tys: Vec<(&String, syntax::Type)> = names
                    .iter()
                    .map(|name| (&name.item, self.fresh_typevar()))
                    .collect();
                let rest_ty: Option<(&String, syntax::Type)> = match rest {
                    None => None,
                    Some(name) => Some((&name.item, self.fresh_typevar())),
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
                let arg_ty: syntax::Type = self.fresh_typevar();
                let rest_ty = Some(self.fresh_typevar());
                let ty = syntax::Type::mk_variant(vec![(name.clone(), arg_ty.clone())], rest_ty);
                (
                    core::Pattern::Variant { name: name.clone() },
                    ty,
                    vec![(&arg.item, arg_ty)],
                )
            }
        }
    }

    fn infer_expr(&mut self, expr: syntax::Expr) -> Result<(core::Expr, syntax::Type), TypeError> {
        match expr {
            syntax::Expr::Var(name) => {
                let entry = self.lookup_expr(name)?;
                Ok((core::Expr::Var(entry.index), entry.ty))
            }
            syntax::Expr::App(f, x) => {
                let (f_core, f_ty) = self.infer_expr(*f)?;
                let in_ty = self.fresh_typevar();
                let out_ty = self.fresh_typevar();
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
            syntax::Expr::IfThenElse(_, _, _) => {
                todo!();
            }
            syntax::Expr::Binop(_, _, _) => {
                todo!();
            }
            syntax::Expr::Int(_) => {
                todo!();
            }
            syntax::Expr::Char(_) => {
                todo!();
            }
            syntax::Expr::String(_) => {
                todo!();
            }
            syntax::Expr::Array(_) => {
                todo!();
            }
            syntax::Expr::Record { fields, rest } => {
                todo!();
            }
            syntax::Expr::Project(_, _) => {
                todo!();
            }
            syntax::Expr::Variant(_, _) => {
                todo!();
            }
            syntax::Expr::Case(_, _) => {
                todo!();
            }
            syntax::Expr::Unit => {
                todo!();
            }
        }
    }

    fn check_expr(
        &mut self,
        expr: syntax::Expr,
        ty: syntax::Type,
    ) -> Result<core::Expr, TypeError> {
        let expected = ty;
        let (expr, actual) = self.infer_expr(expr)?;
        self.unify_type(expected, actual)?;
        Ok(expr)
    }
}
