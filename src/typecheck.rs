use crate::core;
use crate::diagnostic;
use crate::syntax;
use std::collections::HashMap;

#[cfg(test)]
mod test;

#[derive(Clone)]
struct ContextEntry {
    index: usize,
    ty: syntax::Type,
}

pub struct Typechecker {
    kind_solutions: Vec<Option<Kind>>,
    context: HashMap<String, ContextEntry>,
    position: Option<usize>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum TypeError {
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

impl TypeError {
    pub fn position(&self) -> usize {
        match self {
            TypeError::KindMismatch {
                pos,
                expected: _,
                actual: _,
            } => *pos,
            TypeError::NotInScope { pos, name: _ } => *pos,
        }
    }
    pub fn message(&self) -> String {
        todo!()
    }

    pub fn report(&self, diagnostic: &mut diagnostic::Diagnostic) {
        diagnostic.item(diagnostic::Item {
            pos: self.position(),
            message: self.message(),
        })
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Kind {
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
            context: HashMap::new(),
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

    fn lookup(&self, name: String) -> Result<ContextEntry, TypeError> {
        match self.context.get(&name) {
            None => Err(TypeError::NotInScope {
                pos: self.current_position(),
                name,
            }),
            Some(entry) => Ok(entry.clone()),
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
        }
    }

    fn infer_expr(&mut self, expr: syntax::Expr) -> Result<(core::Expr, syntax::Type), TypeError> {
        todo!()
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
