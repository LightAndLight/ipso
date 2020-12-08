use crate::core;
use crate::syntax;
use std::collections::HashMap;

struct ContextEntry {
    index: usize,
    ty: syntax::Type,
}

struct Typechecker {
    context: HashMap<String, ContextEntry>,
}

enum TypeError {
    NotInScope(String),
}

enum Kind {
    Type,
    Row,
    Constraint,
    Arrow(Box<Kind>, Box<Kind>),
}

impl Kind {
    fn mk_arrow(a: Kind, b: Kind) -> Kind {
        Kind::Arrow(Box::new(a), Box::new(b))
    }
}

impl Typechecker {
    fn new() -> Self {
        Typechecker {
            context: HashMap::new(),
        }
    }

    fn module(&self, module: syntax::Module) -> Result<core::Module, TypeError> {
        let decls = module.decls.into_iter().fold(Ok(vec![]), |acc, decl| {
            acc.and_then(|mut decls| {
                self.declaration(decl).and_then(|decl| {
                    decls.push(decl);
                    Ok(decls)
                })
            })
        })?;
        Ok(core::Module { decls })
    }
    fn declaration(&self, decl: syntax::Declaration) -> Result<core::Declaration, TypeError> {
        match decl {
            syntax::Declaration::Definition {
                name,
                ty,
                args,
                body,
            } => {
                let body = self.check_expr(syntax::Expr::mk_lam(args, body), &ty)?;
                Ok(core::Declaration::Definition { name, ty, body })
            }
            syntax::Declaration::TypeAlias { name, args, body } => {
                todo!()
            }
            syntax::Declaration::Import { module, name } => {
                todo!()
            }
            syntax::Declaration::FromImport { module, name } => {
                todo!()
            }
        }
    }
    fn lookup(&self, name: String) -> Result<&ContextEntry, TypeError> {
        match self.context.get(&name) {
            None => Err(TypeError::NotInScope(name)),
            Some(entry) => Ok(entry),
        }
    }
    fn check_kind(&self, ty: &syntax::Type, kind: &Kind) -> Result<(), TypeError> {
        todo!()
    }
    fn infer_kind(&self, ty: &syntax::Type) -> Result<Kind, TypeError> {
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
                    acc.and_then(|()| self.check_kind(constraint, &Kind::Constraint))
                })?;
                Ok(Kind::Constraint)
            }
            syntax::Type::Array => Ok(Kind::mk_arrow(Kind::Type, Kind::Type)),
            syntax::Type::Record(known, rest) => {
                todo!()
            }
            syntax::Type::Variant(known, rest) => {
                todo!()
            }
            syntax::Type::IO => Ok(Kind::mk_arrow(Kind::Type, Kind::Type)),
            syntax::Type::App(a, b) => {
                todo!()
            }
        }
    }
    fn unify_kind(&self, expected: &Kind, actual: &Kind) -> Result<(), TypeError> {
        todo!()
    }
    fn unify_type(&self, expected: &syntax::Type, actual: &syntax::Type) -> Result<(), TypeError> {
        let expected_kind = self.infer_kind(expected)?;
        let actual_kind = self.infer_kind(actual)?;
        self.unify_kind(&expected_kind, &actual_kind)?;
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
            syntax::Type::Record(known, rest) => {
                todo!()
            }
            syntax::Type::Variant(known, rest) => {
                todo!()
            }
            syntax::Type::IO => {
                todo!()
            }
        }
    }
    fn check_expr(&self, expr: syntax::Expr, ty: &syntax::Type) -> Result<core::Expr, TypeError> {
        match expr {
            syntax::Expr::Var(n) => {
                let entry = self.lookup(n)?;
                self.unify_type(ty, &entry.ty)?;
                Ok(core::Expr::Var(entry.index))
            }

            syntax::Expr::App(f, x) => {
                todo!()
            }
            syntax::Expr::Lam { args, body } => {
                todo!()
            }

            syntax::Expr::True => {
                todo!()
            }
            syntax::Expr::False => {
                todo!()
            }
            syntax::Expr::IfThenElse(cond, then_, else_) => {
                todo!()
            }

            syntax::Expr::Int(n) => {
                todo!()
            }

            syntax::Expr::Binop(op, a, b) => {
                todo!()
            }

            syntax::Expr::Char(c) => {
                todo!()
            }

            syntax::Expr::String(parts) => {
                todo!()
            }

            syntax::Expr::Array(items) => {
                todo!()
            }

            syntax::Expr::Record { fields, rest } => {
                todo!()
            }
            syntax::Expr::Project(value, field) => {
                todo!()
            }

            syntax::Expr::Variant(name, args) => {
                todo!()
            }
            syntax::Expr::Case(value, branches) => {
                todo!()
            }
        }
    }
}
