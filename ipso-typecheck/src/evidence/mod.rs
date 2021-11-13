use std::rc::Rc;

use crate::Typechecker;
use ipso_core::{self as core, EVar, Expr, Placeholder};
use ipso_syntax::{kind::Kind, r#type::Type};
pub mod solver;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Evidence {
    evars: Vec<Constraint>,
    environment: Vec<(Constraint, Option<usize>, Option<Expr>)>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Constraint {
    HasField { field: Rc<str>, rest: core::Type },
    Type(core::Type),
}

impl Constraint {
    pub fn from_type(ty: &core::Type) -> Self {
        debug_assert!(
            ty.get_kind() == &Kind::Constraint,
            "{:?} is not Kind::Constraint",
            ty
        );

        match ty.get_value() {
            Type::HasField(field, rest) => Constraint::HasField {
                field: field.clone(),
                rest: core::Type::unsafe_new(rest.as_ref().clone(), Kind::Row),
            },
            _ => Constraint::Type(ty.clone()),
        }
    }

    pub fn to_type(&self) -> core::Type {
        match self {
            Constraint::HasField { field, rest } => {
                core::Type::mk_hasfield(field.clone(), rest.clone())
            }
            Constraint::Type(ty) => ty.clone(),
        }
    }
}

impl Evidence {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Evidence {
            evars: Vec::new(),
            environment: Vec::new(),
        }
    }

    pub fn placeholder(&mut self, pos: Option<usize>, constraint: Constraint) -> Placeholder {
        let ix = self.environment.len();
        self.environment.push((constraint, pos, None));
        Placeholder(ix)
    }

    pub fn assume(&mut self, pos: Option<usize>, constraint: Constraint) -> EVar {
        let ev = EVar(self.evars.len());
        self.evars.push(constraint.clone());
        self.environment
            .push((constraint, pos, Some(Expr::EVar(ev))));
        ev
    }

    pub fn find(&self, tc: &Typechecker, constraint: &Constraint) -> Option<Expr> {
        self.environment.iter().find_map(|c| {
            if tc.eq_zonked_constraint(&c.0, constraint) {
                c.2.clone()
            } else {
                None
            }
        })
    }

    pub fn lookup_evar<'a>(&'a self, ev: &EVar) -> Option<&'a Constraint> {
        self.evars.get(ev.0)
    }
}
