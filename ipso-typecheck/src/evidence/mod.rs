pub mod solver;

use crate::Typechecker;
use ipso_core::{self as core, EVar, Expr, Placeholder};
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
struct Item {
    pos: Option<usize>,
    constraint: Constraint,
    expr: Option<Expr>,
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Evidence {
    evars: Vec<Constraint>,
    environment: Vec<Item>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Constraint {
    HasField { field: Rc<str>, rest: core::Type },
    Type(core::Type),
}

impl Constraint {
    pub fn from_type(ty: &core::Type) -> Self {
        match ty {
            core::Type::HasField(field, rest) => Constraint::HasField {
                field: field.clone(),
                rest: rest.as_ref().clone(),
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
    pub fn new() -> Self {
        Evidence {
            evars: Vec::new(),
            environment: Vec::new(),
        }
    }

    pub fn placeholder(&mut self, pos: Option<usize>, constraint: Constraint) -> Placeholder {
        let ix = self.environment.len();
        self.environment.push(Item {
            pos,
            constraint,
            expr: None,
        });
        Placeholder(ix)
    }

    pub fn assume(&mut self, pos: Option<usize>, constraint: Constraint) -> EVar {
        let ev = EVar(self.evars.len());
        self.evars.push(constraint.clone());
        self.environment.push(Item {
            pos,
            constraint,
            expr: Some(Expr::EVar(ev)),
        });
        ev
    }

    pub fn find(&self, tc: &Typechecker, constraint: &Constraint) -> Option<Expr> {
        self.environment.iter().find_map(|c| {
            if tc.eq_zonked_constraint(&c.constraint, constraint) {
                c.expr.clone()
            } else {
                None
            }
        })
    }

    pub fn lookup_evar<'a>(&'a self, ev: &EVar) -> Option<&'a Constraint> {
        self.evars.get(ev.0)
    }
}
