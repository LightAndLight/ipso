use crate::{eq_zonked_constraint, type_inference};
use ipso_core::{self as core, EVar, Expr, Placeholder};
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Item {
    pub pos: usize,
    pub constraint: Constraint,
    pub expr: Option<Rc<Expr>>,
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Evidence {
    pub evars: Vec<Constraint>,
    pub environment: Vec<Item>,
}

impl Evidence {
    pub fn new() -> Self {
        Evidence {
            evars: Vec::new(),
            environment: Vec::new(),
        }
    }

    pub fn placeholder(&mut self, pos: usize, constraint: Constraint) -> Placeholder {
        let ix = self.environment.len();
        self.environment.push(Item {
            pos,
            constraint,
            expr: None,
        });
        Placeholder(ix)
    }

    pub fn assume(&mut self, pos: usize, constraint: Constraint) -> EVar {
        let ev = EVar(self.evars.len());
        self.evars.push(constraint.clone());
        self.environment.push(Item {
            pos,
            constraint,
            expr: Some(Rc::new(Expr::EVar(ev))),
        });
        ev
    }

    pub fn find(
        &self,
        type_solutions: &type_inference::unification::Solutions,
        constraint: &Constraint,
    ) -> Option<Rc<Expr>> {
        self.environment.iter().find_map(|c| {
            if eq_zonked_constraint(type_solutions, &c.constraint, constraint) {
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
