use crate::{
    core::{EVar, Expr, Placeholder},
    syntax::{self, Type},
    typecheck::Typechecker,
};
pub mod solver;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Evidence {
    evars: usize,
    environment: Vec<(Constraint, Option<usize>, Option<Expr>)>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Constraint {
    HasField {
        field: String,
        rest: syntax::Type<usize>,
    },
    Type(Type<usize>),
}

impl Constraint {
    pub fn from_type(ty: &Type<usize>) -> Self {
        match ty {
            Type::HasField(field, rest) => Constraint::HasField {
                field: field.clone(),
                rest: (**rest).clone(),
            },
            _ => Constraint::Type(ty.clone()),
        }
    }

    pub fn to_type(&self) -> Type<usize> {
        match self {
            Constraint::HasField { field, rest } => Type::mk_hasfield(field.clone(), rest.clone()),
            Constraint::Type(ty) => ty.clone(),
        }
    }
}

impl Evidence {
    pub fn new() -> Self {
        Evidence {
            evars: 0,
            environment: Vec::new(),
        }
    }

    pub fn placeholder(&mut self, pos: Option<usize>, constraint: Constraint) -> Placeholder {
        let ix = self.environment.len();
        self.environment.push((constraint, pos, None));
        Placeholder(ix)
    }

    pub fn assume(&mut self, pos: Option<usize>, constraint: Constraint) -> EVar {
        let ev = self.evars;
        self.evars += 1;
        let ev = EVar(ev);
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
}
