use crate::syntax::{self, Type};
pub mod solver;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct EVar(pub usize);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Evidence<A>(Vec<(Constraint, Option<A>)>);

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
}

impl<A> Evidence<A> {
    pub fn new() -> Self {
        Evidence(Vec::new())
    }

    pub fn fresh_evar(&mut self, constraint: Constraint) -> EVar {
        let ix = self.0.len();
        self.0.push((constraint, None));
        EVar(ix)
    }
}
