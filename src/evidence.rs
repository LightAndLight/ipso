use crate::syntax;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct EVar(pub usize);

pub struct Evidence<A>(Vec<(Constraint, Option<A>)>);

pub enum Constraint {
    HasField {
        field: String,
        rest: syntax::Type<usize>,
    },
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
