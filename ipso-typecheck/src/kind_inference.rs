use std::rc::Rc;

use ipso_syntax::kind::{Kind, KindCompound};

/// The type of kind metavariables.
pub type Meta = usize;

/// The type of kind metavariable solutions.
pub enum Solution {
    Unsolved,
    Solved(Kind),
}

impl Solution {
    fn is_unsolved(&self) -> bool {
        match self {
            Solution::Unsolved => true,
            Solution::Solved(_) => false,
        }
    }
}

/// A mapping from kind metavariables to their solutions.
pub struct Solutions {
    solutions: Vec<Solution>,
}

impl Solutions {
    /**
    Check whether a metavariable is in the [`Solutions`]' domain.
    */
    pub fn contains(&self, meta: Meta) -> bool {
        meta < self.solutions.len()
    }

    /**
    Get a metavariable's solution.

    Returns [`None`] if the metavariable is not in the [`Solutions`]' domain.
    */
    pub fn get(&self, meta: Meta) -> Option<&Solution> {
        self.solutions.get(meta)
    }

    /**
    Set a metavariable's solution.

    Each metavariable can only set once.

    # Preconditions

    * `self.contains(meta)`
    * `self.get(meta).unwrap().is_unsolved()`
    */
    pub fn set(&mut self, meta: Meta, kind: &Kind) {
        match self.solutions.get_mut(meta) {
            None => {
                panic!("meta {:?} not found", meta)
            }
            Some(solution) => {
                if solution.is_unsolved() {
                    *solution = Solution::Solved(kind.clone());
                } else {
                    panic!("meta {:?} has already been set", meta);
                }
            }
        }
    }

    /**
    Generate a new, unsolved metavariable.
    */
    pub fn fresh(&mut self) -> Meta {
        let m = self.solutions.len();
        self.solutions.push(Solution::Unsolved);
        m
    }

    /**
    Substitute all solved metavariables in a kind.

    # Preconditions

    * `kind.iter_metas().all(|meta| self.contains(meta))`

    # Laws

    * Idempotence

      `{ self.zonk(&mut kind); self.zonk(&mut kind); kind } == { self.zonk(&mut kind); kind }`
    */
    pub fn zonk(&self, kind: &mut Kind) {
        fn zonk_compound(solutions: &Solutions, kind: &mut KindCompound) {
            match kind {
                KindCompound::Arrow(a, b) => {
                    solutions.zonk(a);
                    solutions.zonk(b);
                }
            }
        }

        match kind {
            Kind::Type => {}
            Kind::Row => {}
            Kind::Constraint => {}
            Kind::Meta(m) => match self.get(*m) {
                Some(solution) => match solution {
                    Solution::Unsolved => {}
                    Solution::Solved(new_kind) => {
                        *kind = new_kind.clone();
                    }
                },
                None => {
                    panic!("meta {:?} not found", m);
                }
            },
            Kind::Ref(kind_ref) => {
                zonk_compound(self, Rc::make_mut(kind_ref));
            }
        }
    }
}
