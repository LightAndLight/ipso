use std::rc::Rc;

use ipso_syntax::kind::{Kind, KindCompound};

/// The type of kind metavariables.
pub type Meta = usize;

/// The type of kind metavariable solutions.
#[derive(Clone)]
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

    # Preconditions

    * `self.contains(meta)`
    */
    pub fn get(&self, meta: Meta) -> &Solution {
        self.solutions
            .get(meta)
            .unwrap_or_else(|| panic!("meta {:?} not found", meta))
    }

    /**
    Set a metavariable's solution.

    Each metavariable can only set once.

    # Preconditions

    * `self.contains(meta)`
    * `self.get(meta).is_unsolved()`
    */
    pub fn set(&mut self, meta: Meta, kind: &Kind) {
        let solution = self
            .solutions
            .get_mut(meta)
            .unwrap_or_else(|| panic!("meta {:?} not found", meta));
        if solution.is_unsolved() {
            *solution = Solution::Solved(kind.clone());
        } else {
            panic!("meta {:?} has already been set", meta);
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
    Check whether the metavariable occurs in a kind.

    # Preconditions

    * `kind.iter_metas().all(|meta| self.contains(meta))`
    */
    pub fn occurs(&self, meta: Meta, kind: &Kind) -> bool {
        match kind {
            Kind::Type => false,
            Kind::Row => false,
            Kind::Constraint => false,
            Kind::Meta(other_meta) => {
                meta == *other_meta
                    || match self.get(meta) {
                        Solution::Unsolved => false,
                        Solution::Solved(kind) => self.occurs(meta, kind),
                    }
            }
            Kind::Ref(kind) => match kind.as_ref() {
                KindCompound::Arrow(a, b) => self.occurs(meta, a) || self.occurs(meta, b),
            },
        }
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
            Kind::Meta(meta) => match self.get(*meta) {
                Solution::Unsolved => {}
                Solution::Solved(new_kind) => {
                    *kind = new_kind.clone();
                }
            },
            Kind::Ref(kind_ref) => {
                zonk_compound(self, Rc::make_mut(kind_ref));
            }
        }
    }
}

pub enum UnifyError {
    Mismatch { expected: Kind, actual: Kind },
    Occurs { meta: Meta, kind: Kind },
}

impl UnifyError {
    pub fn mismatch(expected: &Kind, actual: &Kind) -> Self {
        UnifyError::Mismatch {
            expected: expected.clone(),
            actual: actual.clone(),
        }
    }

    pub fn occurs(meta: Meta, kind: &Kind) -> Self {
        UnifyError::Occurs {
            meta,
            kind: kind.clone(),
        }
    }
}

pub fn unify(solutions: &mut Solutions, expected: &Kind, actual: &Kind) -> Result<(), UnifyError> {
    fn solve_left(solutions: &mut Solutions, meta: Meta, actual: &Kind) -> Result<(), UnifyError> {
        if solutions.occurs(meta, actual) {
            Err(UnifyError::occurs(meta, actual))
        } else {
            solutions.set(meta, actual);
            Ok(())
        }
    }

    fn solve_right(
        solutions: &mut Solutions,
        expected: &Kind,
        meta: Meta,
    ) -> Result<(), UnifyError> {
        if solutions.occurs(meta, expected) {
            Err(UnifyError::occurs(meta, expected))
        } else {
            solutions.set(meta, expected);
            Ok(())
        }
    }

    fn unify_meta_left(
        solutions: &mut Solutions,
        meta: Meta,
        actual: &Kind,
    ) -> Result<(), UnifyError> {
        match solutions.get(meta).clone() {
            Solution::Unsolved => solve_left(solutions, meta, actual),
            Solution::Solved(expected) => unify(solutions, &expected, actual),
        }
    }

    fn unify_meta_right(
        solutions: &mut Solutions,
        expected: &Kind,
        meta: Meta,
    ) -> Result<(), UnifyError> {
        match solutions.get(meta).clone() {
            Solution::Unsolved => solve_right(solutions, expected, meta),
            Solution::Solved(actual) => unify(solutions, expected, &actual),
        }
    }

    match expected {
        Kind::Meta(meta) => unify_meta_left(solutions, *meta, actual),
        Kind::Type => match actual {
            Kind::Meta(meta) => unify_meta_right(solutions, expected, *meta),
            Kind::Type => Ok(()),
            _ => Err(UnifyError::mismatch(expected, actual)),
        },
        Kind::Row => match actual {
            Kind::Meta(meta) => unify_meta_right(solutions, expected, *meta),
            Kind::Row => Ok(()),
            _ => Err(UnifyError::mismatch(expected, actual)),
        },
        Kind::Constraint => match actual {
            Kind::Meta(meta) => unify_meta_right(solutions, expected, *meta),
            Kind::Constraint => Ok(()),
            _ => Err(UnifyError::mismatch(expected, actual)),
        },
        Kind::Ref(expected_ref) => match expected_ref.as_ref() {
            KindCompound::Arrow(expected_a, expected_b) => match actual {
                Kind::Meta(meta) => unify_meta_right(solutions, expected, *meta),
                Kind::Ref(actual_ref) => match actual_ref.as_ref() {
                    KindCompound::Arrow(actual_a, actual_b) => {
                        unify(solutions, expected_a, actual_a)?;
                        unify(solutions, expected_b, actual_b)
                    }
                },
                _ => Err(UnifyError::mismatch(expected, actual)),
            },
        },
    }
}
