//! Kind unification.

use crate::metavariables::{self, Meta, Solution};
use ipso_syntax::kind::{Kind, KindCompound};
use std::rc::Rc;

/// A mapping from kind metavariables to their solutions.
#[derive(Default)]
pub struct Solutions(pub metavariables::Solutions<Kind>);

/**
# Preconditions

* [`Kind`] arguments must contain valid metavariables.

  `kind.iter_metas().all(|meta| self.contains(meta))`

  Applies to: [`Solutions::zonk`], [`Solutions::occurs`]
*/
impl Solutions {
    /// See [`metavariables::Solutions::new`]
    pub fn new() -> Self {
        Solutions(metavariables::Solutions::new())
    }

    /// See [`metavariables::Solutions::contains`]
    pub fn contains(&self, meta: Meta) -> bool {
        self.0.contains(meta)
    }

    /// See [`metavariables::Solutions::get`]
    pub fn get(&self, meta: Meta) -> &Solution<Kind> {
        self.0.get(meta)
    }

    /// See [`metavariables::Solutions::set`]
    pub fn set(&mut self, meta: Meta, kind: &Kind) {
        self.0.set(meta, kind)
    }

    /// See [`metavariables::Solutions::fresh_meta`]
    pub fn fresh_meta(&mut self) -> Meta {
        self.0.fresh_meta()
    }

    /**
    Check whether a metavariable occurs in a kind.
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

    If `close_unsolved` is `true`, then any unsolved metavariables are replaced with [`Kind::Type`].

    # Laws

    * All solved metavariables are substituted.

      ```text
      { kind.iter_metas().all(|meta| self.contains(meta)) }

      let kind = self.zonk(close_unsolved, kind);

      { kind.iter_metas().all(|meta| self.contains(meta) && self.get(meta).is_unsolved()) }
      ```

    * When `close_unsolved` is `true`, no metavariables are left in the kind.

      ```text
      { kind.iter_metas().all(|meta| self.contains(meta)) }

      let kind = self.zonk(true, kind);

      { kind.iter_metas().count() == 0 }
      ```
    */
    pub fn zonk(&self, close_unsolved: bool, mut kind: Kind) -> Kind {
        self.zonk_mut(close_unsolved, &mut kind);
        kind
    }

    /**
    A mutable version of [`Solutions::zonk`].
    */
    pub fn zonk_mut(&self, close_unsolved: bool, kind: &mut Kind) {
        fn zonk_compound(solutions: &Solutions, close_unsolved: bool, kind: &mut KindCompound) {
            match kind {
                KindCompound::Arrow(a, b) => {
                    zonk_simple(solutions, close_unsolved, a);
                    zonk_simple(solutions, close_unsolved, b);
                }
            }
        }

        fn zonk_simple(solutions: &Solutions, close_unsolved: bool, kind: &mut Kind) {
            match kind {
                Kind::Type => {}
                Kind::Row => {}
                Kind::Constraint => {}
                Kind::Meta(meta) => match solutions.get(*meta) {
                    Solution::Unsolved => {
                        if close_unsolved {
                            *kind = Kind::Type;
                        }
                    }
                    Solution::Solved(new_kind) => {
                        *kind = new_kind.clone();
                    }
                },
                Kind::Ref(kind_ref) => {
                    zonk_compound(solutions, close_unsolved, Rc::make_mut(kind_ref));
                }
            }
        }

        zonk_simple(self, close_unsolved, kind);
    }
}

/// A kind unification error.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    Mismatch { expected: Kind, actual: Kind },
    Occurs { meta: Meta, kind: Kind },
}

impl Error {
    pub fn mismatch(expected: &Kind, actual: &Kind) -> Self {
        Error::Mismatch {
            expected: expected.clone(),
            actual: actual.clone(),
        }
    }

    pub fn occurs(meta: Meta, kind: &Kind) -> Self {
        Error::Occurs {
            meta,
            kind: kind.clone(),
        }
    }
}

/**
Unify two kinds.

# Preconditions

* [`Kind`] arguments must contain valid metavariables.

# Laws

* Unified types are equalised by solving metavariables.

  ```text
  {
    expected.iter_metas().all(|meta| solutions.contains(meta)) &&
    actual.iter_metas().all(|meta| solutions.contains(meta))
  }

  let result = self.unify(expected, actual);

  { result.is_ok() ==> self.zonk(expected) == self.zonk(actual) }
  ```
*/
pub fn unify(solutions: &mut Solutions, expected: &Kind, actual: &Kind) -> Result<(), Error> {
    fn solve_left(solutions: &mut Solutions, meta: Meta, actual: &Kind) -> Result<(), Error> {
        debug_assert!(match actual {
            Kind::Meta(actual_meta) => {
                solutions.get(*actual_meta).is_unsolved()
            }
            _ => true,
        });

        if solutions.occurs(meta, actual) {
            Err(Error::occurs(meta, actual))
        } else {
            solutions.set(meta, actual);
            Ok(())
        }
    }

    fn solve_right(solutions: &mut Solutions, expected: &Kind, meta: Meta) -> Result<(), Error> {
        debug_assert!(match expected {
            Kind::Meta(expected_meta) => {
                solutions.get(*expected_meta).is_unsolved()
            }
            _ => true,
        });

        if solutions.occurs(meta, expected) {
            Err(Error::occurs(meta, expected))
        } else {
            solutions.set(meta, expected);
            Ok(())
        }
    }

    fn walk(solutions: &Solutions, kind: &Kind) -> Kind {
        match kind {
            Kind::Meta(meta) => match solutions.get(*meta) {
                Solution::Unsolved => kind.clone(),
                Solution::Solved(kind) => walk(solutions, kind),
            },
            _ => kind.clone(),
        }
    }

    fn unify_meta_left(solutions: &mut Solutions, meta: Meta, actual: &Kind) -> Result<(), Error> {
        match walk(solutions, actual) {
            Kind::Meta(actual_meta) if meta == actual_meta => Ok(()),
            actual => match solutions.get(meta).clone() {
                Solution::Unsolved => solve_left(solutions, meta, &actual),
                Solution::Solved(expected) => unify(solutions, &expected, &actual),
            },
        }
    }

    fn unify_meta_right(
        solutions: &mut Solutions,
        expected: &Kind,
        meta: Meta,
    ) -> Result<(), Error> {
        match walk(solutions, expected) {
            Kind::Meta(expected_meta) if meta == expected_meta => Ok(()),
            expected => match solutions.get(meta).clone() {
                Solution::Unsolved => solve_right(solutions, &expected, meta),
                Solution::Solved(actual) => unify(solutions, &expected, &actual),
            },
        }
    }

    match expected {
        Kind::Meta(meta) => unify_meta_left(solutions, *meta, actual),
        Kind::Type => match actual {
            Kind::Meta(meta) => unify_meta_right(solutions, expected, *meta),
            Kind::Type => Ok(()),
            _ => Err(Error::mismatch(expected, actual)),
        },
        Kind::Row => match actual {
            Kind::Meta(meta) => unify_meta_right(solutions, expected, *meta),
            Kind::Row => Ok(()),
            _ => Err(Error::mismatch(expected, actual)),
        },
        Kind::Constraint => match actual {
            Kind::Meta(meta) => unify_meta_right(solutions, expected, *meta),
            Kind::Constraint => Ok(()),
            _ => Err(Error::mismatch(expected, actual)),
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
                _ => Err(Error::mismatch(expected, actual)),
            },
        },
    }
}
