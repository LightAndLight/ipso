//! Kind checking and inference.

use crate::BoundVars;
use ipso_core::{self as core, CommonKinds};
use ipso_syntax::{
    self as syntax,
    kind::{Kind, KindCompound},
};
use std::{collections::HashMap, rc::Rc};

/// A kind metavariable.
pub type Meta = usize;

/// A kind metavariable solution.
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

/**
# Preconditions

* [`Meta`] arguments must be valid.

  `self.contains(meta)`

  Applies to: [`Solutions::get`], [`Solutions::set`]

* [`Kind`] arguments must contain valid metavariables.

  `kind.iter_metas().all(|meta| self.contains(meta))`

  Applies to: [`Solutions::set`], [`Solutions::zonk`], [`Solutions::occurs`]
*/
impl Solutions {
    /**
    Check whether a metavariable is in the [`Solutions`]' domain.
    */
    pub fn contains(&self, meta: Meta) -> bool {
        meta < self.solutions.len()
    }

    /**
    Get a metavariable's solution.
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
    pub fn fresh_meta(&mut self) -> Meta {
        let m = self.solutions.len();
        self.solutions.push(Solution::Unsolved);
        m
    }

    /**
    Check whether the metavariable occurs in a kind.
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

    # Laws

    * All solved metavariables are substituted.

      ```text
      { kind.iter_metas().all(|meta| self.contains(meta)) }

      self.zonk(&mut kind)

      { kind.iter_metas().all(|meta| self.contains(meta) && self.get(meta).is_unsolved()) }
      ```
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

/// A kind unification error.
pub enum UnificationError {
    Mismatch { expected: Kind, actual: Kind },
    Occurs { meta: Meta, kind: Kind },
}

impl UnificationError {
    pub fn mismatch(expected: &Kind, actual: &Kind) -> Self {
        UnificationError::Mismatch {
            expected: expected.clone(),
            actual: actual.clone(),
        }
    }

    pub fn occurs(meta: Meta, kind: &Kind) -> Self {
        UnificationError::Occurs {
            meta,
            kind: kind.clone(),
        }
    }
}

/**
Unify two kinds.

# Preconditions

* [`Kind`] arguments must contain valid metavariables.

  `expected.iter_metas().all(|meta| solutions.contains(meta))`

  `actual.iter_metas().all(|meta| solutions.contains(meta))`

# Laws

* Unified types are equalised by solving metavariables.

  ```text
  { self.unify(expected, actual).is_ok() }

  self.zonk(expected); self.zonk(actual)

  { expected == actual }
  ```
*/
pub fn unify(
    solutions: &mut Solutions,
    expected: &Kind,
    actual: &Kind,
) -> Result<(), UnificationError> {
    fn solve_left(
        solutions: &mut Solutions,
        meta: Meta,
        actual: &Kind,
    ) -> Result<(), UnificationError> {
        if solutions.occurs(meta, actual) {
            Err(UnificationError::occurs(meta, actual))
        } else {
            solutions.set(meta, actual);
            Ok(())
        }
    }

    fn solve_right(
        solutions: &mut Solutions,
        expected: &Kind,
        meta: Meta,
    ) -> Result<(), UnificationError> {
        if solutions.occurs(meta, expected) {
            Err(UnificationError::occurs(meta, expected))
        } else {
            solutions.set(meta, expected);
            Ok(())
        }
    }

    fn unify_meta_left(
        solutions: &mut Solutions,
        meta: Meta,
        actual: &Kind,
    ) -> Result<(), UnificationError> {
        match solutions.get(meta).clone() {
            Solution::Unsolved => solve_left(solutions, meta, actual),
            Solution::Solved(expected) => unify(solutions, &expected, actual),
        }
    }

    fn unify_meta_right(
        solutions: &mut Solutions,
        expected: &Kind,
        meta: Meta,
    ) -> Result<(), UnificationError> {
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
            _ => Err(UnificationError::mismatch(expected, actual)),
        },
        Kind::Row => match actual {
            Kind::Meta(meta) => unify_meta_right(solutions, expected, *meta),
            Kind::Row => Ok(()),
            _ => Err(UnificationError::mismatch(expected, actual)),
        },
        Kind::Constraint => match actual {
            Kind::Meta(meta) => unify_meta_right(solutions, expected, *meta),
            Kind::Constraint => Ok(()),
            _ => Err(UnificationError::mismatch(expected, actual)),
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
                _ => Err(UnificationError::mismatch(expected, actual)),
            },
        },
    }
}

/// A kind inference error.
pub enum InferenceError {
    NotInScope { name: Rc<str> },
    UnificationError(UnificationError),
}

impl From<UnificationError> for InferenceError {
    fn from(err: UnificationError) -> Self {
        InferenceError::UnificationError(err)
    }
}

/// Kind inference context.
pub struct InferenceContext<'a> {
    common_kinds: &'a CommonKinds,
    types: &'a HashMap<Rc<str>, Kind>,
    type_variables: &'a BoundVars<Kind>,
    kind_solutions: &'a mut Solutions,
}

impl<'a> InferenceContext<'a> {
    pub fn new(
        common_kinds: &'a CommonKinds,
        types: &'a HashMap<Rc<str>, Kind>,
        type_variables: &'a BoundVars<Kind>,
        kind_solutions: &'a mut Solutions,
    ) -> Self {
        InferenceContext {
            common_kinds,
            types,
            type_variables,
            kind_solutions,
        }
    }

    /// Generate a fresh kind metavariable.
    pub fn fresh_meta(&mut self) -> Kind {
        Kind::Meta(self.kind_solutions.fresh_meta())
    }

    /// Infer a type's kind.
    pub fn infer(
        &mut self,
        ty: &syntax::Type<Rc<str>>,
    ) -> Result<(core::Type, Kind), InferenceError> {
        match ty {
            syntax::Type::Unit => Ok((core::Type::Unit, Kind::Type)),
            syntax::Type::Bool => Ok((core::Type::Bool, Kind::Type)),
            syntax::Type::Int => Ok((core::Type::Int, Kind::Type)),
            syntax::Type::Char => Ok((core::Type::Char, Kind::Type)),
            syntax::Type::String => Ok((core::Type::String, Kind::Type)),
            syntax::Type::Bytes => Ok((core::Type::Bytes, Kind::Type)),
            syntax::Type::Cmd => Ok((core::Type::Cmd, Kind::Type)),
            syntax::Type::Arrow => {
                let kind = self.common_kinds.type_to_type_to_type.clone();
                Ok((core::Type::Arrow(kind.clone()), kind))
            }
            syntax::Type::Array => {
                let kind = self.common_kinds.type_to_type.clone();
                Ok((core::Type::Array(kind.clone()), kind))
            }
            syntax::Type::IO => {
                let kind = self.common_kinds.type_to_type.clone();
                Ok((core::Type::IO(kind.clone()), kind))
            }

            syntax::Type::Record => {
                let kind = self.common_kinds.row_to_type.clone();
                Ok((core::Type::Record(kind.clone()), kind))
            }
            syntax::Type::Variant => {
                let kind = self.common_kinds.row_to_type.clone();
                Ok((core::Type::Variant(kind.clone()), kind))
            }
            syntax::Type::RowNil => Ok((core::Type::RowNil, Kind::Row)),
            syntax::Type::RowCons(field, ty, rest) => {
                let ty = self.check(ty, &Kind::Type)?;
                let rest = self.check(rest, &Kind::Row)?;
                Ok((core::Type::mk_rowcons(field.clone(), ty, rest), Kind::Row))
            }
            syntax::Type::HasField(field, row) => {
                let row = self.check(row, &Kind::Row)?;
                Ok((
                    core::Type::mk_hasfield(field.clone(), row),
                    Kind::Constraint,
                ))
            }

            syntax::Type::FatArrow => {
                let kind = self.common_kinds.constraint_to_type_to_type.clone();
                Ok((core::Type::FatArrow(kind.clone()), kind))
            }
            syntax::Type::Constraints(constraints) => {
                let constraints: Vec<core::Type> = constraints
                    .iter()
                    .map(|constraint| self.check(constraint, &Kind::Constraint))
                    .collect::<Result<_, _>>()?;
                Ok((core::Type::Constraints(constraints), Kind::Constraint))
            }

            syntax::Type::App(a, b) => {
                let in_kind = self.fresh_meta();
                let out_kind = self.fresh_meta();
                let a = self.check(a, &Kind::mk_arrow(&in_kind, &out_kind))?;
                let b = self.check(b, &in_kind)?;
                Ok((core::Type::mk_app(a, b), out_kind))
            }

            syntax::Type::Name(name) => match self.types.get(name) {
                Some(kind) => Ok((core::Type::Name(kind.clone(), name.clone()), kind.clone())),
                None => Err(InferenceError::NotInScope { name: name.clone() }),
            },
            syntax::Type::Var(name) => match self.type_variables.lookup_name(name) {
                Some((index, kind)) => Ok((core::Type::Var(kind.clone(), index), kind.clone())),
                None => Err(InferenceError::NotInScope { name: name.clone() }),
            },
            syntax::Type::Meta(_) => todo!(),
        }
    }

    /// Check a type's kind.
    pub fn check(
        &mut self,
        ty: &syntax::Type<Rc<str>>,
        expected_kind: &Kind,
    ) -> Result<core::Type, InferenceError> {
        let (ty, actual_kind) = self.infer(ty)?;
        unify(self.kind_solutions, expected_kind, &actual_kind)?;
        Ok(ty)
    }
}

/// Infer a type's kind.
pub fn infer(
    ctx: &mut InferenceContext,
    ty: &syntax::Type<Rc<str>>,
) -> Result<(core::Type, Kind), InferenceError> {
    ctx.infer(ty)
}

/// Check a type's kind.
pub fn check(
    ctx: &mut InferenceContext,
    ty: &syntax::Type<Rc<str>>,
    expected_kind: &Kind,
) -> Result<core::Type, InferenceError> {
    ctx.check(ty, expected_kind)
}
