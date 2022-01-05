//! Kind checking and inference.

#[cfg(test)]
mod test;

use crate::{
    metavariables::{self, Meta, Solution},
    BoundVars,
};
use ipso_core::{self as core, CommonKinds};
use ipso_syntax::{
    self as syntax,
    kind::{Kind, KindCompound},
};
use std::{collections::HashMap, rc::Rc};

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

/// Extra context for kind inference errors.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InferenceErrorHint {
    WhileChecking {
        ty: syntax::Type<Rc<str>>,
        has_kind: Kind,
    },
    WhileInferring {
        ty: syntax::Type<Rc<str>>,
    },
}

/// Kind inference error information.
#[derive(Debug, PartialEq, Eq)]
pub enum InferenceErrorInfo {
    NotInScope { name: Rc<str> },
    UnificationError { error: UnificationError },
}

impl From<UnificationError> for InferenceErrorInfo {
    fn from(error: UnificationError) -> Self {
        InferenceErrorInfo::UnificationError { error }
    }
}

/// A kind inference error.
#[derive(Debug, PartialEq, Eq)]
pub struct InferenceError {
    pub info: InferenceErrorInfo,
    pub hint: Option<InferenceErrorHint>,
}

impl InferenceError {
    /// Construct a [`NotInScope`](InferenceErrorInfo::NotInScope) error.
    pub fn not_in_scope(name: Rc<str>) -> Self {
        InferenceError {
            info: InferenceErrorInfo::NotInScope { name },
            hint: None,
        }
    }

    /// Construct a [`Mismatch`](UnificationError::Mismatch) error.
    pub fn mismatch(expected: &Kind, actual: &Kind) -> Self {
        InferenceError {
            info: InferenceErrorInfo::UnificationError {
                error: UnificationError::Mismatch {
                    expected: expected.clone(),
                    actual: actual.clone(),
                },
            },
            hint: None,
        }
    }

    /// Construct an [`Occurs`](UnificationError::Occurs) error.
    pub fn occurs(meta: Meta, kind: &Kind) -> Self {
        InferenceError {
            info: InferenceErrorInfo::UnificationError {
                error: UnificationError::Occurs {
                    meta,
                    kind: kind.clone(),
                },
            },
            hint: None,
        }
    }

    /// Attach a [`hint`](InferenceErrorHint) to an [`InferenceError`].
    pub fn with_hint(mut self, hint: InferenceErrorHint) -> Self {
        self.hint = Some(hint);
        self
    }
}

impl<T: Into<InferenceErrorInfo>> From<T> for InferenceError {
    fn from(error: T) -> Self {
        InferenceError {
            info: error.into(),
            hint: None,
        }
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

    /// Unify two kinds.
    pub fn unify(
        &mut self,
        hint: &dyn Fn() -> InferenceErrorHint,
        expected: &Kind,
        actual: &Kind,
    ) -> Result<(), InferenceError> {
        unify(self.kind_solutions, expected, actual)
            .map_err(|err| InferenceError::from(err).with_hint(hint()))
    }

    /// Infer a type's kind.
    pub fn infer(
        &mut self,
        hint: &dyn Fn() -> InferenceErrorHint,
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
                let ty = self.check(hint, ty, &Kind::Type)?;
                let rest = self.check(hint, rest, &Kind::Row)?;
                Ok((core::Type::mk_rowcons(field.clone(), ty, rest), Kind::Row))
            }
            syntax::Type::HasField(field, row) => {
                let row = self.check(hint, row, &Kind::Row)?;
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
                    .map(|constraint| self.check(hint, constraint, &Kind::Constraint))
                    .collect::<Result<_, _>>()?;
                Ok((core::Type::Constraints(constraints), Kind::Constraint))
            }

            syntax::Type::App(a, b) => {
                let in_kind = self.fresh_meta();
                let out_kind = self.fresh_meta();
                let a = self.check(hint, a, &Kind::mk_arrow(&in_kind, &out_kind))?;
                let b = self.check(hint, b, &in_kind)?;
                Ok((core::Type::mk_app(a, b), out_kind))
            }

            syntax::Type::Name(name) => match self.types.get(name) {
                Some(kind) => Ok((core::Type::Name(kind.clone(), name.clone()), kind.clone())),
                None => Err(InferenceError::not_in_scope(name.clone()).with_hint(hint())),
            },
            syntax::Type::Var(name) => match self.type_variables.lookup_name(name) {
                Some((index, kind)) => Ok((core::Type::Var(kind.clone(), index), kind.clone())),
                None => Err(InferenceError::not_in_scope(name.clone()).with_hint(hint())),
            },
            syntax::Type::Meta(_) => todo!(),
        }
    }

    /// Check a type's kind.
    pub fn check(
        &mut self,
        hint: &dyn Fn() -> InferenceErrorHint,
        ty: &syntax::Type<Rc<str>>,
        expected_kind: &Kind,
    ) -> Result<core::Type, InferenceError> {
        let (ty, actual_kind) = self.infer(hint, ty)?;
        self.unify(hint, expected_kind, &actual_kind)?;
        Ok(ty)
    }
}

/// Infer a type's kind.
pub fn infer(
    ctx: &mut InferenceContext,
    ty: &syntax::Type<Rc<str>>,
) -> Result<(core::Type, Kind), InferenceError> {
    ctx.infer(
        &|| InferenceErrorHint::WhileInferring { ty: ty.clone() },
        ty,
    )
}

/// Check a type's kind.
pub fn check(
    ctx: &mut InferenceContext,
    ty: &syntax::Type<Rc<str>>,
    expected_kind: &Kind,
) -> Result<core::Type, InferenceError> {
    ctx.check(
        &|| InferenceErrorHint::WhileChecking {
            ty: ty.clone(),
            has_kind: expected_kind.clone(),
        },
        ty,
        expected_kind,
    )
}
