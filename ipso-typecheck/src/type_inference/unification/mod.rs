//! Type unification.

use crate::{
    kind_inference,
    metavariables::{self, Meta, Solution},
    BoundVars,
};
use ipso_core::{CommonKinds, Type};

use ipso_rope::Rope;
use ipso_syntax::{self as syntax, kind::Kind};
use std::{collections::HashMap, rc::Rc};

/// A mapping from type metavariables to their solutions.
#[derive(Default)]
pub struct Solutions(pub metavariables::Solutions<Type>);

/**
# Preconditions

* [`Type`] arguments must contain valid metavariables.

  `ty.iter_metas().all(|meta| self.contains(meta))`

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
    pub fn get(&self, meta: Meta) -> &Solution<Type> {
        self.0.get(meta)
    }

    /// See [`metavariables::Solutions::set`]
    pub fn set(&mut self, meta: Meta, ty: &Type) {
        self.0.set(meta, ty)
    }

    /// See [`metavariables::Solutions::fresh_meta`]
    pub fn fresh_meta(&mut self) -> Meta {
        self.0.fresh_meta()
    }

    /**
    Check whether a metavariable occurs in a type.
    */
    pub fn occurs(&self, meta: Meta, ty: &Type) -> bool {
        match ty {
            Type::Bool
            | Type::Int
            | Type::Char
            | Type::String
            | Type::Bytes
            | Type::RowNil
            | Type::Unit
            | Type::Cmd
            | Type::Arrow(_)
            | Type::FatArrow(_)
            | Type::Array(_)
            | Type::Record(_)
            | Type::Variant(_)
            | Type::IO(_)
            | Type::Name(_, _)
            | Type::Var(_, _) => false,
            Type::Constraints(constraints) => constraints
                .iter()
                .any(|constraint| self.occurs(meta, constraint)),
            Type::RowCons(_, ty, rest) => self.occurs(meta, ty) || self.occurs(meta, rest),
            Type::HasField(_, rest) => self.occurs(meta, rest),
            Type::App(_, a, b) => self.occurs(meta, a) || self.occurs(meta, b),
            Type::Meta(_, other_meta) => match self.get(*other_meta) {
                Solution::Unsolved => meta == *other_meta,
                Solution::Solved(ty) => self.occurs(meta, ty),
            },
        }
    }

    /**
    Substitute all solved metavariables in a type.

    # Laws

    * All solved metavariables are substituted.

      ```text
      { ty.iter_metas().all(|meta| self.contains(meta)) }

      let ty = self.zonk(ty);

      { ty.iter_metas().all(|meta| self.contains(meta) && self.get(meta).is_unsolved()) }
      ```
    */
    pub fn zonk(
        &self,
        kind_solutions: &kind_inference::unification::Solutions,
        mut ty: Type,
    ) -> Type {
        self.zonk_mut(kind_solutions, &mut ty);
        ty
    }

    /**
    A mutable version of [`Solutions::zonk`].
    */
    pub fn zonk_mut(&self, kind_solutions: &kind_inference::unification::Solutions, ty: &mut Type) {
        match ty {
            Type::Bool
            | Type::Int
            | Type::Char
            | Type::String
            | Type::Bytes
            | Type::RowNil
            | Type::Unit
            | Type::Cmd => {}
            Type::Constraints(constraints) => constraints.iter_mut().for_each(|constraint| {
                self.zonk_mut(kind_solutions, constraint);
            }),
            Type::RowCons(_field, ty, rest) => {
                self.zonk_mut(kind_solutions, Rc::make_mut(ty));
                self.zonk_mut(kind_solutions, Rc::make_mut(rest));
            }
            Type::HasField(_field, rest) => {
                self.zonk_mut(kind_solutions, Rc::make_mut(rest));
            }
            Type::Arrow(kind)
            | Type::FatArrow(kind)
            | Type::Array(kind)
            | Type::Record(kind)
            | Type::Variant(kind)
            | Type::IO(kind)
            | Type::Name(kind, _)
            | Type::Var(kind, _) => {
                kind_solutions.zonk_mut(false, kind);
            }
            Type::App(kind, a, b) => {
                kind_solutions.zonk_mut(false, kind);
                self.zonk_mut(kind_solutions, Rc::make_mut(a));
                self.zonk_mut(kind_solutions, Rc::make_mut(b));
            }
            Type::Meta(kind, meta) => match self.get(*meta) {
                Solution::Unsolved => {
                    kind_solutions.zonk_mut(false, kind);
                }
                Solution::Solved(new_ty) => {
                    *ty = new_ty.clone();
                    self.zonk_mut(kind_solutions, ty);
                }
            },
        }
    }
}

/// Type unification error hints.
#[derive(PartialEq, Eq, Debug)]
pub enum ErrorHint {
    WhileUnifying {
        expected: syntax::Type<Rc<str>>,
        actual: syntax::Type<Rc<str>>,
    },
}

/// Type unification errors.
#[derive(PartialEq, Eq, Debug)]
pub struct Error {
    pub info: ErrorInfo,
    pub hint: Option<ErrorHint>,
}

impl Error {
    pub fn with_hint(mut self, hint: ErrorHint) -> Self {
        self.hint = Some(hint);
        self
    }

    pub fn occurs(meta: Meta, ty: syntax::Type<Rc<str>>) -> Self {
        Self::from(ErrorInfo::Occurs { meta, ty })
    }

    pub fn mismatch(expected: syntax::Type<Rc<str>>, actual: syntax::Type<Rc<str>>) -> Self {
        Self::from(ErrorInfo::Mismatch { expected, actual })
    }
}

impl From<ErrorInfo> for Error {
    fn from(info: ErrorInfo) -> Self {
        Error { info, hint: None }
    }
}

/// Type unification error info.
#[derive(PartialEq, Eq, Debug)]
pub enum ErrorInfo {
    Mismatch {
        expected: syntax::Type<Rc<str>>,
        actual: syntax::Type<Rc<str>>,
    },
    Occurs {
        meta: Meta,
        ty: syntax::Type<Rc<str>>,
    },
    KindError {
        error: kind_inference::Error,
    },
}

impl ErrorInfo {
    /**
    Construct an [`ErrorInfo::Mismatch`].

    Uses `type_variables` to replace de Bruijn indices with names.
    */
    pub fn mismatch(
        kind_solutions: &kind_inference::unification::Solutions,
        type_solutions: &Solutions,
        type_variables: &BoundVars<Kind>,
        expected: Type,
        actual: Type,
    ) -> Self {
        ErrorInfo::Mismatch {
            expected: type_solutions
                .zonk(kind_solutions, expected)
                .to_syntax()
                .map(&mut |ix| type_variables.lookup_index(*ix).unwrap().0.clone()),
            actual: type_solutions
                .zonk(kind_solutions, actual)
                .to_syntax()
                .map(&mut |ix| type_variables.lookup_index(*ix).unwrap().0.clone()),
        }
    }

    /**
    Construct an [`ErrorInfo::Occurs`].

    Uses `type_variables` to replace de Bruijn indices with names.
    */
    pub fn occurs(type_variables: &BoundVars<Kind>, meta: Meta, ty: &Type) -> Self {
        ErrorInfo::Occurs {
            meta,
            ty: ty
                .to_syntax()
                .map(&mut |ix| type_variables.lookup_index(*ix).unwrap().0.clone()),
        }
    }
}

/// Type unification environment.
#[derive(Clone, Copy)]
pub struct Env<'a> {
    pub common_kinds: &'a CommonKinds,
    pub types: &'a HashMap<Rc<str>, Kind>,
    pub type_variables: &'a BoundVars<Kind>,
}

/// Unify two types, with error hint.
pub fn unify_with_hint(
    env: Env,
    kind_inference_state: &mut kind_inference::State,
    type_solutions: &mut Solutions,
    hint: &dyn Fn() -> ErrorHint,
    expected: &Type,
    actual: &Type,
) -> Result<(), Error> {
    unify_inner(
        env.common_kinds,
        env.types,
        env.type_variables,
        kind_inference_state,
        type_solutions,
        expected,
        actual,
    )
    .map_err(|error| Error::from(error).with_hint(hint()))
}

/// Unify two types.
pub fn unify(
    env: Env,
    kind_inference_state: &mut kind_inference::State,
    type_solutions: &mut Solutions,
    expected: &Type,
    actual: &Type,
) -> Result<(), Error> {
    unify_inner(
        env.common_kinds,
        env.types,
        env.type_variables,
        kind_inference_state,
        type_solutions,
        expected,
        actual,
    )
    .map_err(|error| {
        Error::from(error).with_hint({
            ErrorHint::WhileUnifying {
                expected: type_solutions
                    .zonk(&kind_inference_state.kind_solutions, expected.clone())
                    .to_syntax()
                    .map(&mut |ix| env.type_variables.lookup_index(*ix).unwrap().0.clone()),

                actual: type_solutions
                    .zonk(&kind_inference_state.kind_solutions, actual.clone())
                    .to_syntax()
                    .map(&mut |ix| env.type_variables.lookup_index(*ix).unwrap().0.clone()),
            }
        })
    })
}

fn unify_inner(
    common_kinds: &CommonKinds,
    types: &HashMap<Rc<str>, Kind>,
    type_variables: &BoundVars<Kind>,
    kind_inference_state: &mut kind_inference::State,
    type_solutions: &mut Solutions,
    expected: &Type,
    actual: &Type,
) -> Result<(), ErrorInfo> {
    fn solve_left(
        kind_inference_state: &mut kind_inference::State,
        type_variables: &BoundVars<Kind>,
        type_solutions: &mut Solutions,
        meta: Meta,
        actual: &Type,
    ) -> Result<(), ErrorInfo> {
        /*
        [note: avoiding solved metas as solutions]

        If `actual` is a solved meta, then it's possible for its solution
        to be `Type::Meta(_, meta)`. This would trigger the occurs check, even
        though the equation is valid.

        To keep the occurs check simple, we assume that if a solution is a metavariable
        then it should be unsolved.
        */
        debug_assert!(match actual {
            Type::Meta(_, actual_meta) => {
                type_solutions.get(*actual_meta).is_unsolved()
            }
            _ => true,
        });

        if type_solutions.occurs(meta, actual) {
            Err(ErrorInfo::occurs(
                type_variables,
                meta,
                &type_solutions.zonk(&kind_inference_state.kind_solutions, actual.clone()),
            ))
        } else {
            type_solutions.set(meta, actual);
            Ok(())
        }
    }

    fn solve_right(
        kind_inference_state: &mut kind_inference::State,
        type_variables: &BoundVars<Kind>,
        type_solutions: &mut Solutions,
        expected: &Type,
        meta: Meta,
    ) -> Result<(), ErrorInfo> {
        // See [note: avoiding solved metas as solutions]
        debug_assert!(match expected {
            Type::Meta(_, expected_meta) => {
                type_solutions.get(*expected_meta).is_unsolved()
            }
            _ => true,
        });

        if type_solutions.occurs(meta, expected) {
            Err(ErrorInfo::occurs(
                type_variables,
                meta,
                &type_solutions.zonk(&kind_inference_state.kind_solutions, expected.clone()),
            ))
        } else {
            type_solutions.set(meta, expected);
            Ok(())
        }
    }

    fn walk(type_solutions: &Solutions, ty: &Type) -> Type {
        match ty {
            Type::Meta(_, meta) => match type_solutions.get(*meta) {
                Solution::Unsolved => ty.clone(),
                Solution::Solved(ty) => walk(type_solutions, ty),
            },
            _ => ty.clone(),
        }
    }

    fn unify_meta_left(
        common_kinds: &CommonKinds,
        types: &HashMap<Rc<str>, Kind>,
        type_variables: &BoundVars<Kind>,
        kind_inference_state: &mut kind_inference::State,
        type_solutions: &mut Solutions,
        meta: &usize,
        actual: &Type,
    ) -> Result<(), ErrorInfo> {
        match walk(type_solutions, actual) {
            Type::Meta(_, actual_meta) if *meta == actual_meta => Ok(()),
            actual => match type_solutions.get(*meta).clone() {
                Solution::Unsolved => solve_left(
                    kind_inference_state,
                    type_variables,
                    type_solutions,
                    *meta,
                    &actual,
                ),
                Solution::Solved(expected) => unify_inner(
                    common_kinds,
                    types,
                    type_variables,
                    kind_inference_state,
                    type_solutions,
                    &expected,
                    &actual,
                ),
            },
        }
    }

    fn unify_meta_right(
        common_kinds: &CommonKinds,
        types: &HashMap<Rc<str>, Kind>,
        type_variables: &BoundVars<Kind>,
        kind_inference_state: &mut kind_inference::State,
        type_solutions: &mut Solutions,
        expected: &Type,
        meta: &usize,
    ) -> Result<(), ErrorInfo> {
        match walk(type_solutions, expected) {
            Type::Meta(_, expected_meta) if *meta == expected_meta => Ok(()),
            expected => match type_solutions.get(*meta).clone() {
                Solution::Unsolved => solve_right(
                    kind_inference_state,
                    type_variables,
                    type_solutions,
                    &expected,
                    *meta,
                ),
                Solution::Solved(actual) => unify_inner(
                    common_kinds,
                    types,
                    type_variables,
                    kind_inference_state,
                    type_solutions,
                    &expected,
                    &actual,
                ),
            },
        }
    }

    kind_inference::unification::unify(
        &mut kind_inference_state.kind_solutions,
        &expected.kind(),
        &actual.kind(),
    )
    .map_err(|error| ErrorInfo::KindError {
        error: kind_inference::Error::from(error).with_hint(
            kind_inference::ErrorHint::WhileChecking {
                ty: actual
                    .to_syntax()
                    .map(&mut |ix| type_variables.lookup_index(*ix).unwrap().0.clone()),
                has_kind: expected.kind(),
            },
        ),
    })?;
    /*

    kind_inference::unify_with_hint(kind_inference_state, hint, &expected.kind(), &actual.kind())
        .map_err(|error| ErrorInfo::KindError { error })?;
        */

    match expected {
        Type::Meta(_, meta) => unify_meta_left(
            common_kinds,
            types,
            type_variables,
            kind_inference_state,
            type_solutions,
            meta,
            actual,
        ),
        Type::Bool => match actual {
            Type::Bool => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::Int => match actual {
            Type::Int => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::Char => match actual {
            Type::Char => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::String => match actual {
            Type::String => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::Bytes => match actual {
            Type::Bytes => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::RowNil => match actual {
            Type::RowNil => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::Unit => match actual {
            Type::Unit => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::Cmd => match actual {
            Type::Cmd => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::Name(_, expected_name) => match actual {
            Type::Name(_, actual_name) if expected_name == actual_name => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::Var(_, expected_index) => match actual {
            Type::Var(_, actual_index) if expected_index == actual_index => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::Arrow(_) => match actual {
            Type::Arrow(_) => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::FatArrow(_) => match actual {
            Type::FatArrow(_) => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::Array(_) => match actual {
            Type::Array(_) => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::Record(_) => match actual {
            Type::Record(_) => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::Variant(_) => match actual {
            Type::Variant(_) => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::IO(_) => match actual {
            Type::IO(_) => Ok(()),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::App(_, expected_a, expected_b) => match actual {
            Type::App(_, actual_a, actual_b) => {
                unify_inner(
                    common_kinds,
                    types,
                    type_variables,
                    kind_inference_state,
                    type_solutions,
                    expected_a,
                    actual_a,
                )?;
                unify_inner(
                    common_kinds,
                    types,
                    type_variables,
                    kind_inference_state,
                    type_solutions,
                    expected_b,
                    actual_b,
                )
            }
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::HasField(expected_field, expected_row) => match actual {
            Type::HasField(actual_field, actual_row) if expected_field == actual_field => {
                unify_inner(
                    common_kinds,
                    types,
                    type_variables,
                    kind_inference_state,
                    type_solutions,
                    expected_row,
                    actual_row,
                )
            }
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::Constraints(expected_constraints) => match actual {
            Type::Constraints(actual_constraints)
                if expected_constraints.len() == actual_constraints.len() =>
            {
                expected_constraints
                    .iter()
                    .zip(actual_constraints.iter())
                    .try_for_each(|(expected_constraint, actual_constraint)| {
                        unify_inner(
                            common_kinds,
                            types,
                            type_variables,
                            kind_inference_state,
                            type_solutions,
                            expected_constraint,
                            actual_constraint,
                        )
                    })
            }
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::RowCons(_, _, _) => match actual {
            Type::RowCons(_, _, _) => {
                /*
                The rows being unified need to be fully zonked so that all their currently known
                fields can be extracted using `unwrap_rows` (these fields are called the 'known prefix').

                The zonking also ensures that after the known prefix, each row ends in either an unsolved
                metavariable or the empty row. This ending is called the 'tail'.
                */
                let expected =
                    type_solutions.zonk(&kind_inference_state.kind_solutions, expected.clone());
                let expected_row_parts = expected.unwrap_rows();

                let actual =
                    type_solutions.zonk(&kind_inference_state.kind_solutions, actual.clone());
                let actual_row_parts = actual.unwrap_rows();

                // The fields common to both known prefixes will be unified.
                struct CommonField<'a> {
                    expected: &'a Type,
                    actual: &'a Type,
                }
                let mut common_fields: Vec<CommonField> = Vec::new();

                /*
                The remaining fields are those which are not in the intersection of the known prefixes.

                Vectors are used instead of sets to preserve the relative order of fields.
                */
                let mut remaining_expected_fields: Vec<(Rc<str>, Type)> = Vec::new();
                let mut remaining_actual_fields: Rope<(&Rc<str>, &Type)> =
                    Rope::from_vec(&actual_row_parts.fields);

                for (expected_field, expected_ty) in expected_row_parts.fields {
                    // TODO: turn `find` -> `delete_first` into a single `remove_first` call that
                    // searches for, removes, and returns the first element satisfying the predicate.
                    match remaining_actual_fields
                        .iter()
                        .find(|(actual_field, _)| expected_field == *actual_field)
                    {
                        Some((_, actual_ty)) => {
                            remaining_actual_fields = match remaining_actual_fields
                                .delete_first(|(actual_field, _)| expected_field == *actual_field)
                            {
                                Ok(new) => new,
                                Err(new) => new,
                            };
                            common_fields.push(CommonField {
                                expected: expected_ty,
                                actual: actual_ty,
                            });
                        }
                        None => {
                            remaining_expected_fields
                                .push((expected_field.clone(), expected_ty.clone()));
                        }
                    }
                }

                let remaining_actual_fields: Vec<(Rc<str>, Type)> = remaining_actual_fields
                    .iter()
                    .cloned()
                    .map(|(field, ty)| (field.clone(), ty.clone()))
                    .collect();

                common_fields.into_iter().try_for_each(|common_field| {
                    unify_inner(
                        common_kinds,
                        types,
                        type_variables,
                        kind_inference_state,
                        type_solutions,
                        common_field.expected,
                        common_field.actual,
                    )
                })?;

                /*
                In order for the two rows to be equal, the remaining expected fields must be present
                in the actual tail, and the expected tail must contain the remaining actual fields.

                The final type is terminated with a fresh metavariable (`common_tail`) to keep it
                open-ended.
                */
                let common_tail = Type::Meta(Kind::Row, type_solutions.fresh_meta());
                let expected_tail = expected_row_parts.rest.unwrap_or(&Type::RowNil);
                let actual_tail = actual_row_parts.rest.unwrap_or(&Type::RowNil);

                unify_inner(
                    common_kinds,
                    types,
                    type_variables,
                    kind_inference_state,
                    type_solutions,
                    &Type::mk_rows(remaining_expected_fields, Some(common_tail.clone())),
                    actual_tail,
                )?;

                unify_inner(
                    common_kinds,
                    types,
                    type_variables,
                    kind_inference_state,
                    type_solutions,
                    expected_tail,
                    &Type::mk_rows(remaining_actual_fields, Some(common_tail)),
                )
            }
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_inference_state,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(ErrorInfo::mismatch(
                &kind_inference_state.kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
    }
}
