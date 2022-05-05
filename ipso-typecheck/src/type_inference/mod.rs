//! Type checking and inference.

#[cfg(test)]
mod test;

use crate::{
    evidence::{self, Evidence},
    kind_inference,
    metavariables::{self, Meta, Solution},
    BoundVars,
};
use fnv::{FnvHashMap, FnvHashSet};
use ipso_core::{
    Binop, Branch, CmdPart, CommonKinds, Expr, Name, Pattern, RowParts, Signature, StringPart,
    Type, TypeSig,
};
use ipso_diagnostic::Source;
use ipso_rope::Rope;
use ipso_syntax::{self as syntax, Spanned};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};
use syntax::{kind::Kind, ModuleId};

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
    pub fn zonk(&self, kind_solutions: &kind_inference::Solutions, mut ty: Type) -> Type {
        self.zonk_mut(kind_solutions, &mut ty);
        ty
    }

    /**
    A mutable version of [`Solutions::zonk`].
    */
    pub fn zonk_mut(&self, kind_solutions: &kind_inference::Solutions, ty: &mut Type) {
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

/// A type unification error.
#[derive(PartialEq, Eq, Debug)]
pub enum UnificationError {
    Mismatch {
        expected: syntax::Type<Rc<str>>,
        actual: syntax::Type<Rc<str>>,
    },
    Occurs {
        meta: Meta,
        ty: syntax::Type<Rc<str>>,
    },
    KindError {
        error: kind_inference::InferenceError,
    },
}

impl UnificationError {
    /**
    Construct a [`UnificationError::Mismatch`].

    Uses `type_variables` to replace de Bruijn indices with names.
    */
    pub fn mismatch(
        kind_solutions: &kind_inference::Solutions,
        type_solutions: &Solutions,
        type_variables: &BoundVars<Kind>,
        expected: Type,
        actual: Type,
    ) -> Self {
        UnificationError::Mismatch {
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
    Construct a [`UnificationError::Occurs`].

    Uses `type_variables` to replace de Bruijn indices with names.
    */
    pub fn occurs(type_variables: &BoundVars<Kind>, meta: Meta, ty: &Type) -> Self {
        UnificationError::Occurs {
            meta,
            ty: ty
                .to_syntax()
                .map(&mut |ix| type_variables.lookup_index(*ix).unwrap().0.clone()),
        }
    }
}

/// Unify two types.
pub fn unify(
    common_kinds: &CommonKinds,
    types: &HashMap<Rc<str>, Kind>,
    type_variables: &BoundVars<Kind>,
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut Solutions,
    expected: &Type,
    actual: &Type,
) -> Result<(), UnificationError> {
    fn solve_left(
        kind_solutions: &kind_inference::Solutions,
        type_variables: &BoundVars<Kind>,
        type_solutions: &mut Solutions,
        meta: Meta,
        actual: &Type,
    ) -> Result<(), UnificationError> {
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
            Err(UnificationError::occurs(
                type_variables,
                meta,
                &type_solutions.zonk(kind_solutions, actual.clone()),
            ))
        } else {
            type_solutions.set(meta, actual);
            Ok(())
        }
    }

    fn solve_right(
        kind_solutions: &kind_inference::Solutions,
        type_variables: &BoundVars<Kind>,
        type_solutions: &mut Solutions,
        expected: &Type,
        meta: Meta,
    ) -> Result<(), UnificationError> {
        // See [note: avoiding solved metas as solutions]
        debug_assert!(match expected {
            Type::Meta(_, expected_meta) => {
                type_solutions.get(*expected_meta).is_unsolved()
            }
            _ => true,
        });

        if type_solutions.occurs(meta, expected) {
            Err(UnificationError::occurs(
                type_variables,
                meta,
                &type_solutions.zonk(kind_solutions, expected.clone()),
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
        kind_solutions: &mut kind_inference::Solutions,
        type_solutions: &mut Solutions,
        meta: &usize,
        actual: &Type,
    ) -> Result<(), UnificationError> {
        match walk(type_solutions, actual) {
            Type::Meta(_, actual_meta) if *meta == actual_meta => Ok(()),
            actual => match type_solutions.get(*meta).clone() {
                Solution::Unsolved => solve_left(
                    kind_solutions,
                    type_variables,
                    type_solutions,
                    *meta,
                    &actual,
                ),
                Solution::Solved(expected) => unify(
                    common_kinds,
                    types,
                    type_variables,
                    kind_solutions,
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
        kind_solutions: &mut kind_inference::Solutions,
        type_solutions: &mut Solutions,
        expected: &Type,
        meta: &usize,
    ) -> Result<(), UnificationError> {
        match walk(type_solutions, expected) {
            Type::Meta(_, expected_meta) if *meta == expected_meta => Ok(()),
            expected => match type_solutions.get(*meta).clone() {
                Solution::Unsolved => solve_right(
                    kind_solutions,
                    type_variables,
                    type_solutions,
                    &expected,
                    *meta,
                ),
                Solution::Solved(actual) => unify(
                    common_kinds,
                    types,
                    type_variables,
                    kind_solutions,
                    type_solutions,
                    &expected,
                    &actual,
                ),
            },
        }
    }

    let hint: &dyn Fn() -> kind_inference::InferenceErrorHint =
        &|| kind_inference::InferenceErrorHint::WhileChecking {
            ty: actual
                .to_syntax()
                .map(&mut |ix| type_variables.lookup_index(*ix).unwrap().0.clone()),
            has_kind: expected.kind(),
        };
    kind_inference::InferenceContext::new(common_kinds, types, type_variables, kind_solutions)
        .unify(hint, &expected.kind(), &actual.kind())
        .map_err(|error| UnificationError::KindError { error })?;

    match expected {
        Type::Meta(_, meta) => unify_meta_left(
            common_kinds,
            types,
            type_variables,
            kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::App(_, expected_a, expected_b) => match actual {
            Type::App(_, actual_a, actual_b) => {
                unify(
                    common_kinds,
                    types,
                    type_variables,
                    kind_solutions,
                    type_solutions,
                    expected_a,
                    actual_a,
                )?;
                unify(
                    common_kinds,
                    types,
                    type_variables,
                    kind_solutions,
                    type_solutions,
                    expected_b,
                    actual_b,
                )
            }
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
        Type::HasField(expected_field, expected_row) => match actual {
            Type::HasField(actual_field, actual_row) if expected_field == actual_field => unify(
                common_kinds,
                types,
                type_variables,
                kind_solutions,
                type_solutions,
                expected_row,
                actual_row,
            ),
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                        unify(
                            common_kinds,
                            types,
                            type_variables,
                            kind_solutions,
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
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
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
                let expected = type_solutions.zonk(kind_solutions, expected.clone());
                let expected_row_parts = expected.unwrap_rows();

                let actual = type_solutions.zonk(kind_solutions, actual.clone());
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
                    unify(
                        common_kinds,
                        types,
                        type_variables,
                        kind_solutions,
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

                unify(
                    common_kinds,
                    types,
                    type_variables,
                    kind_solutions,
                    type_solutions,
                    &Type::mk_rows(remaining_expected_fields, Some(common_tail.clone())),
                    actual_tail,
                )?;

                unify(
                    common_kinds,
                    types,
                    type_variables,
                    kind_solutions,
                    type_solutions,
                    expected_tail,
                    &Type::mk_rows(remaining_actual_fields, Some(common_tail)),
                )
            }
            Type::Meta(_, meta) => unify_meta_right(
                common_kinds,
                types,
                type_variables,
                kind_solutions,
                type_solutions,
                expected,
                meta,
            ),
            _ => Err(UnificationError::mismatch(
                kind_solutions,
                type_solutions,
                type_variables,
                expected.clone(),
                actual.clone(),
            )),
        },
    }
}

/// Type inference error information.
#[derive(PartialEq, Eq, Debug)]
pub enum InferenceErrorInfo {
    UnificationError { error: UnificationError },
    NotInScope { name: String },
    NotAValue { name: String },
    NotAModule,
    DuplicateArgument { name: Rc<str> },
    RedundantPattern,
}

/// A type inference error.
#[derive(PartialEq, Eq, Debug)]
pub struct InferenceError {
    pub source: Source,
    pub position: Option<usize>,
    pub info: InferenceErrorInfo,
}

impl InferenceError {
    /// Attach a position to an [`InferenceError`].
    pub fn with_position(mut self, position: usize) -> Self {
        self.position = Some(position);
        self
    }

    /// Construct an [`InferenceErrorInfo::NotInScope`].
    pub fn not_in_scope(source: &Source, name: &str) -> Self {
        InferenceError {
            source: source.clone(),
            position: None,
            info: InferenceErrorInfo::NotInScope {
                name: String::from(name),
            },
        }
    }

    /// Construct an [`InferenceErrorInfo::NotAValue`].
    pub fn not_a_value(source: &Source, name: &str) -> Self {
        InferenceError {
            source: source.clone(),
            position: None,
            info: InferenceErrorInfo::NotAValue {
                name: String::from(name),
            },
        }
    }

    /// Construct an [`InferenceErrorInfo::NotAModule`].
    pub fn not_a_module(source: &Source) -> Self {
        InferenceError {
            source: source.clone(),
            position: None,
            info: InferenceErrorInfo::NotAModule,
        }
    }

    /// Construct an [`InferenceErrorInfo::DuplicateArgument`].
    pub fn duplicate_argument(source: &Source, name: Rc<str>) -> Self {
        InferenceError {
            source: source.clone(),
            position: None,
            info: InferenceErrorInfo::DuplicateArgument { name },
        }
    }

    /// Construct an [`InferenceErrorInfo::RedundantPattern`].
    pub fn redundant_pattern(source: &Source) -> Self {
        InferenceError {
            source: source.clone(),
            position: None,
            info: InferenceErrorInfo::RedundantPattern,
        }
    }

    /// Construct a [`UnificationError::Occurs`].
    pub fn occurs(source: &Source, meta: Meta, ty: syntax::Type<Rc<str>>) -> Self {
        InferenceError::unification_error(source, UnificationError::Occurs { meta, ty })
    }

    /// Construct a [`UnificationError::Mismatch`].
    pub fn mismatch(
        source: &Source,
        expected: syntax::Type<Rc<str>>,
        actual: syntax::Type<Rc<str>>,
    ) -> Self {
        InferenceError::unification_error(source, UnificationError::Mismatch { expected, actual })
    }

    /// Lift a [`InferenceErrorInfo::UnificationError`].
    pub fn unification_error(source: &Source, error: UnificationError) -> Self {
        InferenceError {
            source: source.clone(),
            position: None,
            info: InferenceErrorInfo::UnificationError { error },
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum InferredPattern {
    Any {
        pattern: Pattern,
        names: Vec<(Rc<str>, Type)>,
        ty: Type,
    },
    Variant {
        /// Evidence for the variant's tag.
        tag: Rc<Expr>,

        /// The variant's constructor name.
        ctor: Rc<str>,

        /// The variant's argument name.
        arg_name: Rc<str>,

        /// The variant's argument type.
        arg_ty: Type,

        /// The row corresponding to the all the other constructors of the variant.
        rest: Type,
    },
}

impl InferredPattern {
    pub fn ty(&self, common_kinds: &CommonKinds) -> Type {
        match self {
            InferredPattern::Any { ty, .. } => ty.clone(),
            InferredPattern::Variant {
                ctor, arg_ty, rest, ..
            } => Type::mk_variant(
                common_kinds,
                vec![(ctor.clone(), arg_ty.clone())],
                Some(rest.clone()),
            ),
        }
    }

    pub fn pattern(&self) -> Pattern {
        match self {
            InferredPattern::Any { pattern, .. } => pattern.clone(),
            InferredPattern::Variant { tag, .. } => Pattern::Variant { tag: tag.clone() },
        }
    }

    pub fn names(&self) -> Vec<(Rc<str>, Type)> {
        match self {
            InferredPattern::Any { names, .. } => names.clone(),
            InferredPattern::Variant {
                arg_name, arg_ty, ..
            } => vec![(arg_name.clone(), arg_ty.clone())],
        }
    }
}

enum CheckedPattern {
    Any {
        pattern: Pattern,
        names: Vec<(Rc<str>, Type)>,
    },
    Variant {
        /// Evidence for the variant's tag.
        tag: Rc<Expr>,

        /// The variant's argument name.
        arg_name: Rc<str>,

        /// The variant's argument type.
        arg_ty: Type,

        /// The row corresponding to the all the other constructors of the variant.
        rest: Type,
    },
}

impl CheckedPattern {
    fn pattern(&self) -> Pattern {
        match self {
            CheckedPattern::Any { pattern, .. } => pattern.clone(),
            CheckedPattern::Variant { tag, .. } => Pattern::Variant { tag: tag.clone() },
        }
    }

    fn names(&self) -> Vec<(Rc<str>, Type)> {
        match self {
            CheckedPattern::Any { names, .. } => names.clone(),
            CheckedPattern::Variant {
                arg_name, arg_ty, ..
            } => vec![(arg_name.clone(), arg_ty.clone())],
        }
    }
}

/// Type inference context.
pub struct InferenceContext<'a> {
    common_kinds: &'a CommonKinds,
    source: &'a Source,
    modules: &'a HashMap<ModuleId, HashMap<String, Signature>>,
    types: &'a HashMap<Rc<str>, Kind>,
    type_variables: &'a BoundVars<Kind>,
    kind_solutions: &'a mut kind_inference::Solutions,
    type_solutions: &'a mut Solutions,
    type_signatures: &'a HashMap<String, Signature>,
    variables: &'a mut BoundVars<Type>,
    evidence: &'a mut Evidence,
}

fn pattern_is_redundant(
    seen_ctors: &FnvHashSet<&str>,
    saw_catchall: bool,
    pattern: &syntax::Pattern,
) -> bool {
    saw_catchall
        || match pattern {
            syntax::Pattern::Variant { name, .. } => seen_ctors.contains(name.as_ref()),
            _ => false,
        }
}

impl<'a> InferenceContext<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        common_kinds: &'a CommonKinds,
        source: &'a Source,
        modules: &'a HashMap<ModuleId, HashMap<String, Signature>>,
        types: &'a HashMap<Rc<str>, Kind>,
        type_variables: &'a BoundVars<Kind>,
        kind_solutions: &'a mut kind_inference::Solutions,
        type_solutions: &'a mut Solutions,
        type_signatures: &'a HashMap<String, Signature>,
        variables: &'a mut BoundVars<Type>,
        evidence: &'a mut Evidence,
    ) -> Self {
        InferenceContext {
            common_kinds,
            source,
            modules,
            types,
            type_variables,
            kind_solutions,
            type_solutions,
            type_signatures,
            variables,
            evidence,
        }
    }

    /// Unify two types.
    pub fn unify(
        &mut self,
        position: Option<usize>,
        expected: &Type,
        actual: &Type,
    ) -> Result<(), InferenceError> {
        unify(
            self.common_kinds,
            self.types,
            self.type_variables,
            self.kind_solutions,
            self.type_solutions,
            expected,
            actual,
        )
        .map_err(|error| {
            let error = InferenceError::unification_error(
                self.source,
                /*
                At the level of an `InferenceError`, a type mismatch should
                describe full types involved, rather than the specific components
                that don't match.

                e.g. when `a -> b` and `a -> c` mismatch (because `b` != `c`),
                `InferenceError` should report that `a -> b` != `a -> c`, instead
                of saying `b` != `c`.
                */
                match error {
                    UnificationError::Mismatch { .. } => UnificationError::Mismatch {
                        expected: zonk_type(
                            self.kind_solutions,
                            self.type_solutions,
                            expected.clone(),
                        )
                        .to_syntax()
                        .map(&mut |ix| self.type_variables.lookup_index(*ix).unwrap().0.clone()),
                        actual: zonk_type(self.kind_solutions, self.type_solutions, actual.clone())
                            .to_syntax()
                            .map(&mut |ix| {
                                self.type_variables.lookup_index(*ix).unwrap().0.clone()
                            }),
                    },
                    _ => error,
                },
            );
            match position {
                Some(position) => error.with_position(position),
                None => error,
            }
        })
    }

    /**
    Instantiate a type signature.

    Replaces `type_signature`'s type variables with metavariables, and applies `expr`
    to a placeholder for each constraint in `type_signature`.
    */
    pub fn instantiate(
        &mut self,
        pos: usize,
        expr: Expr,
        type_signature: &TypeSig,
    ) -> (Expr, Type) {
        let metas: Vec<Type> = type_signature
            .ty_vars
            .iter()
            .map(|_| {
                let kind = fresh_kind_meta(self.kind_solutions);
                fresh_type_meta(self.type_solutions, &kind)
            })
            .collect();

        let ty = type_signature.body.instantiate_many(&metas);
        let (constraints, ty) = ty.unwrap_constraints();

        let expr = constraints.iter().fold(expr, |expr, constraint| {
            let placeholder = Expr::Placeholder(
                self.evidence
                    .placeholder(pos, evidence::Constraint::from_type(constraint)),
            );
            Expr::mk_app(expr, placeholder)
        });

        (expr, ty.clone())
    }

    fn check_duplicate_args(
        &self,
        args: &[Spanned<syntax::Pattern>],
    ) -> Result<(), InferenceError> {
        let mut seen: HashSet<&str> = HashSet::new();
        args.iter()
            .flat_map(|arg| arg.item.get_arg_names().into_iter())
            .try_for_each(|arg| {
                if seen.contains(&arg.item.as_ref()) {
                    Err(
                        InferenceError::duplicate_argument(self.source, arg.item.clone())
                            .with_position(arg.pos),
                    )
                } else {
                    seen.insert(&arg.item);
                    Ok(())
                }
            })
    }

    fn infer_name_pattern(&mut self, name: &Spanned<Rc<str>>) -> InferredPattern {
        let name_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
        InferredPattern::Any {
            pattern: Pattern::Name,
            names: vec![(Rc::from(name.item.as_ref()), name_ty.clone())],
            ty: name_ty,
        }
    }

    fn infer_string_pattern(&self, s: &Spanned<Rc<str>>) -> InferredPattern {
        InferredPattern::Any {
            pattern: Pattern::String(s.item.clone()),
            names: Vec::new(),
            ty: Type::String,
        }
    }

    fn infer_int_pattern(&self, n: &Spanned<u32>) -> InferredPattern {
        InferredPattern::Any {
            pattern: Pattern::Int(n.item),
            names: Vec::new(),
            ty: Type::Int,
        }
    }

    fn infer_char_pattern(&self, c: &Spanned<char>) -> InferredPattern {
        InferredPattern::Any {
            pattern: Pattern::Char(c.item),
            names: Vec::new(),
            ty: Type::Char,
        }
    }

    fn infer_record_pattern(
        &mut self,
        names: &[Spanned<Rc<str>>],
        rest: Option<&Spanned<Rc<str>>>,
    ) -> InferredPattern {
        let mut names_to_positions: FnvHashMap<&str, usize> =
            FnvHashMap::with_capacity_and_hasher(names.len(), Default::default());
        let entire_row = {
            let fields = names
                .iter()
                .map(|name| {
                    let name_item_ref = name.item.as_ref();
                    names_to_positions.insert(name_item_ref, name.pos);
                    (
                        Rc::from(name_item_ref),
                        fresh_type_meta(self.type_solutions, &Kind::Type),
                    )
                })
                .collect();
            let rest = rest.map(|_| fresh_type_meta(self.type_solutions, &Kind::Row));
            Type::mk_rows(fields, rest)
        };

        let (names, names_tys): (Vec<Expr>, Vec<(Rc<str>, Type)>) = {
            let mut names: Vec<Expr> = Vec::with_capacity(names.len());
            let mut names_tys: Vec<(Rc<str>, Type)> = Vec::with_capacity(names.len());

            let mut row: &Type = &entire_row;
            while let Type::RowCons(field, ty, rest) = row {
                names.push(Expr::Placeholder(self.evidence.placeholder(
                    names_to_positions.get(field.as_ref()).copied().unwrap_or(0),
                    evidence::Constraint::HasField {
                        field: field.clone(),
                        rest: (**rest).clone(),
                    },
                )));
                names_tys.push((field.clone(), (**ty).clone()));
                row = rest.as_ref();
            }
            if let Some(rest) = rest {
                names_tys.push((
                    Rc::from(rest.item.as_ref()),
                    Type::app(Type::mk_record_ctor(self.common_kinds), row.clone()),
                ));
            }

            (names, names_tys)
        };

        InferredPattern::Any {
            pattern: Pattern::Record {
                names,
                rest: rest.is_some(),
            },
            names: names_tys,
            ty: Type::app(Type::mk_record_ctor(self.common_kinds), entire_row),
        }
    }

    fn infer_variant_pattern(
        &mut self,
        pos: usize,
        ctor: &str,
        arg: &Spanned<Rc<str>>,
    ) -> InferredPattern {
        let ctor: Rc<str> = Rc::from(ctor);
        let arg_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
        let rest_row = fresh_type_meta(self.type_solutions, &Kind::Row);
        let tag = Expr::Placeholder(self.evidence.placeholder(
            pos,
            evidence::Constraint::HasField {
                field: ctor.clone(),
                rest: rest_row.clone(),
            },
        ));
        InferredPattern::Variant {
            tag: Rc::new(tag),
            ctor,
            arg_name: Rc::from(arg.item.as_ref()),
            arg_ty,
            rest: rest_row,
        }
    }

    fn infer_wildcard_pattern(&mut self) -> InferredPattern {
        InferredPattern::Any {
            pattern: Pattern::Wildcard,
            names: Vec::new(),
            ty: fresh_type_meta(self.type_solutions, &Kind::Type),
        }
    }

    pub fn infer_pattern(&mut self, pattern: &Spanned<syntax::Pattern>) -> InferredPattern {
        match &pattern.item {
            syntax::Pattern::Name(name) => self.infer_name_pattern(name),
            syntax::Pattern::Record { names, rest } => {
                self.infer_record_pattern(names, rest.as_ref())
            }
            syntax::Pattern::Variant { name, arg } => {
                self.infer_variant_pattern(pattern.pos, name, arg)
            }
            syntax::Pattern::Char(c) => self.infer_char_pattern(c),
            syntax::Pattern::Int(n) => self.infer_int_pattern(n),
            syntax::Pattern::String(s) => self.infer_string_pattern(s),
            syntax::Pattern::Wildcard => self.infer_wildcard_pattern(),
        }
    }

    fn check_pattern(
        &mut self,
        pattern: &Spanned<syntax::Pattern>,
        expected: &Type,
    ) -> Result<CheckedPattern, InferenceError> {
        let result = self.infer_pattern(pattern);
        self.unify(Some(pattern.pos), expected, &result.ty(self.common_kinds))?;
        Ok(match result {
            InferredPattern::Any { pattern, names, .. } => CheckedPattern::Any { pattern, names },
            InferredPattern::Variant {
                tag,
                arg_name,
                arg_ty,
                rest,
                ..
            } => CheckedPattern::Variant {
                tag,
                arg_name,
                arg_ty,
                rest,
            },
        })
    }

    /// Infer an expression's type.
    pub fn infer(&mut self, expr: &Spanned<syntax::Expr>) -> Result<(Expr, Type), InferenceError> {
        match &expr.item {
            syntax::Expr::Var(name) => match self.variables.lookup_name(name) {
                Some((index, ty)) => Ok((Expr::Var(index), ty.clone())),
                None => match self.type_signatures.get(name) {
                    Some(signature) => match signature {
                        Signature::TypeSig(type_signature) => Ok(self.instantiate(
                            expr.pos,
                            Expr::Name(Name::definition(name.clone())),
                            type_signature,
                        )),
                        Signature::Module(_) => {
                            Err(InferenceError::not_a_value(self.source, name)
                                .with_position(expr.pos))
                        }
                    },
                    None => {
                        Err(InferenceError::not_in_scope(self.source, name).with_position(expr.pos))
                    }
                },
            },
            syntax::Expr::Module { id, path, item } => {
                fn lookup_path<'a>(
                    source: &Source,
                    definitions: &'a HashMap<String, Signature>,
                    path: &[Spanned<String>],
                ) -> Result<&'a HashMap<String, Signature>, InferenceError> {
                    if path.is_empty() {
                        Ok(definitions)
                    } else {
                        match definitions.get(&path[0].item) {
                            None => Err(InferenceError::not_in_scope(source, &path[0].item)
                                .with_position(path[0].pos)),
                            Some(signature) => match signature {
                                Signature::TypeSig(_) => {
                                    Err(InferenceError::not_a_module(source)
                                        .with_position(path[0].pos))
                                }
                                Signature::Module(definitions) => {
                                    lookup_path(source, definitions, &path[1..])
                                }
                            },
                        }
                    }
                }

                let definitions = match id {
                    syntax::ModuleRef::This => self.type_signatures,
                    syntax::ModuleRef::Id(id) => match self.modules.get(id) {
                        None => {
                            /*
                            A module accessor will only be desugared if the module was in scope, so this case
                            is impossible as long as `ctx.modules` is valid w.r.t this expression.
                            */
                            panic!(
                                "module not in scope. id: {:?}, path: {:?}, item: {:?}",
                                id, path, item
                            )
                        }
                        Some(definitions) => definitions,
                    },
                };

                let definitions = lookup_path(self.source, definitions, path)?;

                match definitions.get(&item.item) {
                    None => Err(InferenceError::not_in_scope(self.source, &item.item)
                        .with_position(item.pos)),
                    Some(signature) => match signature {
                        Signature::TypeSig(type_signature) => Ok(self.instantiate(
                            expr.pos,
                            Expr::Module {
                                id: *id,
                                path: path.iter().map(|x| x.item.clone()).collect(),
                                item: Name::definition(item.item.clone()),
                            },
                            type_signature,
                        )),
                        Signature::Module(_) => {
                            Err(InferenceError::not_a_value(self.source, &item.item)
                                .with_position(item.pos))
                        }
                    },
                }
            }

            syntax::Expr::True => Ok((Expr::True, Type::Bool)),
            syntax::Expr::False => Ok((Expr::False, Type::Bool)),
            syntax::Expr::IfThenElse(condition, then_expr, else_expr) => {
                let condition = self.check(condition, &Type::Bool)?;
                let (then_expr, then_ty) = self.infer(then_expr)?;
                let else_expr = self.check(else_expr, &then_ty)?;
                Ok((
                    Expr::mk_ifthenelse(condition, then_expr, else_expr),
                    then_ty,
                ))
            }

            syntax::Expr::Int(n) => Ok((Expr::Int(*n), Type::Int)),
            syntax::Expr::Char(c) => Ok((Expr::Char(*c), Type::Char)),
            syntax::Expr::Unit => Ok((Expr::Unit, Type::Unit)),
            syntax::Expr::Cmd(cmd_parts) => {
                let cmd_parts = cmd_parts
                    .iter()
                    .map(|cmd_part| match cmd_part {
                        syntax::CmdPart::Literal(value) => Ok(CmdPart::Literal(value.clone())),
                        syntax::CmdPart::Expr(expr) => self
                            .check(
                                expr,
                                &Type::app(
                                    Type::Array(self.common_kinds.type_to_type.clone()),
                                    Type::String,
                                ),
                            )
                            .map(CmdPart::Expr),
                    })
                    .collect::<Result<Vec<CmdPart>, _>>()?;
                Ok((Expr::Cmd(cmd_parts), Type::Cmd))
            }
            syntax::Expr::String(string_parts) => {
                let string_parts: Vec<StringPart> = string_parts
                    .iter()
                    .map(|string_part| match string_part {
                        syntax::StringPart::String(string) => {
                            Ok(StringPart::String(string.clone()))
                        }
                        syntax::StringPart::Expr(expr) => {
                            self.check(expr, &Type::String).map(StringPart::Expr)
                        }
                    })
                    .collect::<Result<_, _>>()?;
                Ok((Expr::String(string_parts), Type::String))
            }
            syntax::Expr::Array(items) => {
                let item_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
                let items: Vec<Expr> = items
                    .iter()
                    .map(|item| self.check(item, &item_ty))
                    .collect::<Result<_, _>>()?;
                Ok((
                    Expr::Array(items),
                    Type::app(Type::mk_array(self.common_kinds), item_ty),
                ))
            }

            syntax::Expr::Binop(op, left, right) => match op.item {
                syntax::Binop::Add => {
                    let left = self.check(left, &Type::Int)?;
                    let right = self.check(right, &Type::Int)?;
                    Ok((Expr::mk_binop(Binop::Add, left, right), Type::Int))
                }
                syntax::Binop::Multiply => {
                    let left = self.check(left, &Type::Int)?;
                    let right = self.check(right, &Type::Int)?;
                    Ok((Expr::mk_binop(Binop::Multiply, left, right), Type::Int))
                }
                syntax::Binop::Subtract => {
                    let left = self.check(left, &Type::Int)?;
                    let right = self.check(right, &Type::Int)?;
                    Ok((Expr::mk_binop(Binop::Subtract, left, right), Type::Int))
                }
                syntax::Binop::Divide => {
                    let left = self.check(left, &Type::Int)?;
                    let right = self.check(right, &Type::Int)?;
                    Ok((Expr::mk_binop(Binop::Divide, left, right), Type::Int))
                }
                syntax::Binop::Append => {
                    let item_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
                    let array_ty = Type::app(Type::mk_array(self.common_kinds), item_ty);
                    let left = self.check(left, &array_ty)?;
                    let right = self.check(right, &array_ty)?;
                    Ok((Expr::mk_binop(Binop::Append, left, right), array_ty))
                }
                syntax::Binop::Or => {
                    let left = self.check(left, &Type::Bool)?;
                    let right = self.check(right, &Type::Bool)?;
                    Ok((Expr::mk_binop(Binop::Or, left, right), Type::Bool))
                }
                syntax::Binop::And => {
                    let left = self.check(left, &Type::Bool)?;
                    let right = self.check(right, &Type::Bool)?;
                    Ok((Expr::mk_binop(Binop::And, left, right), Type::Bool))
                }
                syntax::Binop::LApply => {
                    let in_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
                    let out_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
                    let left =
                        self.check(left, &Type::mk_arrow(self.common_kinds, &in_ty, &out_ty))?;
                    let right = self.check(right, &in_ty)?;
                    Ok((Expr::mk_binop(Binop::LApply, left, right), out_ty))
                }
                syntax::Binop::RApply => {
                    let in_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
                    let out_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
                    let left = self.check(left, &in_ty)?;
                    let right =
                        self.check(right, &Type::mk_arrow(self.common_kinds, &in_ty, &out_ty))?;
                    Ok((Expr::mk_binop(Binop::RApply, left, right), out_ty))
                }
                syntax::Binop::Eq
                | syntax::Binop::Neq
                | syntax::Binop::Gt
                | syntax::Binop::Gte
                | syntax::Binop::Lt
                | syntax::Binop::Lte => panic!("overloaded binop not desugared: {:?}", op.item),
            },

            syntax::Expr::App(fun, arg) => {
                let in_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
                let out_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
                let fun = self.check(fun, &Type::mk_arrow(self.common_kinds, &in_ty, &out_ty))?;
                let arg = self.check(arg, &in_ty)?;
                Ok((Expr::mk_app(fun, arg), out_ty))
            }
            syntax::Expr::Lam { args, body } => {
                self.check_duplicate_args(args)?;

                let mut inferred_args: Vec<(Pattern, Type)> = Vec::with_capacity(args.len());
                let bound_variables: Vec<(Rc<str>, Type)> = args
                    .iter()
                    .flat_map(|arg| {
                        let result = self.infer_pattern(arg);
                        inferred_args.push((result.pattern(), result.ty(self.common_kinds)));
                        result.names().into_iter()
                    })
                    .collect();

                self.variables.insert(&bound_variables);
                let (body, body_ty) = self.infer(body)?;
                self.variables.delete(bound_variables.len());

                let (expr, ty) = inferred_args.into_iter().rev().fold(
                    (body, body_ty),
                    |(body, body_ty), (arg_pattern, arg_ty)| {
                        let body = match arg_pattern {
                            Pattern::Name => Expr::mk_lam(true, body),
                            Pattern::Char(_)
                            | Pattern::Int(_)
                            | Pattern::String(_)
                            | Pattern::Record { .. }
                            | Pattern::Variant { .. } => Expr::mk_lam(
                                true,
                                Expr::mk_case(
                                    Expr::Var(0),
                                    vec![Branch {
                                        pattern: arg_pattern,
                                        body,
                                    }],
                                ),
                            ),
                            Pattern::Wildcard => Expr::mk_lam(false, body),
                        };
                        let body_ty = Type::arrow(self.common_kinds, arg_ty, body_ty);

                        (body, body_ty)
                    },
                );

                Ok((expr, ty))
            }
            syntax::Expr::Let { name, value, rest } => {
                let (value, value_ty) = self.infer(value)?;

                self.variables.insert(&[(name.clone(), value_ty)]);
                let (rest, rest_ty) = self.infer(rest)?;
                self.variables.delete(1);

                Ok((Expr::mk_let(value, rest), rest_ty))
            }

            syntax::Expr::Record { fields, rest } => {
                let mut field_to_expr: FnvHashMap<&str, Expr> =
                    FnvHashMap::with_capacity_and_hasher(fields.len(), Default::default());
                let mut field_to_pos: FnvHashMap<&str, usize> =
                    FnvHashMap::with_capacity_and_hasher(fields.len(), Default::default());

                let entire_row: Type = {
                    let fields = fields
                        .iter()
                        .map(|(field_name, field_expr)| {
                            let field_pos = field_expr.pos;
                            self.infer(field_expr).map(|(field_expr, field_ty)| {
                                let field_name_str = field_name.as_str();
                                field_to_expr.insert(field_name_str, field_expr);
                                field_to_pos.insert(field_name_str, field_pos);
                                (Rc::from(field_name_str), field_ty)
                            })
                        })
                        .collect::<Result<_, _>>()?;
                    let rest = rest
                        .as_ref()
                        .map(|_| fresh_type_meta(self.type_solutions, &Kind::Row));
                    Ok(Type::mk_rows(fields, rest))
                }?;

                let mut expr_fields: Vec<(Expr, Expr)> = Vec::with_capacity(fields.len());
                let mut ty_fields: Vec<(Rc<str>, Type)> = Vec::with_capacity(fields.len());

                let mut row = &entire_row;
                while let Type::RowCons(field, ty, rest) = row {
                    let field_index = Expr::Placeholder(self.evidence.placeholder(
                        field_to_pos.get(field.as_ref()).copied().unwrap_or(0),
                        evidence::Constraint::HasField {
                            field: field.clone(),
                            rest: (**rest).clone(),
                        },
                    ));
                    let field_expr = field_to_expr.remove(field.as_ref()).unwrap();
                    expr_fields.push((field_index, field_expr));
                    ty_fields.push((field.clone(), (**ty).clone()));

                    row = rest.as_ref()
                }

                let (expr_rest, ty_rest) = match rest {
                    Some(rest) => {
                        let rest = self.check(
                            rest,
                            &Type::mk_app(&Type::mk_record_ctor(self.common_kinds), row),
                        )?;
                        Ok((Some(rest), Some(row.clone())))
                    }
                    None => {
                        self.unify(None, &Type::RowNil, row)?;
                        Ok((None, None))
                    }
                }?;

                Ok((
                    Expr::mk_record(expr_fields, expr_rest),
                    Type::mk_record(self.common_kinds, ty_fields, ty_rest),
                ))
            }
            syntax::Expr::Project(expr, field) => {
                let field_name: Rc<str> = Rc::from(field.item.as_str());
                let field_ty = fresh_type_meta(self.type_solutions, &Kind::Type);

                let rest_row = fresh_type_meta(self.type_solutions, &Kind::Row);

                let pos = expr.pos;
                let expr = self.check(
                    expr,
                    &Type::mk_record(
                        self.common_kinds,
                        vec![(field_name.clone(), field_ty.clone())],
                        Some(rest_row.clone()),
                    ),
                )?;

                let placeholder = Expr::Placeholder(self.evidence.placeholder(
                    pos,
                    evidence::Constraint::HasField {
                        field: field_name,
                        rest: rest_row,
                    },
                ));
                Ok((Expr::mk_project(expr, placeholder), field_ty))
            }

            syntax::Expr::Variant(constructor) => {
                let pos = constructor.pos;
                let constructor: Rc<str> = Rc::from(constructor.item.as_str());

                let rest_row = fresh_type_meta(self.type_solutions, &Kind::Row);
                let placeholder = Expr::Placeholder(self.evidence.placeholder(
                    pos,
                    evidence::Constraint::HasField {
                        field: constructor.clone(),
                        rest: rest_row.clone(),
                    },
                ));

                let arg_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
                Ok((
                    Expr::mk_variant(placeholder),
                    Type::arrow(
                        self.common_kinds,
                        arg_ty.clone(),
                        Type::mk_variant(
                            self.common_kinds,
                            vec![(constructor, arg_ty)],
                            Some(rest_row),
                        ),
                    ),
                ))
            }
            syntax::Expr::Embed(constructor, expr) => {
                let rest_row = fresh_type_meta(self.type_solutions, &Kind::Row);
                let expr = self.check(
                    expr,
                    &Type::app(Type::mk_variant_ctor(self.common_kinds), rest_row.clone()),
                )?;

                let constructor_pos = constructor.pos;
                let constructor: Rc<str> = Rc::from(constructor.item.as_str());
                let arg_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
                let placeholder = Expr::Placeholder(self.evidence.placeholder(
                    constructor_pos,
                    evidence::Constraint::HasField {
                        field: constructor.clone(),
                        rest: rest_row.clone(),
                    },
                ));
                Ok((
                    Expr::mk_embed(placeholder, expr),
                    Type::mk_variant(
                        self.common_kinds,
                        vec![(constructor, arg_ty)],
                        Some(rest_row),
                    ),
                ))
            }
            syntax::Expr::Case(expr, branches) => self.check_case(expr, branches),

            syntax::Expr::Comp(_) => {
                panic!("computation expression was not desugared")
            }
        }
    }

    fn check_case(
        &mut self,
        expr: &Spanned<syntax::Expr>,
        branches: &[syntax::Branch],
    ) -> Result<(Expr, Type), InferenceError> {
        let (expr, mut expr_ty) = self.infer(expr)?;

        /*
        [note: peeling constructors when matching on variants]

        As each variant constructor is checked, the constructor needs to be 'peeled'
        off the original expression type before checking the next branch.

        When a catch-all pattern is reached, it can be assigned a variant type that's
        missing all the constructors that have already been matched.

        e.g.

        ```
        # expr : (| A : a, B : b, c : C, d : D |)
        case expr of
          # check that `A x` has type `(| A : a, B : b, c : C, d : D |)`
          A x -> ...

          # check that `B y` has type `(| B : b, c : C, d : D |)`
          B y -> ...

          # check that `c` has type `(| c : C, d : D |)`
          c -> ...
        ```

        A consequence of this is that the tags associated with each variant pattern
        aren't unique. The above example gives:

        ```
        case expr of
          # A's tag is 0
          A x -> ...

          # B's tag is also 0 (because `B` is lexicographically the first constructor
          # in `(| B : b, c : C, d : D |)`)
          B y -> ...

          c -> ...

        The interpreter needs to account for this when checking pattern matches.
        ```
        */

        let out_ty = fresh_type_meta(self.type_solutions, &Kind::Type);
        let mut seen_ctors = FnvHashSet::default();
        let mut saw_catchall = false;
        let branches: Vec<Branch> = branches
            .iter()
            .map(|branch| {
                if pattern_is_redundant(&seen_ctors, saw_catchall, &branch.pattern.item) {
                    return Err(InferenceError::redundant_pattern(self.source)
                        .with_position(branch.pattern.pos));
                }

                if let syntax::Pattern::Variant { name, .. } = &branch.pattern.item {
                    seen_ctors.insert(name.as_ref());
                }

                let result = self.check_pattern(&branch.pattern, &expr_ty)?;

                if let CheckedPattern::Variant { rest, .. } = &result {
                    expr_ty = Type::app(Type::mk_variant_ctor(self.common_kinds), rest.clone())
                }

                let names = result.names();
                self.variables.insert(&names);
                let body = self.check(&branch.body, &out_ty)?;
                self.variables.delete(names.len());

                let pattern = result.pattern();
                if let Pattern::Wildcard | Pattern::Name = pattern {
                    saw_catchall = true;
                }

                Ok(Branch { pattern, body })
            })
            .collect::<Result<_, _>>()?;
        zonk_type_mut(self.kind_solutions, self.type_solutions, &mut expr_ty);
        match expr_ty.unwrap_variant() {
            Some(RowParts {
                rest: Some(rest), ..
            }) if !saw_catchall => self.unify(None, &Type::RowNil, rest),
            _ => Ok(()),
        }?;
        Ok((Expr::mk_case(expr, branches), out_ty))
    }

    /// Check an expression's type.
    pub fn check(
        &mut self,
        expr: &Spanned<syntax::Expr>,
        expected: &Type,
    ) -> Result<Expr, InferenceError> {
        let position = expr.pos;
        let (expr, expr_ty) = self.infer(expr)?;
        self.unify(Some(position), expected, &expr_ty)?;
        Ok(expr)
    }
}

/// Generate a fresh kind metavariable.
pub fn fresh_kind_meta(kind_solutions: &mut kind_inference::Solutions) -> Kind {
    Kind::Meta(kind_solutions.fresh_meta())
}

/// Generate a fresh type metavariable.
pub fn fresh_type_meta(type_solutions: &mut Solutions, kind: &Kind) -> Type {
    Type::Meta(kind.clone(), type_solutions.fresh_meta())
}

/// Substitute all solved type and kind metavariables in a type.
pub fn zonk_type(
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut Solutions,
    ty: Type,
) -> Type {
    type_solutions.zonk(kind_solutions, ty)
}

/// A mutable version of [`zonk_type`].
pub fn zonk_type_mut(
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut Solutions,
    ty: &mut Type,
) {
    type_solutions.zonk_mut(kind_solutions, ty);
}

/// Infer an expression's type.
pub fn infer(
    ctx: &mut InferenceContext,
    expr: &Spanned<syntax::Expr>,
) -> Result<(Expr, Type), InferenceError> {
    ctx.infer(expr)
}

/// Check an expression's type.
pub fn check(
    ctx: &mut InferenceContext,
    expr: &Spanned<syntax::Expr>,
    expected: &Type,
) -> Result<Expr, InferenceError> {
    ctx.check(expr, expected)
}
