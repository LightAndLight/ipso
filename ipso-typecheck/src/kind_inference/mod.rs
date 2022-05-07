//! Kind checking and inference.

#[cfg(test)]
mod test;

pub mod unification;

use crate::{metavariables::Meta, BoundVars};
use ipso_core::{self as core, CommonKinds};
use ipso_syntax::{self as syntax, kind::Kind};
use std::{collections::HashMap, rc::Rc};

/// Extra context for kind inference errors.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorHint {
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
pub enum ErrorInfo {
    NotInScope { name: Rc<str> },
    UnificationError { error: unification::Error },
}

impl From<unification::Error> for ErrorInfo {
    fn from(error: unification::Error) -> Self {
        ErrorInfo::UnificationError { error }
    }
}

/// A kind inference error.
#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub info: ErrorInfo,
    pub hint: Option<ErrorHint>,
}

impl Error {
    /// Construct a [`NotInScope`](ErrorInfo::NotInScope) error.
    pub fn not_in_scope(name: Rc<str>) -> Self {
        Error {
            info: ErrorInfo::NotInScope { name },
            hint: None,
        }
    }

    /// Construct a [`Mismatch`](unification::Error::Mismatch) error.
    pub fn mismatch(expected: &Kind, actual: &Kind) -> Self {
        Error {
            info: ErrorInfo::UnificationError {
                error: unification::Error::Mismatch {
                    expected: expected.clone(),
                    actual: actual.clone(),
                },
            },
            hint: None,
        }
    }

    /// Construct an [`Occurs`](unification::Error::Occurs) error.
    pub fn occurs(meta: Meta, kind: &Kind) -> Self {
        Error {
            info: ErrorInfo::UnificationError {
                error: unification::Error::Occurs {
                    meta,
                    kind: kind.clone(),
                },
            },
            hint: None,
        }
    }

    /// Attach a [`hint`](ErrorHint) to an [`Error`].
    pub fn with_hint(mut self, hint: ErrorHint) -> Self {
        self.hint = Some(hint);
        self
    }
}

impl<T: Into<ErrorInfo>> From<T> for Error {
    fn from(error: T) -> Self {
        Error {
            info: error.into(),
            hint: None,
        }
    }
}

/// Kind inference environment.
#[derive(Clone, Copy)]
pub struct Env<'a> {
    pub common_kinds: &'a CommonKinds,
    pub types: &'a HashMap<Rc<str>, Kind>,
    pub type_variables: &'a BoundVars<Kind>,
}

/// Kind inference state.
pub struct State {
    kind_solutions: unification::Solutions,
}

impl State {
    pub fn new() -> Self {
        State {
            kind_solutions: unification::Solutions::new(),
        }
    }

    pub fn kind_solutions(&self) -> &unification::Solutions {
        &self.kind_solutions
    }

    pub fn kind_solutions_mut(&mut self) -> &mut unification::Solutions {
        &mut self.kind_solutions
    }

    /// Generate a fresh kind metavariable.
    pub fn fresh_meta(&mut self) -> Kind {
        Kind::Meta(self.kind_solutions.fresh_meta())
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

/// Unify two kinds, providing an error hint.
pub fn unify_with_hint(
    state: &mut State,
    hint: &dyn Fn() -> ErrorHint,
    expected: &Kind,
    actual: &Kind,
) -> Result<(), Error> {
    unification::unify(&mut state.kind_solutions, expected, actual)
        .map_err(|err| Error::from(err).with_hint(hint()))
}

/// Infer a type's kind, providing an error hint.
pub fn infer_with_hint(
    env: Env,
    state: &mut State,
    hint: &dyn Fn() -> ErrorHint,
    ty: &syntax::Type<Rc<str>>,
) -> Result<(core::Type, Kind), Error> {
    match ty {
        syntax::Type::Unit => Ok((core::Type::Unit, Kind::Type)),
        syntax::Type::Bool => Ok((core::Type::Bool, Kind::Type)),
        syntax::Type::Int => Ok((core::Type::Int, Kind::Type)),
        syntax::Type::Char => Ok((core::Type::Char, Kind::Type)),
        syntax::Type::String => Ok((core::Type::String, Kind::Type)),
        syntax::Type::Bytes => Ok((core::Type::Bytes, Kind::Type)),
        syntax::Type::Cmd => Ok((core::Type::Cmd, Kind::Type)),
        syntax::Type::Arrow => {
            let kind = env.common_kinds.type_to_type_to_type.clone();
            Ok((core::Type::Arrow(kind.clone()), kind))
        }
        syntax::Type::Array => {
            let kind = env.common_kinds.type_to_type.clone();
            Ok((core::Type::Array(kind.clone()), kind))
        }
        syntax::Type::IO => {
            let kind = env.common_kinds.type_to_type.clone();
            Ok((core::Type::IO(kind.clone()), kind))
        }

        syntax::Type::Record => {
            let kind = env.common_kinds.row_to_type.clone();
            Ok((core::Type::Record(kind.clone()), kind))
        }
        syntax::Type::Variant => {
            let kind = env.common_kinds.row_to_type.clone();
            Ok((core::Type::Variant(kind.clone()), kind))
        }
        syntax::Type::RowNil => Ok((core::Type::RowNil, Kind::Row)),
        syntax::Type::RowCons(field, ty, rest) => {
            let ty = check_with_hint(env, state, hint, ty, &Kind::Type)?;
            let rest = check_with_hint(env, state, hint, rest, &Kind::Row)?;
            Ok((core::Type::mk_rowcons(field.clone(), ty, rest), Kind::Row))
        }
        syntax::Type::HasField(field, row) => {
            let row = check_with_hint(env, state, hint, row, &Kind::Row)?;
            Ok((
                core::Type::mk_hasfield(field.clone(), row),
                Kind::Constraint,
            ))
        }

        syntax::Type::FatArrow => {
            let kind = env.common_kinds.constraint_to_type_to_type.clone();
            Ok((core::Type::FatArrow(kind.clone()), kind))
        }
        syntax::Type::Constraints(constraints) => {
            let constraints: Vec<core::Type> = constraints
                .iter()
                .map(|constraint| check_with_hint(env, state, hint, constraint, &Kind::Constraint))
                .collect::<Result<_, _>>()?;
            Ok((core::Type::Constraints(constraints), Kind::Constraint))
        }

        syntax::Type::App(a, b) => {
            let in_kind = state.fresh_meta();
            let out_kind = state.fresh_meta();
            let a = check_with_hint(env, state, hint, a, &Kind::mk_arrow(&in_kind, &out_kind))?;
            let b = check_with_hint(env, state, hint, b, &in_kind)?;
            Ok((core::Type::app(a, b), out_kind))
        }

        syntax::Type::Name(name) => match env.types.get(name) {
            Some(kind) => Ok((core::Type::Name(kind.clone(), name.clone()), kind.clone())),
            None => Err(Error::not_in_scope(name.clone()).with_hint(hint())),
        },
        syntax::Type::Var(name) => match env.type_variables.lookup_name(name) {
            Some((index, kind)) => Ok((core::Type::Var(kind.clone(), index), kind.clone())),
            None => Err(Error::not_in_scope(name.clone()).with_hint(hint())),
        },
        syntax::Type::Meta(_) => todo!(),
    }
}

/// Infer a type's kind.
pub fn infer(
    env: Env,
    state: &mut State,
    ty: &syntax::Type<Rc<str>>,
) -> Result<(core::Type, Kind), Error> {
    infer_with_hint(
        env,
        state,
        &|| ErrorHint::WhileInferring { ty: ty.clone() },
        ty,
    )
}

/// Check a type's kind, providing an error hint.
pub fn check_with_hint(
    env: Env,
    state: &mut State,
    hint: &dyn Fn() -> ErrorHint,
    ty: &syntax::Type<Rc<str>>,
    expected_kind: &Kind,
) -> Result<core::Type, Error> {
    let (ty, actual_kind) = infer_with_hint(env, state, hint, ty)?;
    unify_with_hint(state, hint, expected_kind, &actual_kind)?;
    Ok(ty)
}

/// Check a type's kind.
pub fn check(
    env: Env,
    state: &mut State,
    ty: &syntax::Type<Rc<str>>,
    expected_kind: &Kind,
) -> Result<core::Type, Error> {
    check_with_hint(
        env,
        state,
        &|| ErrorHint::WhileChecking {
            ty: ty.clone(),
            has_kind: expected_kind.clone(),
        },
        ty,
        expected_kind,
    )
}
