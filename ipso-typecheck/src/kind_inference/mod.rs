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

    pub fn message(&self) -> String {
        match &self.info {
            ErrorInfo::NotInScope { .. } => String::from("type not in scope"),
            ErrorInfo::UnificationError {
                error: unification_error,
            } => match unification_error {
                unification::Error::Mismatch { expected, actual } => {
                    let mut message = String::from("expected kind ");
                    message.push('"');
                    message.push_str(expected.render().as_str());
                    message.push('"');
                    message.push_str(", got kind ");
                    message.push('"');
                    message.push_str(actual.render().as_str());
                    message.push('"');
                    message
                }
                unification::Error::Occurs { meta, kind } => {
                    format!(
                        "infinite kind from equating ?{} with \"{}\"",
                        meta,
                        kind.render()
                    )
                }
            },
        }
    }

    pub fn addendum(&self) -> Option<String> {
        match self.info {
            ErrorInfo::NotInScope { .. } => None,
            ErrorInfo::UnificationError { .. } => self.hint.as_ref().map(|hint| match hint {
                ErrorHint::WhileChecking { ty, has_kind } => {
                    format!(
                        "While checking that \"{}\" has kind \"{}\"",
                        ty.render(),
                        has_kind.render()
                    )
                }
                ErrorHint::WhileInferring { ty } => {
                    format!("While inferring the kind of \"{}\"", ty.render())
                }
            }),
        }
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
    pub kind_solutions: unification::Solutions,
}

impl State {
    pub fn new() -> Self {
        State {
            kind_solutions: unification::Solutions::new(),
        }
    }

    pub fn zonk(&self, close_unsolved: bool, kind: Kind) -> Kind {
        self.kind_solutions.zonk(close_unsolved, kind)
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

/// Infer a type's kind.
pub fn infer(
    env: Env,
    state: &mut State,
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
            let ty = check(env, state, ty, &Kind::Type)?;
            let rest = check(env, state, rest, &Kind::Row)?;
            Ok((core::Type::mk_rowcons(field.clone(), ty, rest), Kind::Row))
        }
        syntax::Type::HasField(field, row) => {
            let row = check(env, state, row, &Kind::Row)?;
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
                .map(|constraint| check(env, state, constraint, &Kind::Constraint))
                .collect::<Result<_, _>>()?;
            Ok((core::Type::Constraints(constraints), Kind::Constraint))
        }

        syntax::Type::App(a, b) => {
            let in_kind = state.fresh_meta();
            let out_kind = state.fresh_meta();
            let a = check(env, state, a, &Kind::mk_arrow(&in_kind, &out_kind))?;
            let b = check(env, state, b, &in_kind)?;
            Ok((core::Type::app(a, b), out_kind))
        }

        syntax::Type::Name(name) => match name.as_ref() {
            "DebugRecordFields" => Ok((
                core::Type::DebugRecordFields,
                Kind::mk_arrow(&Kind::Row, &Kind::Constraint),
            )),
            "DebugVariantCtor" => Ok((
                core::Type::DebugVariantCtor,
                Kind::mk_arrow(&Kind::Row, &Kind::Constraint),
            )),
            _ => match env.types.get(name) {
                Some(kind) => Ok((core::Type::Name(kind.clone(), name.clone()), kind.clone())),
                None => Err(Error::not_in_scope(name.clone())),
            },
        },
        syntax::Type::Var(name) => match env.type_variables.lookup_name(name) {
            Some((index, kind)) => Ok((core::Type::Var(kind.clone(), index), kind.clone())),
            None => Err(Error::not_in_scope(name.clone())),
        },
        syntax::Type::Meta(meta) => {
            /*
            This should be impossible because there is no syntax for metavariables.

            The `Meta` constructor exists only for pretty-printing types that contain metavariables.
            */
            panic!("found meta ?{} in syntax", meta)
        }
    }
}

/// Check a type's kind.
pub fn check(
    env: Env,
    state: &mut State,
    ty: &syntax::Type<Rc<str>>,
    expected_kind: &Kind,
) -> Result<core::Type, Error> {
    let (ty, actual_kind) = infer(env, state, ty)?;
    unification::unify(&mut state.kind_solutions, expected_kind, &actual_kind)?;
    Ok(ty)
}
