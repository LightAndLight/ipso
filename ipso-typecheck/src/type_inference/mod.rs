//! Type checking and inference.

use crate::{
    kind_inference,
    metavariables::{self, Meta, Solution},
};
use ipso_core::Type;
use std::rc::Rc;

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
    Check whether the metavariable occurs in a type.
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
            | Type::Cmd => todo!(),
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
