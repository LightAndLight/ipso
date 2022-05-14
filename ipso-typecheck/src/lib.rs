#[cfg(test)]
mod test;

pub mod constraint_solving;
pub mod declaration;
pub mod evidence;
pub mod kind_inference;
pub mod metavariables;
pub mod module;
pub mod type_inference;

use constraint_solving::{solve_placeholder, Implication};
use diagnostic::{Location, Message};
use ipso_core::{self as core, CommonKinds};
use ipso_diagnostic::{self as diagnostic, Source};
use ipso_syntax::{self as syntax, kind::Kind};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    rc::Rc,
    todo,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BoundVars<A> {
    indices: HashMap<Rc<str>, Vec<usize>>,
    info: Vec<(Rc<str>, A)>,
}

impl<A> BoundVars<A> {
    pub fn new() -> Self {
        BoundVars {
            indices: HashMap::new(),
            info: Vec::new(),
        }
    }

    fn lookup_name(&self, name: &str) -> Option<(usize, &A)> {
        self.indices
            .get(name)
            .and_then(|entries| entries.last())
            .and_then(|&ix| self.lookup_index(ix).map(|(_, item)| (ix, item)))
    }

    fn lookup_index(&self, ix: usize) -> Option<&(Rc<str>, A)> {
        self.info.get(self.info.len() - 1 - ix)
    }

    fn insert(&mut self, vars: &[(Rc<str>, A)])
    where
        A: Debug + Clone,
    {
        debug_assert!(
            {
                let mut seen: HashSet<&Rc<str>> = HashSet::new();
                vars.iter().fold(true, |acc, el: &(Rc<str>, A)| {
                    let acc = acc && !seen.contains(&el.0);
                    seen.insert(&el.0);
                    acc
                })
            },
            "duplicate name in {:?}",
            vars
        );
        let num_vars = vars.len();
        for entries in self.indices.values_mut() {
            for entry in entries {
                *entry += num_vars;
            }
        }
        for (index, (var, _)) in vars.iter().rev().enumerate() {
            match self.indices.get_mut(var) {
                None => {
                    self.indices.insert((*var).clone(), vec![index]);
                }
                Some(entries) => {
                    entries.push(index);
                }
            };
        }
        self.info.extend(
            vars.iter()
                .map(|(name, item)| ((*name).clone(), item.clone())),
        );
    }

    fn delete(&mut self, count: usize) {
        for _ in 0..count {
            match self.info.pop() {
                None => panic!("unexpected empty context"),
                Some((name, _)) => {
                    let should_delete = match self.indices.get_mut(&name) {
                        None => panic!("context missing entry {:?}", name),
                        Some(ixs) => match ixs.pop() {
                            None => panic!("context ran out of indices in {:?}", name),
                            Some(_) => ixs.is_empty(),
                        },
                    };
                    if should_delete {
                        self.indices.remove(&name);
                    }
                }
            }
        }
        for item in &mut self.indices {
            for entry in item.1 {
                *entry -= count;
            }
        }
    }
}

impl<A> Default for BoundVars<A> {
    fn default() -> Self {
        Self::new()
    }
}

/// Type checking errors.
#[derive(PartialEq, Eq, Debug)]
pub enum Error {
    TypeError {
        error: type_inference::Error,
    },
    ConstraintError {
        error: constraint_solving::Error,
    },
    KindError {
        source: Source,
        pos: usize,
        error: kind_inference::Error,
    },
    DuplicateClassArgument {
        source: Source,
        pos: usize,
    },
    NoSuchClass {
        source: Source,
        pos: usize,
    },
    NotAMember {
        source: Source,
        pos: usize,
        cls: Rc<str>,
    },
}

impl Error {
    pub fn source(&self) -> Source {
        match self {
            Error::TypeError { error } => error.source.clone(),
            Error::ConstraintError { error } => error.source.clone(),
            Error::KindError { source, .. } => source.clone(),
            Error::DuplicateClassArgument { source, .. } => source.clone(),
            Error::NoSuchClass { source, .. } => source.clone(),
            Error::NotAMember { source, .. } => source.clone(),
        }
    }

    pub fn position(&self) -> usize {
        match self {
            Error::TypeError { error } => error.position,
            Error::ConstraintError { error } => error.position.unwrap_or(0),
            Error::KindError { pos, .. } => *pos,
            Error::DuplicateClassArgument { pos, .. } => *pos,
            Error::NoSuchClass { pos, .. } => *pos,
            Error::NotAMember { pos, .. } => *pos,
        }
    }

    pub fn message(&self) -> String {
        match self {
            Error::TypeError { error, .. } => error.message(),
            Error::KindError { error, .. } => error.message(),
            Error::ConstraintError { error } => error.message(),
            Error::DuplicateClassArgument { .. } => String::from("duplicate type class argument"),
            Error::NoSuchClass { .. } => String::from("type class not in scope"),
            Error::NotAMember { cls, .. } => {
                format!("not a member of the {:?} type class", cls)
            }
        }
    }

    pub fn addendum(&self) -> Option<String> {
        match self {
            Error::TypeError { .. } => None,
            Error::ConstraintError { error } => error.addendum(),
            Error::KindError { error, .. } => error.addendum(),
            Error::DuplicateClassArgument { .. } => None,
            Error::NoSuchClass { .. } => None,
            Error::NotAMember { .. } => None,
        }
    }

    pub fn report(&self, diagnostic: &mut diagnostic::Diagnostic) {
        diagnostic.item(
            Some(Location {
                source: self.source(),
                offset: Some(self.position()),
            }),
            Message {
                content: self.message(),
                addendum: self.addendum(),
            },
        )
    }
}

impl From<type_inference::Error> for Error {
    fn from(error: type_inference::Error) -> Self {
        Error::TypeError { error }
    }
}

impl From<constraint_solving::Error> for Error {
    fn from(error: constraint_solving::Error) -> Self {
        Error::ConstraintError { error }
    }
}

pub fn abstract_evidence(
    common_kinds: &CommonKinds,
    implications: &[Implication],
    types: &HashMap<Rc<str>, Kind>,
    type_variables: &BoundVars<Kind>,
    type_inference_state: &mut type_inference::State,
    source: &Source,
    mut expr: core::Expr,
) -> Result<(core::Expr, Vec<core::Type>), Error> {
    expr.subst_placeholder(&mut |p| -> Result<_, Error> {
        let (expr, _solved_constraint) = solve_placeholder(
            constraint_solving::Env {
                common_kinds,
                types,
                implications,
                type_variables,
                source,
            },
            type_inference_state,
            *p,
        )?;
        Ok(expr.as_ref().clone())
    })?;

    let mut unsolved_constraints: Vec<(core::EVar, core::Type)> = Vec::new();
    let mut seen_evars: HashSet<core::EVar> = HashSet::new();
    for ev in expr.iter_evars() {
        if !seen_evars.contains(ev) {
            seen_evars.insert(*ev);
            let constraint = type_inference_state.evidence.lookup_evar(ev).unwrap();
            unsolved_constraints.push((*ev, type_inference_state.zonk_type(constraint.to_type())));
        }
    }

    let mut expr = expr;
    let mut new_unsolved_constraints = Vec::new();
    for (ev, constraint) in unsolved_constraints.into_iter().rev() {
        expr = expr.abstract_evar(ev);
        match constraint.iter_metas().next() {
            None => {}
            Some(_) => {
                todo!("handle ambiguous constraints")
            }
        }
        new_unsolved_constraints.push(constraint);
    }
    new_unsolved_constraints.reverse();

    Ok((expr, new_unsolved_constraints))
}

pub fn generalise(
    common_kinds: &CommonKinds,
    implications: &[Implication],
    types: &HashMap<Rc<str>, Kind>,
    type_variables: &BoundVars<Kind>,
    type_inference_state: &mut type_inference::State,
    source: &Source,
    expr: core::Expr,
    ty: core::Type,
) -> Result<(core::Expr, core::TypeSig), Error> {
    let (expr, unsolved_constraints) = abstract_evidence(
        common_kinds,
        implications,
        types,
        type_variables,
        type_inference_state,
        source,
        expr,
    )?;

    let mut ty = type_inference_state.zonk_type(ty);
    for constraint in unsolved_constraints.into_iter().rev() {
        ty = core::Type::mk_fatarrow(common_kinds, constraint, ty);
    }

    let ty_vars: Vec<(Rc<str>, Kind)> = type_variables
        .info
        .iter()
        .map(|(name, kind)| {
            (
                name.clone(),
                type_inference_state.zonk_kind(true, kind.clone()),
            )
        })
        .collect();
    let sig = core::TypeSig::new(ty_vars, ty);

    Ok((expr, sig))
}

pub fn fill_ty_names(
    bound_tyvars: &BoundVars<Kind>,
    ty: syntax::Type<usize>,
) -> syntax::Type<Rc<str>> {
    ty.map(&mut |&ix| bound_tyvars.lookup_index(ix).unwrap().0.clone())
}

fn infer_kind(
    common_kinds: &CommonKinds,
    types: &HashMap<Rc<str>, Kind>,
    type_variables: &BoundVars<Kind>,
    kind_inference_state: &mut kind_inference::State,
    source: &Source,
    pos: usize,
    ty: &syntax::Type<Rc<str>>,
) -> Result<(core::Type, Kind), Error> {
    let env = kind_inference::Env {
        common_kinds,
        types,
        type_variables,
    };

    kind_inference::infer(env, kind_inference_state, ty).map_err(|error| Error::KindError {
        source: source.clone(),
        pos,
        error: error.with_hint(kind_inference::ErrorHint::WhileInferring { ty: ty.clone() }),
    })
}

fn check_kind(
    common_kinds: &CommonKinds,
    types: &HashMap<Rc<str>, Kind>,
    type_variables: &BoundVars<Kind>,
    kind_inference_state: &mut kind_inference::State,
    source: &Source,
    pos: Option<usize>,
    ty: &syntax::Type<Rc<str>>,
    kind: &Kind,
) -> Result<core::Type, Error> {
    let env = kind_inference::Env {
        common_kinds,
        types,
        type_variables,
    };

    kind_inference::check(env, kind_inference_state, ty, kind).map_err(|error| Error::KindError {
        source: source.clone(),
        pos: pos.unwrap_or(0),
        error: error.with_hint(kind_inference::ErrorHint::WhileChecking {
            ty: ty.clone(),
            has_kind: kind.clone(),
        }),
    })
}

pub fn eq_zonked_type(
    type_solutions: &type_inference::unification::Solutions,
    t1: &core::Type,
    t2: &core::Type,
) -> bool {
    fn zonk_just_enough<'a>(
        type_solutions: &'a type_inference::unification::Solutions,
        t: &'a core::Type,
    ) -> &'a core::Type {
        match t {
            core::Type::Meta(_, n) => match type_solutions.get(*n) {
                metavariables::Solution::Unsolved => t,
                metavariables::Solution::Solved(sol) => zonk_just_enough(type_solutions, sol),
            },
            t => t,
        }
    }

    let t2: &core::Type = zonk_just_enough(type_solutions, t2);
    match t1 {
        core::Type::Name(_, n) => match t2 {
            core::Type::Name(_, n2) => n == n2,
            _ => false,
        },
        core::Type::Var(_, v) => match t2 {
            core::Type::Var(_, v2) => v == v2,
            _ => false,
        },
        core::Type::Bool => matches!(t2, core::Type::Bool),
        core::Type::Int => matches!(t2, core::Type::Int),
        core::Type::Char => matches!(t2, core::Type::Char),
        core::Type::String => matches!(t2, core::Type::String),
        core::Type::Bytes => matches!(t2, core::Type::Bytes),
        core::Type::Cmd => matches!(t2, core::Type::Cmd),
        core::Type::Arrow(_) => matches!(t2, core::Type::Arrow(_)),
        core::Type::DebugRecordFields => matches!(t2, core::Type::DebugRecordFields),
        core::Type::DebugVariantCtor => matches!(t2, core::Type::DebugVariantCtor),
        core::Type::FatArrow(_) => matches!(t2, core::Type::FatArrow(_)),
        core::Type::Array(_) => matches!(t2, core::Type::Arrow(_)),
        core::Type::Record(_) => matches!(t2, core::Type::Record(_)),
        core::Type::Variant(_) => matches!(t2, core::Type::Variant(_)),
        core::Type::IO(_) => matches!(t2, core::Type::IO(_)),
        core::Type::RowNil => matches!(t2, core::Type::RowNil),
        core::Type::Unit => matches!(t2, core::Type::Unit),
        core::Type::Constraints(cs) => match t2 {
            core::Type::Constraints(cs2) => {
                cs.len() == cs2.len()
                    && cs
                        .iter()
                        .zip(cs2.iter())
                        .all(|(a, b)| eq_zonked_type(type_solutions, a, b))
            }
            _ => false,
        },
        core::Type::App(_, a, b) => match t2 {
            core::Type::App(_, a2, b2) => {
                eq_zonked_type(type_solutions, a, a2) && eq_zonked_type(type_solutions, b, b2)
            }
            _ => false,
        },
        core::Type::RowCons(a, b, c) => match t2 {
            core::Type::RowCons(a2, b2, c2) => {
                a == a2
                    && eq_zonked_type(type_solutions, b, b2)
                    && eq_zonked_type(type_solutions, c, c2)
            }
            _ => false,
        },
        core::Type::HasField(a, b) => match t2 {
            core::Type::HasField(a2, b2) => a == a2 && eq_zonked_type(type_solutions, b, b2),
            _ => false,
        },
        core::Type::Meta(_, n) => match type_solutions.get(*n) {
            metavariables::Solution::Unsolved => match t2 {
                core::Type::Meta(_, n2) => n == n2,
                _ => false,
            },
            metavariables::Solution::Solved(sol) => eq_zonked_type(type_solutions, sol, t2),
        },
    }
}

pub fn eq_zonked_constraint(
    type_solutions: &type_inference::unification::Solutions,
    c1: &evidence::Constraint,
    c2: &evidence::Constraint,
) -> bool {
    match c1 {
        evidence::Constraint::HasField { field, rest } => match c2 {
            evidence::Constraint::HasField {
                field: field2,
                rest: rest2,
            } => field == field2 && eq_zonked_type(type_solutions, rest, rest2),
            _ => false,
        },
        evidence::Constraint::DebugRecordFields(ty) => match c2 {
            evidence::Constraint::DebugRecordFields(ty2) => eq_zonked_type(type_solutions, ty, ty2),
            _ => false,
        },
        evidence::Constraint::DebugVariantCtor(ty) => match c2 {
            evidence::Constraint::DebugVariantCtor(ty2) => eq_zonked_type(type_solutions, ty, ty2),
            _ => false,
        },
        evidence::Constraint::Type(ty) => match c2 {
            evidence::Constraint::Type(ty2) => eq_zonked_type(type_solutions, ty, ty2),
            _ => false,
        },
    }
}
