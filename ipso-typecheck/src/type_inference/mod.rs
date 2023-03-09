//! Type checking and inference.

#[cfg(test)]
mod test;

pub mod unification;

use crate::{
    evidence::{self, Evidence},
    kind_inference, BoundVars,
};
use fnv::{FnvHashMap, FnvHashSet};
use ipso_core::{
    Binop, Branch, CmdPart, CommonKinds, Expr, Name, Pattern, Signature, StringPart, Type, TypeSig,
};
use ipso_diagnostic::Source;
use ipso_syntax::{self as syntax, Spanned};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};
use syntax::{kind::Kind, ModuleId};

/// Type inference error information.
#[derive(PartialEq, Eq, Debug)]
pub enum ErrorInfo {
    UnificationError { error: unification::Error },
    NotInScope { name: String },
    NotAValue { name: String },
    NotAModule,
    DuplicateArgument { name: Rc<str> },
    RedundantPattern,
}

/// A type inference error.
#[derive(PartialEq, Eq, Debug)]
pub struct Error {
    pub source: Source,
    pub position: usize,
    pub info: ErrorInfo,
}

impl Error {
    /// Construct an [`ErrorInfo::NotInScope`].
    pub fn not_in_scope(source: &Source, position: usize, name: &str) -> Self {
        Error {
            source: source.clone(),
            position,
            info: ErrorInfo::NotInScope {
                name: String::from(name),
            },
        }
    }

    /// Construct an [`ErrorInfo::NotAValue`].
    pub fn not_a_value(source: &Source, position: usize, name: &str) -> Self {
        Error {
            source: source.clone(),
            position,
            info: ErrorInfo::NotAValue {
                name: String::from(name),
            },
        }
    }

    /// Construct an [`ErrorInfo::NotAModule`].
    pub fn not_a_module(source: &Source, position: usize) -> Self {
        Error {
            source: source.clone(),
            position,
            info: ErrorInfo::NotAModule,
        }
    }

    /// Construct an [`ErrorInfo::DuplicateArgument`].
    pub fn duplicate_argument(source: &Source, position: usize, name: Rc<str>) -> Self {
        Error {
            source: source.clone(),
            position,
            info: ErrorInfo::DuplicateArgument { name },
        }
    }

    /// Construct an [`ErrorInfo::RedundantPattern`].
    pub fn redundant_pattern(source: &Source, position: usize) -> Self {
        Error {
            source: source.clone(),
            position,
            info: ErrorInfo::RedundantPattern,
        }
    }

    /// Lift a [`unification::Error`].
    pub fn unification_error(source: &Source, position: usize, error: unification::Error) -> Self {
        Error {
            source: source.clone(),
            position,
            info: ErrorInfo::UnificationError { error },
        }
    }

    pub fn message(&self) -> String {
        match &self.info {
            ErrorInfo::UnificationError { error } => error.message(),
            ErrorInfo::NotInScope { .. } => String::from("variable not in scope"),
            ErrorInfo::DuplicateArgument { .. } => String::from("duplicate argument"),
            ErrorInfo::RedundantPattern => String::from("redundant pattern"),
            ErrorInfo::NotAValue { .. } => String::from("not a value"),
            ErrorInfo::NotAModule => String::from("not a module"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CheckedPattern {
    Any {
        pattern: Pattern<Expr>,
        names: Vec<(Rc<str>, Type)>,
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

impl CheckedPattern {
    pub fn pattern(&self) -> Pattern<Expr> {
        match self {
            CheckedPattern::Any { pattern, .. } => pattern.clone(),
            CheckedPattern::Variant { tag, .. } => Pattern::Variant { tag: tag.clone() },
        }
    }

    pub fn names(&self) -> Vec<(Rc<str>, Type)> {
        match self {
            CheckedPattern::Any { names, .. } => names.clone(),
            CheckedPattern::Variant {
                arg_name, arg_ty, ..
            } => vec![(arg_name.clone(), arg_ty.clone())],
        }
    }
}

/**
Type inference environment.

[`infer`] and [`check`] are mutually recursive and take many arguments. This
struct bundles all those arguments for convenience.
*/
#[derive(Clone, Copy)]
pub struct Env<'a> {
    pub common_kinds: &'a CommonKinds,
    pub modules: &'a HashMap<ModuleId, HashMap<String, Signature>>,
    pub types: &'a HashMap<Rc<str>, Kind>,
    pub type_variables: &'a BoundVars<Kind>,
    pub type_signatures: &'a HashMap<String, Signature>,
    pub source: &'a Source,
}

impl<'a> Env<'a> {
    pub fn as_unification_env(&self) -> unification::Env {
        unification::Env {
            common_kinds: self.common_kinds,
            types: self.types,
            type_variables: self.type_variables,
        }
    }
}

/**
Type inference state.
*/
pub struct State {
    pub kind_inference_state: kind_inference::State,
    pub type_solutions: unification::Solutions,
    variables: BoundVars<Type>,
    pub evidence: Evidence,
}

impl State {
    pub fn new() -> Self {
        State {
            kind_inference_state: kind_inference::State::new(),
            type_solutions: unification::Solutions::new(),
            variables: BoundVars::new(),
            evidence: Evidence::new(),
        }
    }

    /// Substitute all solved type and kind metavariables in a type.
    pub fn zonk_type(&self, mut ty: Type) -> Type {
        self.zonk_type_mut(&mut ty);
        ty
    }

    /// A mutable version of [`State::zonk_type`].
    fn zonk_type_mut(&self, ty: &mut Type) {
        self.type_solutions
            .zonk_mut(&self.kind_inference_state.kind_solutions, ty);
    }

    pub fn zonk_kind(&self, close_unsolved: bool, kind: Kind) -> Kind {
        self.kind_inference_state.zonk(close_unsolved, kind)
    }

    /// Generate a fresh kind metavariable.
    pub fn fresh_kind_meta(&mut self) -> Kind {
        self.kind_inference_state.fresh_meta()
    }

    /// Generate a fresh type metavariable.
    pub fn fresh_type_meta(&mut self, kind: Kind) -> Type {
        fresh_type_meta(&mut self.type_solutions, kind)
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
                let kind = self.fresh_kind_meta();
                self.fresh_type_meta(kind)
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

    pub fn with_bound_vars<A>(
        &mut self,
        bindings: &[(Rc<str>, Type)],
        f: impl Fn(&mut Self) -> A,
    ) -> A {
        self.variables.insert(bindings);
        let result = f(self);
        self.variables.delete(bindings.len());
        result
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

fn fresh_type_meta(type_solutions: &mut unification::Solutions, kind: Kind) -> Type {
    Type::Meta(kind, type_solutions.fresh_meta())
}

fn check_duplicate_args(source: &Source, args: &[Spanned<syntax::Pattern>]) -> Result<(), Error> {
    let mut seen: HashSet<&str> = HashSet::new();
    args.iter()
        .flat_map(|arg| arg.item.iter_names())
        .try_for_each(|arg| {
            if seen.contains(&arg.item.as_ref()) {
                Err(Error::duplicate_argument(source, arg.pos, arg.item.clone()))
            } else {
                seen.insert(&arg.item);
                Ok(())
            }
        })
}

fn check_name_pattern(name: &Spanned<Rc<str>>, expected: &Type) -> Result<CheckedPattern, Error> {
    Ok(CheckedPattern::Any {
        pattern: Pattern::Name,
        names: vec![(Rc::from(name.item.as_ref()), expected.clone())],
    })
}

fn check_string_pattern(
    env: Env,
    state: &mut State,
    s: &Spanned<Rc<str>>,
    expected: &Type,
) -> Result<CheckedPattern, Error> {
    unification::unify(
        env.as_unification_env(),
        &mut state.kind_inference_state,
        &mut state.type_solutions,
        s.pos,
        expected,
        &Type::String,
    )
    .map_err(|error| Error::unification_error(env.source, s.pos, error))?;

    Ok(CheckedPattern::Any {
        pattern: Pattern::String(s.item.clone()),
        names: Vec::new(),
    })
}

fn check_int_pattern(
    env: Env,
    state: &mut State,
    n: &Spanned<i32>,
    expected: &Type,
) -> Result<CheckedPattern, Error> {
    unification::unify(
        env.as_unification_env(),
        &mut state.kind_inference_state,
        &mut state.type_solutions,
        n.pos,
        expected,
        &Type::Int,
    )
    .map_err(|error| Error::unification_error(env.source, n.pos, error))?;

    Ok(CheckedPattern::Any {
        pattern: Pattern::Int(n.item),
        names: Vec::new(),
    })
}

fn check_char_pattern(
    env: Env,
    state: &mut State,
    c: &Spanned<char>,
    expected: &Type,
) -> Result<CheckedPattern, Error> {
    unification::unify(
        env.as_unification_env(),
        &mut state.kind_inference_state,
        &mut state.type_solutions,
        c.pos,
        expected,
        &Type::Char,
    )
    .map_err(|error| Error::unification_error(env.source, c.pos, error))?;

    Ok(CheckedPattern::Any {
        pattern: Pattern::Char(c.item),
        names: Vec::new(),
    })
}

fn check_record_pattern(
    env: Env,
    state: &mut State,
    pos: usize,
    names: &[Spanned<Rc<str>>],
    rest: Option<&Spanned<Rc<str>>>,
    expected: &Type,
) -> Result<CheckedPattern, Error> {
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
                    fresh_type_meta(&mut state.type_solutions, Kind::Type),
                )
            })
            .collect();
        let rest = rest.map(|_| fresh_type_meta(&mut state.type_solutions, Kind::Row));
        Type::mk_rows(fields, rest)
    };

    let actual = unification::App {
        left: Type::mk_record_ctor(env.common_kinds),
        right: entire_row.clone(),
    };

    unification::unify_app(
        env.as_unification_env(),
        &mut state.kind_inference_state,
        &mut state.type_solutions,
        pos,
        expected,
        actual,
    )
    .map_err(|error| Error::unification_error(env.source, pos, error))?;

    let (names, names_tys): (Vec<Expr>, Vec<(Rc<str>, Type)>) = {
        let mut names: Vec<Expr> = Vec::with_capacity(names.len());
        let mut names_tys: Vec<(Rc<str>, Type)> = Vec::with_capacity(names.len());

        let mut row: &Type = &entire_row;
        while let Type::RowCons(field, ty, rest) = row {
            names.push(Expr::Placeholder(state.evidence.placeholder(
                names_to_positions.get(field.as_ref()).copied().unwrap_or(0),
                evidence::Constraint::HasField {
                    field: field.clone(),
                    rest: entire_row.clone(),
                },
            )));
            names_tys.push((field.clone(), ty.as_ref().clone()));
            row = rest.as_ref();
        }
        if let Some(rest) = rest {
            names_tys.push((
                Rc::from(rest.item.as_ref()),
                Type::app(Type::mk_record_ctor(env.common_kinds), row.clone()),
            ));
        }

        (names, names_tys)
    };

    Ok(CheckedPattern::Any {
        pattern: Pattern::Record {
            names,
            rest: rest.is_some(),
        },
        names: names_tys,
    })
}

fn check_array_pattern(
    env: Env,
    state: &mut State,
    pos: usize,
    names: &[Spanned<Rc<str>>],
    expected: &Type,
) -> Result<CheckedPattern, Error> {
    let item_type = state.fresh_type_meta(Kind::Type);
    let actual = unification::App {
        left: Type::mk_array(env.common_kinds),
        right: item_type.clone(),
    };

    unification::unify_app(
        env.as_unification_env(),
        &mut state.kind_inference_state,
        &mut state.type_solutions,
        pos,
        expected,
        actual,
    )
    .map_err(|error| Error::unification_error(env.source, pos, error))?;

    Ok(CheckedPattern::Any {
        pattern: Pattern::Array { names: names.len() },
        names: names
            .iter()
            .map(|name| (name.item.clone(), item_type.clone()))
            .collect(),
    })
}

fn check_variant_pattern(
    env: Env,
    state: &mut State,
    pos: usize,
    ctor: &str,
    arg: &Spanned<Rc<str>>,
    expected: &Type,
) -> Result<CheckedPattern, Error> {
    let ctor: Rc<str> = Rc::from(ctor);

    let arg_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
    let rest_row = fresh_type_meta(&mut state.type_solutions, Kind::Row);
    let actual = unification::App {
        left: Type::mk_variant_ctor(env.common_kinds),
        right: Type::mk_rows(vec![(ctor.clone(), arg_ty.clone())], Some(rest_row.clone())),
    };

    unification::unify_app(
        env.as_unification_env(),
        &mut state.kind_inference_state,
        &mut state.type_solutions,
        pos,
        expected,
        actual,
    )
    .map_err(|error| Error::unification_error(env.source, pos, error))?;

    let tag = Expr::Placeholder(state.evidence.placeholder(
        pos,
        evidence::Constraint::HasField {
            field: ctor.clone(),
            rest: rest_row.clone(),
        },
    ));

    Ok(CheckedPattern::Variant {
        tag: Rc::new(tag),
        ctor,
        arg_name: Rc::from(arg.item.as_ref()),
        arg_ty,
        rest: rest_row,
    })
}

fn check_wildcard_pattern(_expected: &Type) -> Result<CheckedPattern, Error> {
    Ok(CheckedPattern::Any {
        pattern: Pattern::Wildcard,
        names: Vec::new(),
    })
}

fn check_unit_pattern(
    env: Env,
    state: &mut State,
    pos: usize,
    expected: &Type,
) -> Result<CheckedPattern, Error> {
    unification::unify(
        env.as_unification_env(),
        &mut state.kind_inference_state,
        &mut state.type_solutions,
        pos,
        expected,
        &Type::Unit,
    )
    .map_err(|error| Error::unification_error(env.source, pos, error))?;

    Ok(CheckedPattern::Any {
        pattern: Pattern::Unit,
        names: Vec::new(),
    })
}

pub fn check_pattern(
    env: Env,
    state: &mut State,
    pattern: &Spanned<syntax::Pattern>,
    expected: &Type,
) -> Result<CheckedPattern, Error> {
    match &pattern.item {
        syntax::Pattern::Name(name) => check_name_pattern(name, expected),
        syntax::Pattern::Char(c) => check_char_pattern(env, state, c, expected),
        syntax::Pattern::Int(n) => check_int_pattern(env, state, n, expected),
        syntax::Pattern::String(s) => check_string_pattern(env, state, s, expected),
        syntax::Pattern::Unit => check_unit_pattern(env, state, pattern.pos, expected),
        syntax::Pattern::Wildcard => check_wildcard_pattern(expected),
        syntax::Pattern::Record { names, rest } => {
            check_record_pattern(env, state, pattern.pos, names, rest.as_ref(), expected)
        }
        syntax::Pattern::Array { items } => {
            check_array_pattern(env, state, pattern.pos, items, expected)
        }
        syntax::Pattern::Variant { name, arg } => match arg.item.as_ref() {
            syntax::Pattern::Name(arg) => {
                check_variant_pattern(env, state, pattern.pos, name, arg, expected)
            }
            syntax::Pattern::Record { .. }
            | syntax::Pattern::Array { .. }
            | syntax::Pattern::Variant { .. }
            | syntax::Pattern::Char(_)
            | syntax::Pattern::Int(_)
            | syntax::Pattern::String(_)
            | syntax::Pattern::Unit
            | syntax::Pattern::Wildcard => panic!("un-desugared pattern: {:?}", pattern),
        },
    }
}

fn check_string_part(
    env: Env,
    state: &mut State,
    string_part: &syntax::StringPart,
) -> Result<StringPart<Expr>, Error> {
    match string_part {
        syntax::StringPart::String(string) => Ok(StringPart::String(string.clone())),
        syntax::StringPart::Expr(expr) => {
            check(env, state, expr, &Type::String).map(StringPart::Expr)
        }
    }
}

/// Check an expression's type.
pub fn check(
    env: Env,
    state: &mut State,
    expr: &Spanned<syntax::Expr>,
    expected: &Type,
) -> Result<Expr, Error> {
    let position = expr.pos;

    match &expr.item {
        syntax::Expr::Var(name) => match state.variables.lookup_name(name) {
            Some((index, ty)) => {
                unification::unify(
                    env.as_unification_env(),
                    &mut state.kind_inference_state,
                    &mut state.type_solutions,
                    position,
                    expected,
                    ty,
                )
                .map_err(|error| Error::unification_error(env.source, position, error))?;

                Ok(Expr::Var(index))
            }
            None => match env.type_signatures.get(name) {
                Some(signature) => match signature {
                    Signature::TypeSig(type_signature) => {
                        let (expr, ty) = state.instantiate(
                            expr.pos,
                            Expr::Name(Name::definition(name.clone())),
                            type_signature,
                        );

                        unification::unify(
                            env.as_unification_env(),
                            &mut state.kind_inference_state,
                            &mut state.type_solutions,
                            position,
                            expected,
                            &ty,
                        )
                        .map_err(|error| Error::unification_error(env.source, position, error))?;

                        Ok(expr)
                    }
                    Signature::Module(_) => Err(Error::not_a_value(env.source, expr.pos, name)),
                },
                None => Err(Error::not_in_scope(env.source, expr.pos, name)),
            },
        },
        syntax::Expr::Module { id, path, item } => {
            fn lookup_path<'a>(
                source: &Source,
                definitions: &'a HashMap<String, Signature>,
                path: &[Spanned<String>],
            ) -> Result<&'a HashMap<String, Signature>, Error> {
                if path.is_empty() {
                    Ok(definitions)
                } else {
                    match definitions.get(&path[0].item) {
                        None => Err(Error::not_in_scope(source, path[0].pos, &path[0].item)),
                        Some(signature) => match signature {
                            Signature::TypeSig(_) => Err(Error::not_a_module(source, path[0].pos)),
                            Signature::Module(definitions) => {
                                lookup_path(source, definitions, &path[1..])
                            }
                        },
                    }
                }
            }

            let definitions = match id {
                syntax::ModuleRef::This => env.type_signatures,
                syntax::ModuleRef::Id(id) => match env.modules.get(id) {
                    None => {
                        /*
                        A module accessor will only be desugared if the module was in scope, so this case
                        is impossible as long as the set of modules valid w.r.t this expression.
                        */
                        panic!(
                            "module not in scope. id: {:?}, path: {:?}, item: {:?}",
                            id, path, item
                        )
                    }
                    Some(definitions) => definitions,
                },
            };

            let definitions = lookup_path(env.source, definitions, path)?;

            match definitions.get(&item.item) {
                None => Err(Error::not_in_scope(env.source, item.pos, &item.item)),
                Some(signature) => match signature {
                    Signature::TypeSig(type_signature) => {
                        let (expr, ty) = state.instantiate(
                            expr.pos,
                            Expr::Module {
                                id: *id,
                                path: path.iter().map(|x| x.item.clone()).collect(),
                                item: Name::definition(item.item.clone()),
                            },
                            type_signature,
                        );

                        unification::unify(
                            env.as_unification_env(),
                            &mut state.kind_inference_state,
                            &mut state.type_solutions,
                            position,
                            expected,
                            &ty,
                        )
                        .map_err(|error| Error::unification_error(env.source, position, error))?;

                        Ok(expr)
                    }
                    Signature::Module(_) => {
                        Err(Error::not_a_value(env.source, item.pos, &item.item))
                    }
                },
            }
        }
        syntax::Expr::App(_fun, _arg) => {
            fn unapply(
                expr: &Spanned<syntax::Expr>,
            ) -> (&Spanned<syntax::Expr>, Vec<&Spanned<syntax::Expr>>) {
                match &expr.item {
                    syntax::Expr::App(f, x) => {
                        let (f, mut xs) = unapply(f.as_ref());
                        xs.push(x.as_ref());
                        (f, xs)
                    }
                    _ => (expr, Vec::new()),
                }
            }

            let (fun, args) = unapply(expr);

            let in_tys = args
                .iter()
                .map(|_| fresh_type_meta(&mut state.type_solutions, Kind::Type))
                .collect::<Vec<_>>();
            let out_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);

            let fun = check(
                env,
                state,
                fun,
                &in_tys.iter().rev().fold(out_ty.clone(), |result, in_ty| {
                    Type::arrow(env.common_kinds, in_ty.clone(), result)
                }),
            )?;

            unification::unify(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                position,
                expected,
                &out_ty,
            )
            .map_err(|error| Error::unification_error(env.source, position, error))?;

            let args = args
                .iter()
                .zip(in_tys.iter())
                .map(|(arg, in_ty)| check(env, state, arg, in_ty))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(args.into_iter().fold(fun, Expr::mk_app))
        }
        syntax::Expr::Lam { args, body } => {
            check_duplicate_args(env.source, args)?;

            let in_tys: Vec<Type> = args
                .iter()
                .map(|_| state.fresh_type_meta(Kind::Type))
                .collect();
            let out_ty = state.fresh_type_meta(Kind::Type);

            unification::unify(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                position,
                expected,
                &in_tys.iter().rev().fold(out_ty.clone(), |acc, el| {
                    Type::arrow(env.common_kinds, el.clone(), acc)
                }),
            )
            .map_err(|error| Error::unification_error(env.source, position, error))?;

            let mut inferred_args: Vec<(Pattern<Expr>, Type)> = Vec::with_capacity(args.len());
            let mut bound_variables: Vec<(Rc<str>, Type)> = Vec::new();

            debug_assert!(
                args.len() == in_tys.len(),
                "args should be the same length as in_tys"
            );
            args.iter()
                .zip(in_tys.iter())
                .try_for_each(|(arg, arg_ty)| {
                    let result = check_pattern(env, state, arg, arg_ty)?;
                    inferred_args.push((result.pattern(), arg_ty.clone()));
                    bound_variables.extend(result.names());
                    Ok(())
                })?;

            state.variables.insert(&bound_variables);
            let body = check(env, state, body, &out_ty)?;
            state.variables.delete(bound_variables.len());

            let expr = inferred_args
                .into_iter()
                .rev()
                .fold(body, |body, (arg_pattern, _)| match arg_pattern {
                    Pattern::Name => Expr::mk_lam(true, body),
                    Pattern::Wildcard => Expr::mk_lam(false, body),
                    Pattern::Char(_)
                    | Pattern::Int(_)
                    | Pattern::String(_)
                    | Pattern::Unit
                    | Pattern::Record { .. }
                    | Pattern::Variant { .. }
                    | Pattern::Array { .. } => {
                        panic!("undesugared pattern: {:?}", arg_pattern)
                    }
                });

            Ok(expr)
        }
        syntax::Expr::True => {
            unification::unify(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                expr.pos,
                expected,
                &Type::Bool,
            )
            .map_err(|error| Error::unification_error(env.source, expr.pos, error))?;

            Ok(Expr::True)
        }
        syntax::Expr::False => {
            unification::unify(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                expr.pos,
                expected,
                &Type::Bool,
            )
            .map_err(|error| Error::unification_error(env.source, expr.pos, error))?;

            Ok(Expr::False)
        }
        syntax::Expr::Int(n) => {
            unification::unify(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                expr.pos,
                expected,
                &Type::Int,
            )
            .map_err(|error| Error::unification_error(env.source, expr.pos, error))?;

            Ok(Expr::Int(*n))
        }
        syntax::Expr::Char(c) => {
            unification::unify(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                expr.pos,
                expected,
                &Type::Char,
            )
            .map_err(|error| Error::unification_error(env.source, expr.pos, error))?;

            Ok(Expr::Char(*c))
        }
        syntax::Expr::Unit => {
            unification::unify(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                expr.pos,
                expected,
                &Type::Unit,
            )
            .map_err(|error| Error::unification_error(env.source, expr.pos, error))?;

            Ok(Expr::Unit)
        }
        syntax::Expr::String(string_parts) => {
            unification::unify(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                expr.pos,
                expected,
                &Type::String,
            )
            .map_err(|error| Error::unification_error(env.source, expr.pos, error))?;

            let string_parts = string_parts
                .iter()
                .map(|string_part| check_string_part(env, state, string_part))
                .collect::<Result<_, Error>>()?;

            Ok(Expr::String(string_parts))
        }
        syntax::Expr::Cmd(cmd_parts) => {
            unification::unify(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                expr.pos,
                expected,
                &Type::Cmd,
            )
            .map_err(|error| Error::unification_error(env.source, expr.pos, error))?;

            let cmd_parts = cmd_parts
                .iter()
                .map(|cmd_part| match cmd_part {
                    syntax::CmdPart::Arg(string_parts) => {
                        let string_parts = string_parts
                            .iter()
                            .map(|string_part| check_string_part(env, state, string_part))
                            .collect::<Result<Vec<_>, _>>()?;
                        Ok(CmdPart::Arg(string_parts))
                    }
                    syntax::CmdPart::Args(expr) => {
                        let expr = check(
                            env,
                            state,
                            expr,
                            &Type::app(Type::mk_array(env.common_kinds), Type::String),
                        )?;
                        Ok(CmdPart::Args(expr))
                    }
                })
                .collect::<Result<_, Error>>()?;

            Ok(Expr::Cmd(cmd_parts))
        }
        syntax::Expr::Array(items) => {
            let item_type = fresh_type_meta(&mut state.type_solutions, Kind::Type);
            let actual = unification::App {
                left: Type::mk_array(env.common_kinds),
                right: item_type.clone(),
            };

            unification::unify_app(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                expr.pos,
                expected,
                actual,
            )
            .map_err(|error| Error::unification_error(env.source, expr.pos, error))?;

            let items = items
                .iter()
                .map(|item| check(env, state, item, &item_type))
                .collect::<Result<_, Error>>()?;

            Ok(Expr::Array(items))
        }
        syntax::Expr::IfThenElse(condition, a, b) => {
            let condition = check(env, state, condition, &Type::Bool)?;
            let a = check(env, state, a, expected)?;
            let b = check(env, state, b, expected)?;
            Ok(Expr::mk_ifthenelse(condition, a, b))
        }
        syntax::Expr::Let { name, value, rest } => {
            let (value, value_type) = infer(env, state, value)?;

            state.variables.insert(&[(name.clone(), value_type)]);
            let rest = check(env, state, rest, expected)?;
            state.variables.delete(1);

            Ok(Expr::mk_let(value, rest))
        }
        syntax::Expr::Binop(op, left, right) => {
            let (op, expected_left_ty, expected_right_ty, actual_out_ty) = match op.item {
                syntax::Binop::Add => (Binop::Add, Type::Int, Type::Int, Type::Int),
                syntax::Binop::Multiply => (Binop::Multiply, Type::Int, Type::Int, Type::Int),
                syntax::Binop::Subtract => (Binop::Subtract, Type::Int, Type::Int, Type::Int),
                syntax::Binop::Divide => (Binop::Divide, Type::Int, Type::Int, Type::Int),
                syntax::Binop::Append => {
                    let item_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
                    let array_ty = Type::app(Type::mk_array(env.common_kinds), item_ty);
                    (Binop::Append, array_ty.clone(), array_ty.clone(), array_ty)
                }
                syntax::Binop::Or => (Binop::Or, Type::Bool, Type::Bool, Type::Bool),
                syntax::Binop::And => (Binop::And, Type::Bool, Type::Bool, Type::Bool),
                syntax::Binop::LApply => {
                    let in_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
                    let out_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
                    (
                        Binop::LApply,
                        Type::mk_arrow(env.common_kinds, &in_ty, &out_ty),
                        in_ty,
                        out_ty,
                    )
                }
                syntax::Binop::RApply => {
                    let in_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
                    let out_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
                    (
                        Binop::RApply,
                        in_ty.clone(),
                        Type::mk_arrow(env.common_kinds, &in_ty, &out_ty),
                        out_ty,
                    )
                }
                syntax::Binop::Eq
                | syntax::Binop::Neq
                | syntax::Binop::Gt
                | syntax::Binop::Gte
                | syntax::Binop::Lt
                | syntax::Binop::Lte => panic!("overloaded binop not desugared: {:?}", op.item),
            };

            let left = check(env, state, left, &expected_left_ty)?;
            let right = check(env, state, right, &expected_right_ty)?;

            unification::unify(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                expr.pos,
                expected,
                &actual_out_ty,
            )
            .map_err(|error| Error::unification_error(env.source, expr.pos, error))?;

            Ok(Expr::mk_binop(op, left, right))
        }
        syntax::Expr::Project(record, field) => {
            let field_name: Rc<str> = Rc::from(field.item.as_str());

            let rest_row = fresh_type_meta(&mut state.type_solutions, Kind::Row);
            let expected = Type::mk_record(
                env.common_kinds,
                vec![(field_name.clone(), expected.clone())],
                Some(rest_row.clone()),
            );

            let record = check(env, state, record, &expected)?;

            let field = Expr::Placeholder(state.evidence.placeholder(
                expr.pos,
                evidence::Constraint::HasField {
                    field: field_name,
                    rest: rest_row,
                },
            ));

            Ok(Expr::mk_project(record, field))
        }
        syntax::Expr::Variant(constructor) => {
            let constructor_name: Rc<str> = Rc::from(constructor.item.as_str());

            let arg_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
            let rest_row = fresh_type_meta(&mut state.type_solutions, Kind::Row);
            let actual = unification::App {
                left: Type::app(Type::mk_arrow_ctor(env.common_kinds), (&arg_ty).clone()),
                right: Type::mk_variant(
                    env.common_kinds,
                    vec![(constructor_name.clone(), arg_ty.clone())],
                    Some(rest_row.clone()),
                ),
            };

            unification::unify_app(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                expr.pos,
                expected,
                actual,
            )
            .map_err(|error| Error::unification_error(env.source, expr.pos, error))?;

            let tag = Expr::Placeholder(state.evidence.placeholder(
                expr.pos,
                evidence::Constraint::HasField {
                    field: constructor_name,
                    rest: rest_row,
                },
            ));

            Ok(Expr::mk_variant(tag))
        }
        syntax::Expr::Embed(constructor, rest) => {
            let constructor_name: Rc<str> = Rc::from(constructor.item.as_str());

            let arg_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
            let rest_row = fresh_type_meta(&mut state.type_solutions, Kind::Row);
            let actual = unification::App {
                left: Type::mk_variant_ctor(env.common_kinds),
                right: Type::mk_rows(
                    vec![(constructor_name.clone(), arg_ty)],
                    Some(rest_row.clone()),
                ),
            };

            unification::unify_app(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                expr.pos,
                expected,
                actual,
            )
            .map_err(|error| Error::unification_error(env.source, expr.pos, error))?;

            let rest = check(
                env,
                state,
                rest,
                &Type::app(Type::mk_variant_ctor(env.common_kinds), rest_row.clone()),
            )?;

            let tag = Expr::Placeholder(state.evidence.placeholder(
                expr.pos,
                evidence::Constraint::HasField {
                    field: constructor_name,
                    rest: rest_row,
                },
            ));

            Ok(Expr::mk_embed(tag, rest))
        }
        syntax::Expr::Record { fields, rest } => {
            let rest_row = match rest {
                Some(_) => fresh_type_meta(&mut state.type_solutions, Kind::Row),
                None => Type::RowNil,
            };

            let typed_fields = fields
                .iter()
                .map(|(field_name, _)| {
                    let field_type = fresh_type_meta(&mut state.type_solutions, Kind::Type);
                    (Rc::from(field_name.as_str()), field_type)
                })
                .collect::<Vec<_>>();

            let actual_row = Type::mk_rows(typed_fields, Some(rest_row.clone()));
            let actual = unification::App {
                left: Type::mk_record_ctor(env.common_kinds),
                right: actual_row.clone(),
            };

            unification::unify_app(
                env.as_unification_env(),
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                expr.pos,
                expected,
                actual,
            )
            .map_err(|error| Error::unification_error(env.source, expr.pos, error))?;

            let mut current_row = actual_row.clone();
            let fields = fields
                .iter()
                .map(|(field_name, field_value)| {
                    debug_assert!(matches!(current_row, Type::RowCons(_, _, _)));

                    let (_field_name, field_type, remaining_row) = match &current_row {
                        Type::RowCons(a, b, c) => (a.clone(), b.clone(), c.clone()),
                        _ => unreachable!(),
                    };

                    let field_name: Rc<str> = Rc::from(field_name.as_str());
                    debug_assert!(_field_name == field_name);

                    let field_value = check(env, state, field_value, &field_type)?;

                    let field = Expr::Placeholder(state.evidence.placeholder(
                        expr.pos,
                        evidence::Constraint::HasField {
                            field: field_name,
                            rest: actual_row.clone(),
                        },
                    ));

                    current_row = remaining_row.as_ref().clone();

                    Ok((field, field_value))
                })
                .collect::<Result<_, Error>>()?;

            let rest = match rest {
                Some(rest) => {
                    let rest = check(
                        env,
                        state,
                        rest,
                        &Type::app(Type::mk_record_ctor(env.common_kinds), rest_row),
                    )?;
                    Ok(Some(rest))
                }
                None => Ok(None),
            }?;

            Ok(Expr::mk_record(fields, rest))
        }
        syntax::Expr::Case(value, branches) => {
            fn pattern_is_redundant(
                seen_ctors: &FnvHashSet<Rc<str>>,
                saw_catchall: bool,
                pattern: &syntax::Pattern,
            ) -> bool {
                saw_catchall
                    || match pattern {
                        syntax::Pattern::Variant { name, .. } => seen_ctors.contains(name.as_ref()),
                        _ => false,
                    }
            }

            let mut seen_ctors: FnvHashSet<Rc<str>> = FnvHashSet::default();

            let value_pos = value.pos;
            let (value, value_ty) = infer(env, state, value)?;

            let mut current_value_ty = value_ty;
            let mut has_catchall = false;

            let branches = branches
                .iter()
                .map(|branch| {
                    if pattern_is_redundant(&seen_ctors, has_catchall, &branch.pattern.item) {
                        Err(Error::redundant_pattern(env.source, branch.pattern.pos))
                    } else {
                        Ok(())
                    }?;

                    let pattern = check_pattern(env, state, &branch.pattern, &current_value_ty)?;

                    if let CheckedPattern::Variant { ctor, rest, .. } = &pattern {
                        current_value_ty =
                            Type::app(Type::mk_variant_ctor(env.common_kinds), rest.clone());

                        seen_ctors.insert(ctor.clone());
                    }

                    has_catchall = has_catchall
                        || match &pattern {
                            CheckedPattern::Any { pattern, .. } => match pattern {
                                Pattern::Record { .. }
                                | Pattern::Variant { .. }
                                | Pattern::Char(_)
                                | Pattern::Int(_)
                                | Pattern::String(_)
                                | Pattern::Unit
                                | Pattern::Array { .. } => false,
                                Pattern::Name | Pattern::Wildcard => true,
                            },
                            CheckedPattern::Variant { .. } => false,
                        };

                    let body = state.with_bound_vars(&pattern.names(), |state| {
                        check(env, state, &branch.body, expected)
                    })?;

                    Ok(Branch {
                        pattern: pattern.pattern(),
                        body,
                    })
                })
                .collect::<Result<_, Error>>()?;

            /*
            When a `case` expression on variants has no catch-all patterns (i.e. names or wildcards),
            its scrutinee should be treated as a closed variant.

            ## Example (no catch-alls)

            This expression:

            ```
            \x ->
              case x of
                A a -> 0
                B b -> 1
            ```

            has type `(| A : a, B : b |) -> Int`

            ## Example (catch-all)

            This expression:

            ```
            \x ->
              case x of
                A a -> 0
                B b -> 1
                _ -> 2
            ```

            has type `(| A : a, B : b, r |) -> Int`
            */
            state
                .zonk_type(current_value_ty)
                .unwrap_variant()
                .iter()
                .try_for_each(|row_parts| {
                    if !has_catchall {
                        row_parts.rest.iter().try_for_each(|rest| {
                            unification::unify(
                                env.as_unification_env(),
                                &mut state.kind_inference_state,
                                &mut state.type_solutions,
                                value_pos,
                                &Type::RowNil,
                                rest,
                            )
                            .map_err(|error| Error::unification_error(env.source, value_pos, error))
                        })
                    } else {
                        Ok(())
                    }
                })?;

            Ok(Expr::mk_case(value, branches))
        }
        syntax::Expr::Comp(_comp_lines) => {
            panic!("computation expression was not desugared")
        }
    }
}

/// Infer an expression's type.
pub fn infer(
    env: Env,
    state: &mut State,
    expr: &Spanned<syntax::Expr>,
) -> Result<(Expr, Type), Error> {
    let ty = state.fresh_type_meta(Kind::Type);
    let expr = check(env, state, expr, &ty)?;
    Ok((expr, state.zonk_type(ty)))
}
