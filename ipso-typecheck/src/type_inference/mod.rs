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
    Binop, Branch, CmdPart, CommonKinds, Expr, Name, Pattern, RowParts, Signature, StringPart,
    Type, TypeSig,
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
    pub position: Option<usize>,
    pub info: ErrorInfo,
}

impl Error {
    /// Attach a position to an [`Error`].
    pub fn with_position(mut self, position: usize) -> Self {
        self.position = Some(position);
        self
    }

    /// Construct an [`ErrorInfo::NotInScope`].
    pub fn not_in_scope(source: &Source, name: &str) -> Self {
        Error {
            source: source.clone(),
            position: None,
            info: ErrorInfo::NotInScope {
                name: String::from(name),
            },
        }
    }

    /// Construct an [`ErrorInfo::NotAValue`].
    pub fn not_a_value(source: &Source, name: &str) -> Self {
        Error {
            source: source.clone(),
            position: None,
            info: ErrorInfo::NotAValue {
                name: String::from(name),
            },
        }
    }

    /// Construct an [`ErrorInfo::NotAModule`].
    pub fn not_a_module(source: &Source) -> Self {
        Error {
            source: source.clone(),
            position: None,
            info: ErrorInfo::NotAModule,
        }
    }

    /// Construct an [`ErrorInfo::DuplicateArgument`].
    pub fn duplicate_argument(source: &Source, name: Rc<str>) -> Self {
        Error {
            source: source.clone(),
            position: None,
            info: ErrorInfo::DuplicateArgument { name },
        }
    }

    /// Construct an [`ErrorInfo::RedundantPattern`].
    pub fn redundant_pattern(source: &Source) -> Self {
        Error {
            source: source.clone(),
            position: None,
            info: ErrorInfo::RedundantPattern,
        }
    }

    /// Lift a [`unification::Error`].
    pub fn unification_error(source: &Source, error: unification::Error) -> Self {
        Error {
            source: source.clone(),
            position: None,
            info: ErrorInfo::UnificationError { error },
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

pub enum CheckedPattern {
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

/**
Type inference state.
*/
pub struct State<'a> {
    pub kind_inference_state: kind_inference::State,
    pub type_solutions: unification::Solutions,
    variables: BoundVars<Type>,
    pub evidence: &'a mut Evidence,
}

impl<'a> State<'a> {
    pub fn new(evidence: &'a mut Evidence) -> Self {
        State {
            kind_inference_state: kind_inference::State::new(),
            type_solutions: unification::Solutions::new(),
            variables: BoundVars::new(),
            evidence,
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

fn fresh_type_meta(type_solutions: &mut unification::Solutions, kind: Kind) -> Type {
    Type::Meta(kind, type_solutions.fresh_meta())
}

fn check_duplicate_args(source: &Source, args: &[Spanned<syntax::Pattern>]) -> Result<(), Error> {
    let mut seen: HashSet<&str> = HashSet::new();
    args.iter()
        .flat_map(|arg| arg.item.get_arg_names().into_iter())
        .try_for_each(|arg| {
            if seen.contains(&arg.item.as_ref()) {
                Err(Error::duplicate_argument(source, arg.item.clone()).with_position(arg.pos))
            } else {
                seen.insert(&arg.item);
                Ok(())
            }
        })
}

fn infer_name_pattern(
    type_solutions: &mut unification::Solutions,
    name: &Spanned<Rc<str>>,
) -> InferredPattern {
    let name_ty = fresh_type_meta(type_solutions, Kind::Type);
    InferredPattern::Any {
        pattern: Pattern::Name,
        names: vec![(Rc::from(name.item.as_ref()), name_ty.clone())],
        ty: name_ty,
    }
}

fn infer_string_pattern(s: &Spanned<Rc<str>>) -> InferredPattern {
    InferredPattern::Any {
        pattern: Pattern::String(s.item.clone()),
        names: Vec::new(),
        ty: Type::String,
    }
}

fn infer_int_pattern(n: &Spanned<u32>) -> InferredPattern {
    InferredPattern::Any {
        pattern: Pattern::Int(n.item),
        names: Vec::new(),
        ty: Type::Int,
    }
}

fn infer_char_pattern(c: &Spanned<char>) -> InferredPattern {
    InferredPattern::Any {
        pattern: Pattern::Char(c.item),
        names: Vec::new(),
        ty: Type::Char,
    }
}

fn infer_record_pattern(
    common_kinds: &CommonKinds,
    type_solutions: &mut unification::Solutions,
    evidence: &mut Evidence,
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
                    fresh_type_meta(type_solutions, Kind::Type),
                )
            })
            .collect();
        let rest = rest.map(|_| fresh_type_meta(type_solutions, Kind::Row));
        Type::mk_rows(fields, rest)
    };

    let (names, names_tys): (Vec<Expr>, Vec<(Rc<str>, Type)>) = {
        let mut names: Vec<Expr> = Vec::with_capacity(names.len());
        let mut names_tys: Vec<(Rc<str>, Type)> = Vec::with_capacity(names.len());

        let mut row: &Type = &entire_row;
        while let Type::RowCons(field, ty, rest) = row {
            names.push(Expr::Placeholder(evidence.placeholder(
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
                Type::app(Type::mk_record_ctor(common_kinds), row.clone()),
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
        ty: Type::app(Type::mk_record_ctor(common_kinds), entire_row),
    }
}

fn infer_variant_pattern(
    type_solutions: &mut unification::Solutions,
    evidence: &mut Evidence,
    pos: usize,
    ctor: &str,
    arg: &Spanned<Rc<str>>,
) -> InferredPattern {
    let ctor: Rc<str> = Rc::from(ctor);
    let arg_ty = fresh_type_meta(type_solutions, Kind::Type);
    let rest_row = fresh_type_meta(type_solutions, Kind::Row);
    let tag = Expr::Placeholder(evidence.placeholder(
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

fn infer_wildcard_pattern(type_solutions: &mut unification::Solutions) -> InferredPattern {
    InferredPattern::Any {
        pattern: Pattern::Wildcard,
        names: Vec::new(),
        ty: fresh_type_meta(type_solutions, Kind::Type),
    }
}

/// Infer a pattern's type.
pub fn infer_pattern(
    env: Env,
    state: &mut State,
    pattern: &Spanned<syntax::Pattern>,
) -> InferredPattern {
    match &pattern.item {
        syntax::Pattern::Name(name) => infer_name_pattern(&mut state.type_solutions, name),
        syntax::Pattern::Record { names, rest } => infer_record_pattern(
            env.common_kinds,
            &mut state.type_solutions,
            state.evidence,
            names,
            rest.as_ref(),
        ),
        syntax::Pattern::Variant { name, arg } => infer_variant_pattern(
            &mut state.type_solutions,
            state.evidence,
            pattern.pos,
            name,
            arg,
        ),
        syntax::Pattern::Char(c) => infer_char_pattern(c),
        syntax::Pattern::Int(n) => infer_int_pattern(n),
        syntax::Pattern::String(s) => infer_string_pattern(s),
        syntax::Pattern::Wildcard => infer_wildcard_pattern(&mut state.type_solutions),
    }
}

pub fn check_pattern(
    env: Env,
    state: &mut State,
    pattern: &Spanned<syntax::Pattern>,
    expected: &Type,
) -> Result<CheckedPattern, Error> {
    let result = infer_pattern(env, state, pattern);

    let actual = result.ty(env.common_kinds);
    unification::unify(
        unification::Env {
            common_kinds: env.common_kinds,
            types: env.types,
            type_variables: env.type_variables,
        },
        &mut state.kind_inference_state,
        &mut state.type_solutions,
        expected,
        &actual,
    )
    .map_err(|error| Error::unification_error(env.source, error).with_position(pattern.pos))?;
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
pub fn infer(
    env: Env,
    state: &mut State,
    expr: &Spanned<syntax::Expr>,
) -> Result<(Expr, Type), Error> {
    match &expr.item {
        syntax::Expr::Var(name) => match state.variables.lookup_name(name) {
            Some((index, ty)) => Ok((Expr::Var(index), ty.clone())),
            None => match env.type_signatures.get(name) {
                Some(signature) => match signature {
                    Signature::TypeSig(type_signature) => Ok(state.instantiate(
                        expr.pos,
                        Expr::Name(Name::definition(name.clone())),
                        type_signature,
                    )),
                    Signature::Module(_) => {
                        Err(Error::not_a_value(env.source, name).with_position(expr.pos))
                    }
                },
                None => Err(Error::not_in_scope(env.source, name).with_position(expr.pos)),
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
                        None => {
                            Err(Error::not_in_scope(source, &path[0].item)
                                .with_position(path[0].pos))
                        }
                        Some(signature) => match signature {
                            Signature::TypeSig(_) => {
                                Err(Error::not_a_module(source).with_position(path[0].pos))
                            }
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
                None => Err(Error::not_in_scope(env.source, &item.item).with_position(item.pos)),
                Some(signature) => match signature {
                    Signature::TypeSig(type_signature) => Ok(state.instantiate(
                        expr.pos,
                        Expr::Module {
                            id: *id,
                            path: path.iter().map(|x| x.item.clone()).collect(),
                            item: Name::definition(item.item.clone()),
                        },
                        type_signature,
                    )),
                    Signature::Module(_) => {
                        Err(Error::not_a_value(env.source, &item.item).with_position(item.pos))
                    }
                },
            }
        }

        syntax::Expr::True => Ok((Expr::True, Type::Bool)),
        syntax::Expr::False => Ok((Expr::False, Type::Bool)),
        syntax::Expr::IfThenElse(condition, then_expr, else_expr) => {
            let condition = check(env, state, condition, &Type::Bool)?;
            let (then_expr, then_ty) = infer(env, state, then_expr)?;
            let else_expr = check(env, state, else_expr, &then_ty)?;
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
                    syntax::CmdPart::Expr(expr) => check(
                        env,
                        state,
                        expr,
                        &Type::app(
                            Type::Array(env.common_kinds.type_to_type.clone()),
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
                    syntax::StringPart::String(string) => Ok(StringPart::String(string.clone())),
                    syntax::StringPart::Expr(expr) => {
                        check(env, state, expr, &Type::String).map(StringPart::Expr)
                    }
                })
                .collect::<Result<_, _>>()?;
            Ok((Expr::String(string_parts), Type::String))
        }
        syntax::Expr::Array(items) => {
            let item_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
            let items: Vec<Expr> = items
                .iter()
                .map(|item| check(env, state, item, &item_ty))
                .collect::<Result<_, _>>()?;
            Ok((
                Expr::Array(items),
                Type::app(Type::mk_array(env.common_kinds), item_ty),
            ))
        }

        syntax::Expr::Binop(op, left, right) => match op.item {
            syntax::Binop::Add => {
                let left = check(env, state, left, &Type::Int)?;
                let right = check(env, state, right, &Type::Int)?;
                Ok((Expr::mk_binop(Binop::Add, left, right), Type::Int))
            }
            syntax::Binop::Multiply => {
                let left = check(env, state, left, &Type::Int)?;
                let right = check(env, state, right, &Type::Int)?;
                Ok((Expr::mk_binop(Binop::Multiply, left, right), Type::Int))
            }
            syntax::Binop::Subtract => {
                let left = check(env, state, left, &Type::Int)?;
                let right = check(env, state, right, &Type::Int)?;
                Ok((Expr::mk_binop(Binop::Subtract, left, right), Type::Int))
            }
            syntax::Binop::Divide => {
                let left = check(env, state, left, &Type::Int)?;
                let right = check(env, state, right, &Type::Int)?;
                Ok((Expr::mk_binop(Binop::Divide, left, right), Type::Int))
            }
            syntax::Binop::Append => {
                let item_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
                let array_ty = Type::app(Type::mk_array(env.common_kinds), item_ty);
                let left = check(env, state, left, &array_ty)?;
                let right = check(env, state, right, &array_ty)?;
                Ok((Expr::mk_binop(Binop::Append, left, right), array_ty))
            }
            syntax::Binop::Or => {
                let left = check(env, state, left, &Type::Bool)?;
                let right = check(env, state, right, &Type::Bool)?;
                Ok((Expr::mk_binop(Binop::Or, left, right), Type::Bool))
            }
            syntax::Binop::And => {
                let left = check(env, state, left, &Type::Bool)?;
                let right = check(env, state, right, &Type::Bool)?;
                Ok((Expr::mk_binop(Binop::And, left, right), Type::Bool))
            }
            syntax::Binop::LApply => {
                let in_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
                let out_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
                let left = check(
                    env,
                    state,
                    left,
                    &Type::mk_arrow(env.common_kinds, &in_ty, &out_ty),
                )?;
                let right = check(env, state, right, &in_ty)?;
                Ok((Expr::mk_binop(Binop::LApply, left, right), out_ty))
            }
            syntax::Binop::RApply => {
                let in_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
                let out_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
                let left = check(env, state, left, &in_ty)?;
                let right = check(
                    env,
                    state,
                    right,
                    &Type::mk_arrow(env.common_kinds, &in_ty, &out_ty),
                )?;
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
            let in_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
            let out_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
            let fun = check(
                env,
                state,
                fun,
                &Type::mk_arrow(env.common_kinds, &in_ty, &out_ty),
            )?;
            let arg = check(env, state, arg, &in_ty)?;
            Ok((Expr::mk_app(fun, arg), out_ty))
        }
        syntax::Expr::Lam { args, body } => {
            check_duplicate_args(env.source, args)?;

            let mut inferred_args: Vec<(Pattern, Type)> = Vec::with_capacity(args.len());
            let bound_variables: Vec<(Rc<str>, Type)> = args
                .iter()
                .flat_map(|arg| {
                    let result = infer_pattern(env, state, arg);
                    inferred_args.push((result.pattern(), result.ty(env.common_kinds)));
                    result.names().into_iter()
                })
                .collect();

            state.variables.insert(&bound_variables);
            let (body, body_ty) = infer(env, state, body)?;
            state.variables.delete(bound_variables.len());

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
                    let body_ty = Type::arrow(env.common_kinds, arg_ty, body_ty);

                    (body, body_ty)
                },
            );

            Ok((expr, ty))
        }
        syntax::Expr::Let { name, value, rest } => {
            let (value, value_ty) = infer(env, state, value)?;

            state.variables.insert(&[(name.clone(), value_ty)]);
            let (rest, rest_ty) = infer(env, state, rest)?;
            state.variables.delete(1);

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
                        infer(env, state, field_expr).map(|(field_expr, field_ty)| {
                            let field_name_str = field_name.as_str();
                            field_to_expr.insert(field_name_str, field_expr);
                            field_to_pos.insert(field_name_str, field_pos);
                            (Rc::from(field_name_str), field_ty)
                        })
                    })
                    .collect::<Result<_, _>>()?;
                let rest = rest
                    .as_ref()
                    .map(|_| fresh_type_meta(&mut state.type_solutions, Kind::Row));
                Ok(Type::mk_rows(fields, rest))
            }?;

            let mut expr_fields: Vec<(Expr, Expr)> = Vec::with_capacity(fields.len());
            let mut ty_fields: Vec<(Rc<str>, Type)> = Vec::with_capacity(fields.len());

            let mut row = &entire_row;
            while let Type::RowCons(field, ty, rest) = row {
                let field_index = Expr::Placeholder(state.evidence.placeholder(
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
                    let rest = check(
                        env,
                        state,
                        rest,
                        &Type::mk_app(&Type::mk_record_ctor(env.common_kinds), row),
                    )?;
                    Ok((Some(rest), Some(row.clone())))
                }
                None => {
                    let expected = Type::RowNil;
                    let actual = row;

                    unification::unify(
                        unification::Env {
                            common_kinds: env.common_kinds,
                            types: env.types,
                            type_variables: env.type_variables,
                        },
                        &mut state.kind_inference_state,
                        &mut state.type_solutions,
                        &expected,
                        actual,
                    )
                    .map_err(|error| Error::unification_error(env.source, error))?;

                    Ok((None, None))
                }
            }?;

            Ok((
                Expr::mk_record(expr_fields, expr_rest),
                Type::mk_record(env.common_kinds, ty_fields, ty_rest),
            ))
        }
        syntax::Expr::Project(expr, field) => {
            let field_name: Rc<str> = Rc::from(field.item.as_str());
            let field_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);

            let rest_row = fresh_type_meta(&mut state.type_solutions, Kind::Row);

            let pos = expr.pos;
            let expr = check(
                env,
                state,
                expr,
                &Type::mk_record(
                    env.common_kinds,
                    vec![(field_name.clone(), field_ty.clone())],
                    Some(rest_row.clone()),
                ),
            )?;

            let placeholder = Expr::Placeholder(state.evidence.placeholder(
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

            let rest_row = fresh_type_meta(&mut state.type_solutions, Kind::Row);
            let placeholder = Expr::Placeholder(state.evidence.placeholder(
                pos,
                evidence::Constraint::HasField {
                    field: constructor.clone(),
                    rest: rest_row.clone(),
                },
            ));

            let arg_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
            Ok((
                Expr::mk_variant(placeholder),
                Type::arrow(
                    env.common_kinds,
                    arg_ty.clone(),
                    Type::mk_variant(
                        env.common_kinds,
                        vec![(constructor, arg_ty)],
                        Some(rest_row),
                    ),
                ),
            ))
        }
        syntax::Expr::Embed(constructor, expr) => {
            let rest_row = fresh_type_meta(&mut state.type_solutions, Kind::Row);
            let expr = check(
                env,
                state,
                expr,
                &Type::app(Type::mk_variant_ctor(env.common_kinds), rest_row.clone()),
            )?;

            let constructor_pos = constructor.pos;
            let constructor: Rc<str> = Rc::from(constructor.item.as_str());
            let arg_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
            let placeholder = Expr::Placeholder(state.evidence.placeholder(
                constructor_pos,
                evidence::Constraint::HasField {
                    field: constructor.clone(),
                    rest: rest_row.clone(),
                },
            ));
            Ok((
                Expr::mk_embed(placeholder, expr),
                Type::mk_variant(
                    env.common_kinds,
                    vec![(constructor, arg_ty)],
                    Some(rest_row),
                ),
            ))
        }
        syntax::Expr::Case(expr, branches) => infer_case(env, state, expr, branches),

        syntax::Expr::Comp(_) => {
            panic!("computation expression was not desugared")
        }
    }
}

fn infer_case(
    env: Env,
    state: &mut State,
    expr: &Spanned<syntax::Expr>,
    branches: &[syntax::Branch],
) -> Result<(Expr, Type), Error> {
    let (expr, mut expr_ty) = infer(env, state, expr)?;

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

    let out_ty = fresh_type_meta(&mut state.type_solutions, Kind::Type);
    let mut seen_ctors = FnvHashSet::default();
    let mut saw_catchall = false;
    let branches: Vec<Branch> = branches
        .iter()
        .map(|branch| {
            if pattern_is_redundant(&seen_ctors, saw_catchall, &branch.pattern.item) {
                return Err(Error::redundant_pattern(env.source).with_position(branch.pattern.pos));
            }

            if let syntax::Pattern::Variant { name, .. } = &branch.pattern.item {
                seen_ctors.insert(name.as_ref());
            }

            let result = check_pattern(env, state, &branch.pattern, &expr_ty)?;

            if let CheckedPattern::Variant { rest, .. } = &result {
                expr_ty = Type::app(Type::mk_variant_ctor(env.common_kinds), rest.clone())
            }

            let names = result.names();
            state.variables.insert(&names);
            let body = check(env, state, &branch.body, &out_ty)?;
            state.variables.delete(names.len());

            let pattern = result.pattern();
            if let Pattern::Wildcard | Pattern::Name = pattern {
                saw_catchall = true;
            }

            Ok(Branch { pattern, body })
        })
        .collect::<Result<_, _>>()?;
    state.zonk_type_mut(&mut expr_ty);
    match expr_ty.unwrap_variant() {
        Some(RowParts {
            rest: Some(rest), ..
        }) if !saw_catchall => {
            let expected = Type::RowNil;
            let actual = rest;

            unification::unify(
                unification::Env {
                    common_kinds: env.common_kinds,
                    types: env.types,
                    type_variables: env.type_variables,
                },
                &mut state.kind_inference_state,
                &mut state.type_solutions,
                &expected,
                actual,
            )
            .map_err(|error| Error::unification_error(env.source, error))
        }
        _ => Ok(()),
    }?;
    Ok((Expr::mk_case(expr, branches), out_ty))
}

/// Check an expression's type.
pub fn check(
    env: Env,
    state: &mut State,
    expr: &Spanned<syntax::Expr>,
    expected: &Type,
) -> Result<Expr, Error> {
    let position = expr.pos;
    let (expr, expr_ty) = infer(env, state, expr)?;

    let actual = expr_ty;
    unification::unify(
        unification::Env {
            common_kinds: env.common_kinds,
            types: env.types,
            type_variables: env.type_variables,
        },
        &mut state.kind_inference_state,
        &mut state.type_solutions,
        expected,
        &actual,
    )
    .map_err(|error| Error::unification_error(env.source, error).with_position(position))?;

    Ok(expr)
}
