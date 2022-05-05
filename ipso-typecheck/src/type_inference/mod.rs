//! Type checking and inference.

#[cfg(test)]
mod test;

pub mod unification;

use crate::{
    evidence::{self, Evidence},
    kind_inference,
    metavariables::Meta,
    BoundVars,
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
pub enum InferenceErrorInfo {
    UnificationError { error: unification::Error },
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

    /// Construct a [`unification::Error::Occurs`].
    pub fn occurs(source: &Source, meta: Meta, ty: syntax::Type<Rc<str>>) -> Self {
        InferenceError::unification_error(source, unification::Error::Occurs { meta, ty })
    }

    /// Construct a [`unification::Error::Mismatch`].
    pub fn mismatch(
        source: &Source,
        expected: syntax::Type<Rc<str>>,
        actual: syntax::Type<Rc<str>>,
    ) -> Self {
        InferenceError::unification_error(source, unification::Error::Mismatch { expected, actual })
    }

    /// Lift a [`unification::Error`].
    pub fn unification_error(source: &Source, error: unification::Error) -> Self {
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
    type_solutions: &'a mut unification::Solutions,
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
        type_solutions: &'a mut unification::Solutions,
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
}

fn check_duplicate_args(
    ctx: &InferenceContext,
    args: &[Spanned<syntax::Pattern>],
) -> Result<(), InferenceError> {
    let mut seen: HashSet<&str> = HashSet::new();
    args.iter()
        .flat_map(|arg| arg.item.get_arg_names().into_iter())
        .try_for_each(|arg| {
            if seen.contains(&arg.item.as_ref()) {
                Err(
                    InferenceError::duplicate_argument(ctx.source, arg.item.clone())
                        .with_position(arg.pos),
                )
            } else {
                seen.insert(&arg.item);
                Ok(())
            }
        })
}

fn infer_name_pattern(ctx: &mut InferenceContext, name: &Spanned<Rc<str>>) -> InferredPattern {
    let name_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
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
    ctx: &mut InferenceContext,
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
                    fresh_type_meta(ctx.type_solutions, &Kind::Type),
                )
            })
            .collect();
        let rest = rest.map(|_| fresh_type_meta(ctx.type_solutions, &Kind::Row));
        Type::mk_rows(fields, rest)
    };

    let (names, names_tys): (Vec<Expr>, Vec<(Rc<str>, Type)>) = {
        let mut names: Vec<Expr> = Vec::with_capacity(names.len());
        let mut names_tys: Vec<(Rc<str>, Type)> = Vec::with_capacity(names.len());

        let mut row: &Type = &entire_row;
        while let Type::RowCons(field, ty, rest) = row {
            names.push(Expr::Placeholder(ctx.evidence.placeholder(
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
                Type::app(Type::mk_record_ctor(ctx.common_kinds), row.clone()),
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
        ty: Type::app(Type::mk_record_ctor(ctx.common_kinds), entire_row),
    }
}

fn infer_variant_pattern(
    ctx: &mut InferenceContext,
    pos: usize,
    ctor: &str,
    arg: &Spanned<Rc<str>>,
) -> InferredPattern {
    let ctor: Rc<str> = Rc::from(ctor);
    let arg_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
    let rest_row = fresh_type_meta(ctx.type_solutions, &Kind::Row);
    let tag = Expr::Placeholder(ctx.evidence.placeholder(
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

fn infer_wildcard_pattern(ctx: &mut InferenceContext) -> InferredPattern {
    InferredPattern::Any {
        pattern: Pattern::Wildcard,
        names: Vec::new(),
        ty: fresh_type_meta(ctx.type_solutions, &Kind::Type),
    }
}

pub fn infer_pattern(
    ctx: &mut InferenceContext,
    pattern: &Spanned<syntax::Pattern>,
) -> InferredPattern {
    match &pattern.item {
        syntax::Pattern::Name(name) => infer_name_pattern(ctx, name),
        syntax::Pattern::Record { names, rest } => infer_record_pattern(ctx, names, rest.as_ref()),
        syntax::Pattern::Variant { name, arg } => {
            infer_variant_pattern(ctx, pattern.pos, name, arg)
        }
        syntax::Pattern::Char(c) => infer_char_pattern(c),
        syntax::Pattern::Int(n) => infer_int_pattern(n),
        syntax::Pattern::String(s) => infer_string_pattern(s),
        syntax::Pattern::Wildcard => infer_wildcard_pattern(ctx),
    }
}

fn check_pattern(
    ctx: &mut InferenceContext,
    pattern: &Spanned<syntax::Pattern>,
    expected: &Type,
) -> Result<CheckedPattern, InferenceError> {
    let result = infer_pattern(ctx, pattern);
    unify(
        ctx.common_kinds,
        ctx.types,
        ctx.type_variables,
        ctx.kind_solutions,
        ctx.type_solutions,
        ctx.source,
        Some(pattern.pos),
        expected,
        &result.ty(ctx.common_kinds),
    )?;
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

fn check_case(
    ctx: &mut InferenceContext,
    expr: &Spanned<syntax::Expr>,
    branches: &[syntax::Branch],
) -> Result<(Expr, Type), InferenceError> {
    let (expr, mut expr_ty) = infer(ctx, expr)?;

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

    A consequence of ctx is that the tags associated with each variant pattern
    aren't unique. The above example gives:

    ```
    case expr of
      # A's tag is 0
      A x -> ...

      # B's tag is also 0 (because `B` is lexicographically the first constructor
      # in `(| B : b, c : C, d : D |)`)
      B y -> ...

      c -> ...

    The interpreter needs to account for ctx when checking pattern matches.
    ```
    */

    let out_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
    let mut seen_ctors = FnvHashSet::default();
    let mut saw_catchall = false;
    let branches: Vec<Branch> = branches
        .iter()
        .map(|branch| {
            if pattern_is_redundant(&seen_ctors, saw_catchall, &branch.pattern.item) {
                return Err(
                    InferenceError::redundant_pattern(ctx.source).with_position(branch.pattern.pos)
                );
            }

            if let syntax::Pattern::Variant { name, .. } = &branch.pattern.item {
                seen_ctors.insert(name.as_ref());
            }

            let result = check_pattern(ctx, &branch.pattern, &expr_ty)?;

            if let CheckedPattern::Variant { rest, .. } = &result {
                expr_ty = Type::app(Type::mk_variant_ctor(ctx.common_kinds), rest.clone())
            }

            let names = result.names();
            ctx.variables.insert(&names);
            let body = check(ctx, &branch.body, &out_ty)?;
            ctx.variables.delete(names.len());

            let pattern = result.pattern();
            if let Pattern::Wildcard | Pattern::Name = pattern {
                saw_catchall = true;
            }

            Ok(Branch { pattern, body })
        })
        .collect::<Result<_, _>>()?;
    zonk_type_mut(ctx.kind_solutions, ctx.type_solutions, &mut expr_ty);
    match expr_ty.unwrap_variant() {
        Some(RowParts {
            rest: Some(rest), ..
        }) if !saw_catchall => unify(
            ctx.common_kinds,
            ctx.types,
            ctx.type_variables,
            ctx.kind_solutions,
            ctx.type_solutions,
            ctx.source,
            None,
            &Type::RowNil,
            rest,
        ),
        _ => Ok(()),
    }?;
    Ok((Expr::mk_case(expr, branches), out_ty))
}

/// Generate a fresh kind metavariable.
pub fn fresh_kind_meta(kind_solutions: &mut kind_inference::Solutions) -> Kind {
    Kind::Meta(kind_solutions.fresh_meta())
}

/// Generate a fresh type metavariable.
pub fn fresh_type_meta(type_solutions: &mut unification::Solutions, kind: &Kind) -> Type {
    Type::Meta(kind.clone(), type_solutions.fresh_meta())
}

/// Substitute all solved type and kind metavariables in a type.
pub fn zonk_type(
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut unification::Solutions,
    ty: Type,
) -> Type {
    type_solutions.zonk(kind_solutions, ty)
}

/// A mutable version of [`zonk_type`].
pub fn zonk_type_mut(
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut unification::Solutions,
    ty: &mut Type,
) {
    type_solutions.zonk_mut(kind_solutions, ty);
}

/// Infer an expression's type.
pub fn infer(
    ctx: &mut InferenceContext,
    expr: &Spanned<syntax::Expr>,
) -> Result<(Expr, Type), InferenceError> {
    match &expr.item {
        syntax::Expr::Var(name) => match ctx.variables.lookup_name(name) {
            Some((index, ty)) => Ok((Expr::Var(index), ty.clone())),
            None => match ctx.type_signatures.get(name) {
                Some(signature) => match signature {
                    Signature::TypeSig(type_signature) => Ok(ctx.instantiate(
                        expr.pos,
                        Expr::Name(Name::definition(name.clone())),
                        type_signature,
                    )),
                    Signature::Module(_) => {
                        Err(InferenceError::not_a_value(ctx.source, name).with_position(expr.pos))
                    }
                },
                None => Err(InferenceError::not_in_scope(ctx.source, name).with_position(expr.pos)),
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
                                Err(InferenceError::not_a_module(source).with_position(path[0].pos))
                            }
                            Signature::Module(definitions) => {
                                lookup_path(source, definitions, &path[1..])
                            }
                        },
                    }
                }
            }

            let definitions = match id {
                syntax::ModuleRef::This => ctx.type_signatures,
                syntax::ModuleRef::Id(id) => match ctx.modules.get(id) {
                    None => {
                        /*
                        A module accessor will only be desugared if the module was in scope, so ctx case
                        is impossible as long as `ctx.modules` is valid w.r.t ctx expression.
                        */
                        panic!(
                            "module not in scope. id: {:?}, path: {:?}, item: {:?}",
                            id, path, item
                        )
                    }
                    Some(definitions) => definitions,
                },
            };

            let definitions = lookup_path(ctx.source, definitions, path)?;

            match definitions.get(&item.item) {
                None => {
                    Err(InferenceError::not_in_scope(ctx.source, &item.item)
                        .with_position(item.pos))
                }
                Some(signature) => match signature {
                    Signature::TypeSig(type_signature) => Ok(ctx.instantiate(
                        expr.pos,
                        Expr::Module {
                            id: *id,
                            path: path.iter().map(|x| x.item.clone()).collect(),
                            item: Name::definition(item.item.clone()),
                        },
                        type_signature,
                    )),
                    Signature::Module(_) => {
                        Err(InferenceError::not_a_value(ctx.source, &item.item)
                            .with_position(item.pos))
                    }
                },
            }
        }

        syntax::Expr::True => Ok((Expr::True, Type::Bool)),
        syntax::Expr::False => Ok((Expr::False, Type::Bool)),
        syntax::Expr::IfThenElse(condition, then_expr, else_expr) => {
            let condition = check(ctx, condition, &Type::Bool)?;
            let (then_expr, then_ty) = infer(ctx, then_expr)?;
            let else_expr = check(ctx, else_expr, &then_ty)?;
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
                        ctx,
                        expr,
                        &Type::app(
                            Type::Array(ctx.common_kinds.type_to_type.clone()),
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
                        check(ctx, expr, &Type::String).map(StringPart::Expr)
                    }
                })
                .collect::<Result<_, _>>()?;
            Ok((Expr::String(string_parts), Type::String))
        }
        syntax::Expr::Array(items) => {
            let item_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
            let items: Vec<Expr> = items
                .iter()
                .map(|item| check(ctx, item, &item_ty))
                .collect::<Result<_, _>>()?;
            Ok((
                Expr::Array(items),
                Type::app(Type::mk_array(ctx.common_kinds), item_ty),
            ))
        }

        syntax::Expr::Binop(op, left, right) => match op.item {
            syntax::Binop::Add => {
                let left = check(ctx, left, &Type::Int)?;
                let right = check(ctx, right, &Type::Int)?;
                Ok((Expr::mk_binop(Binop::Add, left, right), Type::Int))
            }
            syntax::Binop::Multiply => {
                let left = check(ctx, left, &Type::Int)?;
                let right = check(ctx, right, &Type::Int)?;
                Ok((Expr::mk_binop(Binop::Multiply, left, right), Type::Int))
            }
            syntax::Binop::Subtract => {
                let left = check(ctx, left, &Type::Int)?;
                let right = check(ctx, right, &Type::Int)?;
                Ok((Expr::mk_binop(Binop::Subtract, left, right), Type::Int))
            }
            syntax::Binop::Divide => {
                let left = check(ctx, left, &Type::Int)?;
                let right = check(ctx, right, &Type::Int)?;
                Ok((Expr::mk_binop(Binop::Divide, left, right), Type::Int))
            }
            syntax::Binop::Append => {
                let item_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
                let array_ty = Type::app(Type::mk_array(ctx.common_kinds), item_ty);
                let left = check(ctx, left, &array_ty)?;
                let right = check(ctx, right, &array_ty)?;
                Ok((Expr::mk_binop(Binop::Append, left, right), array_ty))
            }
            syntax::Binop::Or => {
                let left = check(ctx, left, &Type::Bool)?;
                let right = check(ctx, right, &Type::Bool)?;
                Ok((Expr::mk_binop(Binop::Or, left, right), Type::Bool))
            }
            syntax::Binop::And => {
                let left = check(ctx, left, &Type::Bool)?;
                let right = check(ctx, right, &Type::Bool)?;
                Ok((Expr::mk_binop(Binop::And, left, right), Type::Bool))
            }
            syntax::Binop::LApply => {
                let in_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
                let out_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
                let left = check(
                    ctx,
                    left,
                    &Type::mk_arrow(ctx.common_kinds, &in_ty, &out_ty),
                )?;
                let right = check(ctx, right, &in_ty)?;
                Ok((Expr::mk_binop(Binop::LApply, left, right), out_ty))
            }
            syntax::Binop::RApply => {
                let in_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
                let out_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
                let left = check(ctx, left, &in_ty)?;
                let right = check(
                    ctx,
                    right,
                    &Type::mk_arrow(ctx.common_kinds, &in_ty, &out_ty),
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
            let in_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
            let out_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
            let fun = check(ctx, fun, &Type::mk_arrow(ctx.common_kinds, &in_ty, &out_ty))?;
            let arg = check(ctx, arg, &in_ty)?;
            Ok((Expr::mk_app(fun, arg), out_ty))
        }
        syntax::Expr::Lam { args, body } => {
            check_duplicate_args(ctx, args)?;

            let mut inferred_args: Vec<(Pattern, Type)> = Vec::with_capacity(args.len());
            let bound_variables: Vec<(Rc<str>, Type)> = args
                .iter()
                .flat_map(|arg| {
                    let result = infer_pattern(ctx, arg);
                    inferred_args.push((result.pattern(), result.ty(ctx.common_kinds)));
                    result.names().into_iter()
                })
                .collect();

            ctx.variables.insert(&bound_variables);
            let (body, body_ty) = infer(ctx, body)?;
            ctx.variables.delete(bound_variables.len());

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
                    let body_ty = Type::arrow(ctx.common_kinds, arg_ty, body_ty);

                    (body, body_ty)
                },
            );

            Ok((expr, ty))
        }
        syntax::Expr::Let { name, value, rest } => {
            let (value, value_ty) = infer(ctx, value)?;

            ctx.variables.insert(&[(name.clone(), value_ty)]);
            let (rest, rest_ty) = infer(ctx, rest)?;
            ctx.variables.delete(1);

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
                        infer(ctx, field_expr).map(|(field_expr, field_ty)| {
                            let field_name_str = field_name.as_str();
                            field_to_expr.insert(field_name_str, field_expr);
                            field_to_pos.insert(field_name_str, field_pos);
                            (Rc::from(field_name_str), field_ty)
                        })
                    })
                    .collect::<Result<_, _>>()?;
                let rest = rest
                    .as_ref()
                    .map(|_| fresh_type_meta(ctx.type_solutions, &Kind::Row));
                Ok(Type::mk_rows(fields, rest))
            }?;

            let mut expr_fields: Vec<(Expr, Expr)> = Vec::with_capacity(fields.len());
            let mut ty_fields: Vec<(Rc<str>, Type)> = Vec::with_capacity(fields.len());

            let mut row = &entire_row;
            while let Type::RowCons(field, ty, rest) = row {
                let field_index = Expr::Placeholder(ctx.evidence.placeholder(
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
                        ctx,
                        rest,
                        &Type::mk_app(&Type::mk_record_ctor(ctx.common_kinds), row),
                    )?;
                    Ok((Some(rest), Some(row.clone())))
                }
                None => {
                    unify(
                        ctx.common_kinds,
                        ctx.types,
                        ctx.type_variables,
                        ctx.kind_solutions,
                        ctx.type_solutions,
                        ctx.source,
                        None,
                        &Type::RowNil,
                        row,
                    )?;
                    Ok((None, None))
                }
            }?;

            Ok((
                Expr::mk_record(expr_fields, expr_rest),
                Type::mk_record(ctx.common_kinds, ty_fields, ty_rest),
            ))
        }
        syntax::Expr::Project(expr, field) => {
            let field_name: Rc<str> = Rc::from(field.item.as_str());
            let field_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);

            let rest_row = fresh_type_meta(ctx.type_solutions, &Kind::Row);

            let pos = expr.pos;
            let expr = check(
                ctx,
                expr,
                &Type::mk_record(
                    ctx.common_kinds,
                    vec![(field_name.clone(), field_ty.clone())],
                    Some(rest_row.clone()),
                ),
            )?;

            let placeholder = Expr::Placeholder(ctx.evidence.placeholder(
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

            let rest_row = fresh_type_meta(ctx.type_solutions, &Kind::Row);
            let placeholder = Expr::Placeholder(ctx.evidence.placeholder(
                pos,
                evidence::Constraint::HasField {
                    field: constructor.clone(),
                    rest: rest_row.clone(),
                },
            ));

            let arg_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
            Ok((
                Expr::mk_variant(placeholder),
                Type::arrow(
                    ctx.common_kinds,
                    arg_ty.clone(),
                    Type::mk_variant(
                        ctx.common_kinds,
                        vec![(constructor, arg_ty)],
                        Some(rest_row),
                    ),
                ),
            ))
        }
        syntax::Expr::Embed(constructor, expr) => {
            let rest_row = fresh_type_meta(ctx.type_solutions, &Kind::Row);
            let expr = check(
                ctx,
                expr,
                &Type::app(Type::mk_variant_ctor(ctx.common_kinds), rest_row.clone()),
            )?;

            let constructor_pos = constructor.pos;
            let constructor: Rc<str> = Rc::from(constructor.item.as_str());
            let arg_ty = fresh_type_meta(ctx.type_solutions, &Kind::Type);
            let placeholder = Expr::Placeholder(ctx.evidence.placeholder(
                constructor_pos,
                evidence::Constraint::HasField {
                    field: constructor.clone(),
                    rest: rest_row.clone(),
                },
            ));
            Ok((
                Expr::mk_embed(placeholder, expr),
                Type::mk_variant(
                    ctx.common_kinds,
                    vec![(constructor, arg_ty)],
                    Some(rest_row),
                ),
            ))
        }
        syntax::Expr::Case(expr, branches) => check_case(ctx, expr, branches),

        syntax::Expr::Comp(_) => {
            panic!("computation expression was not desugared")
        }
    }
}

/// Check an expression's type.
pub fn check(
    ctx: &mut InferenceContext,
    expr: &Spanned<syntax::Expr>,
    expected: &Type,
) -> Result<Expr, InferenceError> {
    let position = expr.pos;
    let (expr, expr_ty) = infer(ctx, expr)?;
    unify(
        ctx.common_kinds,
        ctx.types,
        ctx.type_variables,
        ctx.kind_solutions,
        ctx.type_solutions,
        ctx.source,
        Some(position),
        expected,
        &expr_ty,
    )?;
    Ok(expr)
}

/// Unify two types.
pub fn unify(
    common_kinds: &CommonKinds,
    types: &HashMap<Rc<str>, Kind>,
    type_variables: &BoundVars<Kind>,
    kind_solutions: &mut kind_inference::Solutions,
    type_solutions: &mut unification::Solutions,
    source: &Source,
    position: Option<usize>,
    expected: &Type,
    actual: &Type,
) -> Result<(), InferenceError> {
    unification::unify(
        common_kinds,
        types,
        type_variables,
        kind_solutions,
        type_solutions,
        expected,
        actual,
    )
    .map_err(|error| {
        let error = InferenceError::unification_error(
            source,
            /*
            At the level of an `InferenceError`, a type mismatch should
            describe full types involved, rather than the specific components
            that don't match.

            e.g. when `a -> b` and `a -> c` mismatch (because `b` != `c`),
            `InferenceError` should report that `a -> b` != `a -> c`, instead
            of saying `b` != `c`.
            */
            match error {
                unification::Error::Mismatch { .. } => unification::Error::Mismatch {
                    expected: zonk_type(kind_solutions, type_solutions, expected.clone())
                        .to_syntax()
                        .map(&mut |ix| type_variables.lookup_index(*ix).unwrap().0.clone()),
                    actual: zonk_type(kind_solutions, type_solutions, actual.clone())
                        .to_syntax()
                        .map(&mut |ix| type_variables.lookup_index(*ix).unwrap().0.clone()),
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
