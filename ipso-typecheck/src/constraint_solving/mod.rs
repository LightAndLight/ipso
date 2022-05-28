#[cfg(test)]
mod test;

use crate::{evidence::Constraint, fill_ty_names, metavariables, type_inference, BoundVars};
use ipso_core::{
    self as core, Binop, Branch, CommonKinds, Expr, Pattern, Placeholder, StringPart, Type,
};
use ipso_diagnostic::Source;
use ipso_syntax::{self as syntax, kind::Kind};
use std::{collections::HashMap, rc::Rc};

/// What gave rise to the [`Implication`]?
#[derive(Debug, Clone, Copy)]
pub enum Sort {
    /**
    This implication exists due to a class definition.

    The implication projects a superclass dictionary from an
    existing dictionary.
    */
    Class,
    /**
    This implication exists due to an instance definition.

    The implication constructs a dictionary given some assumptions.
    */
    Instance,
}

#[derive(Debug, Clone)]
pub struct Implication {
    pub sort: Sort,
    pub ty_vars: Vec<Kind>,
    pub antecedents: Vec<core::Type>,
    pub consequent: core::Type,
    pub evidence: Rc<core::Expr>,
}

impl Implication {
    pub fn instantiate_many(&self, tys: &[core::Type]) -> Self {
        let mut ty_vars = self.ty_vars.clone();
        for _ in tys.iter().rev() {
            let _ = ty_vars.pop();
        }
        let antecedents = self
            .antecedents
            .iter()
            .map(|ty| ty.instantiate_many(tys))
            .collect();
        let consequent = self.consequent.instantiate_many(tys);
        Implication {
            sort: self.sort,
            ty_vars,
            antecedents,
            consequent,
            evidence: self.evidence.clone(),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum ErrorInfo {
    CannotDeduce {
        constraint: syntax::Type<Rc<str>>,
    },
    UnificationError {
        error: type_inference::unification::Error,
    },
}

#[derive(PartialEq, Eq, Debug)]
pub enum ErrorHint {
    WhileSolving { constraint: syntax::Type<Rc<str>> },
}

#[derive(PartialEq, Eq, Debug)]
pub struct Error {
    pub source: Source,
    pub position: Option<usize>,
    pub info: ErrorInfo,
    pub hint: Option<ErrorHint>,
}

impl Error {
    pub fn cannot_deduce(source: Source, constraint: syntax::Type<Rc<str>>) -> Self {
        Error {
            source,
            position: None,
            info: ErrorInfo::CannotDeduce { constraint },
            hint: None,
        }
    }

    pub fn unification_error(source: Source, error: type_inference::unification::Error) -> Self {
        Error {
            source,
            position: None,
            info: ErrorInfo::UnificationError { error },
            hint: None,
        }
    }

    pub fn with_position(mut self, position: usize) -> Self {
        self.position = Some(position);
        self
    }

    pub fn with_hint(mut self, hint: ErrorHint) -> Self {
        self.hint = Some(hint);
        self
    }

    pub fn message(&self) -> String {
        match &self.info {
            ErrorInfo::CannotDeduce { constraint } => {
                let constraint = match &self.hint {
                    Some(hint) => match hint {
                        ErrorHint::WhileSolving { constraint } => constraint,
                    },
                    None => constraint,
                };
                format!("cannot deduce \"{}\"", constraint.render())
            }
            ErrorInfo::UnificationError { error } => error.message(),
        }
    }

    pub fn addendum(&self) -> Option<String> {
        match &self.info {
            ErrorInfo::CannotDeduce { constraint } => match &self.hint {
                Some(hint) => match hint {
                    ErrorHint::WhileSolving {
                        constraint: hint_constraint,
                    } => {
                        if constraint == hint_constraint {
                            None
                        } else {
                            Some(format!(
                                "couldn't find instance for \"{}\"",
                                constraint.render()
                            ))
                        }
                    }
                },
                None => None,
            },
            ErrorInfo::UnificationError { .. } => None,
        }
    }
}

trait A {}

trait B: A {
    fn thingo(&self);
}

/**
Constraint solver environment.

[`solve_placeholder`] and [`solve_constraint`] take many unchanging arguments, so we bundle the
arguments into a struct for convenience.
*/
#[derive(Clone, Copy)]
pub struct Env<'a> {
    pub common_kinds: &'a CommonKinds,
    pub types: &'a HashMap<Rc<str>, Kind>,
    pub type_variables: &'a BoundVars<Kind>,
    pub implications: &'a [Implication],
    pub source: &'a Source,
}

pub fn solve_constraint(
    env: Env,
    type_inference_state: &mut type_inference::State,
    pos: usize,
    constraint: &Constraint,
) -> Result<Rc<core::Expr>, Error> {
    match constraint {
        Constraint::Type(constraint) => {
            debug_assert!(
                type_inference_state.zonk_kind(false, constraint.kind()) == Kind::Constraint
            );

            match type_inference_state.evidence.find(
                &type_inference_state.type_solutions,
                &Constraint::from_type(constraint),
            ) {
                None => {}
                Some(expr) => {
                    return Ok(expr);
                }
            }

            let mut result = None;
            for implication in env.implications.iter() {
                let metas: Vec<core::Type> = implication
                    .ty_vars
                    .iter()
                    .map(|kind| type_inference_state.fresh_type_meta(kind.clone()))
                    .collect();

                let implication = implication.instantiate_many(&metas);

                match type_inference::unification::unify(
                    type_inference::unification::Env {
                        common_kinds: env.common_kinds,
                        types: env.types,
                        type_variables: env.type_variables,
                    },
                    &mut type_inference_state.kind_inference_state,
                    &mut type_inference_state.type_solutions,
                    constraint,
                    &implication.consequent,
                ) {
                    Err(_) => {
                        continue;
                    }
                    Ok(()) => {
                        let implication = Implication {
                            sort: implication.sort,
                            ty_vars: implication.ty_vars,
                            antecedents: implication
                                .antecedents
                                .into_iter()
                                .map(|x| type_inference_state.zonk_type(x))
                                .collect(),
                            consequent: type_inference_state.zonk_type(implication.consequent),
                            evidence: implication.evidence,
                        };

                        let mut evidence_result = implication.evidence;
                        match implication.sort {
                            Sort::Class => {
                                debug_assert!(implication.antecedents.len() == 1);

                                match type_inference_state.evidence.find(
                                    &type_inference_state.type_solutions,
                                    &Constraint::from_type(&implication.antecedents[0]),
                                ) {
                                    None => {
                                        // This implication isn't going to help us. Keep searching.
                                        continue;
                                    }
                                    Some(arg) => {
                                        evidence_result = Rc::new(Expr::app(evidence_result, arg));

                                        // We successfully satisfied all assumptions. Search over.
                                        result = Some(evidence_result);
                                        break;
                                    }
                                }
                            }
                            Sort::Instance => {
                                /*
                                We only backwards-chain for instance-generated implications.

                                Here's the argument:

                                A class-generated implication projects some dictionary E from an instance
                                dictionary D. If we can construct D using the current environment, then we
                                can also construct E using the current environment. Backward-chaining to
                                construct D in order to project E from it would be inefficient; we should
                                just construct E directly.

                                This means that class-generated implications only need to project from
                                already-constructed dictionaries, and that if we can't find such a dictionary
                                then we just continue searching.

                                Example:

                                These definitions

                                ```
                                class Eq a

                                class Eq a => Ord a

                                instance Eq Int

                                instance Ord Int
                                ```

                                give these implications:

                                ```
                                Ord a -{class}----> Eq a
                                      -{instance}-> Eq Int
                                      -{instance}-> Ord Int
                                ```

                                If we require `Eq Int`, we *could* get it by projecting from `Ord Int`.
                                But the fact that we have access to `Ord Int` means that we have more direct
                                access to `Eq Int`. When `instance Ord Int` was defined, we checked that `Eq Int`
                                was constructable under the same assumptions (In this case there are no assumptions.
                                In an example like `instance Ord a => Ord (Array a)` we'd check that given `Ord a` we
                                can construct `Eq (Array a)`).

                                See section 7.3 in *Jones, M. P. (1999, September). Typing haskell in haskell. In Haskell workshop (Vol. 7).*
                                for another explanation.
                                */
                                for antecedent in &implication.antecedents {
                                    match solve_constraint(
                                        env,
                                        type_inference_state,
                                        pos,
                                        &Constraint::from_type(antecedent),
                                    ) {
                                        Err(err) => {
                                            return Err(err);
                                        }
                                        Ok(arg) => {
                                            evidence_result =
                                                Rc::new(Expr::app(evidence_result, arg));
                                        }
                                    }
                                }

                                // We successfully satisfied all assumptions. Search over.
                                result = Some(evidence_result);
                                break;
                            }
                        }
                    }
                }
            }

            match result {
                None => Err(Error::cannot_deduce(
                    env.source.clone(),
                    fill_ty_names(
                        env.type_variables,
                        type_inference_state
                            .zonk_type(constraint.clone())
                            .to_syntax(),
                    ),
                )
                .with_position(pos)),
                Some(evidence) => Ok(evidence),
            }
        }
        Constraint::HasField { field, rest } => {
            debug_assert!(type_inference_state.zonk_kind(false, rest.kind()) == Kind::Row);

            let new_evidence: Rc<core::Expr> = match rest {
                core::Type::RowNil => Ok(Rc::new(core::Expr::Int(0))),
                core::Type::RowCons(other_field, _, other_rest) => {
                    if field <= other_field {
                        solve_constraint(
                            env,
                            type_inference_state,
                            pos,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: other_rest.as_ref().clone(),
                            },
                        )
                    } else {
                        let ev = solve_constraint(
                            env,
                            type_inference_state,
                            pos,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: other_rest.as_ref().clone(),
                            },
                        )?;

                        Ok(Rc::new(core::Expr::mk_binop_l(
                            Binop::Add,
                            core::Expr::Int(1),
                            ev,
                        )))
                    }
                }

                core::Type::Var(_, _) => {
                    let evidence_result = type_inference_state
                        .evidence
                        .find(&type_inference_state.type_solutions, constraint);
                    match evidence_result {
                        None => {
                            // we're allow to conjure evidence for non-extistent HasField constraints,
                            // so the user doesn't have to write them
                            let ev = type_inference_state.evidence.assume(0, constraint.clone());
                            debug_assert!(type_inference_state.evidence.lookup_evar(&ev) != None);
                            Ok(Rc::new(core::Expr::EVar(ev)))
                        }
                        Some(evidence) => Ok(evidence),
                    }
                }
                core::Type::Meta(kind, n) => {
                    let sol = type_inference_state.type_solutions.get(*n);
                    // we assume solving is done after unification, so any unsolved variables
                    // will never recieve solutions
                    match sol.clone() {
                        metavariables::Solution::Unsolved => {
                            match type_inference_state.zonk_kind(false, kind.clone()) {
                                // row metavariables can be safely defaulted to the empty row in the
                                // presence of ambiguity
                                Kind::Row => {
                                    type_inference::unification::unify(
                                        type_inference::unification::Env {
                                            common_kinds: env.common_kinds,
                                            types: env.types,
                                            type_variables: env.type_variables,
                                        },
                                        &mut type_inference_state.kind_inference_state,
                                        &mut type_inference_state.type_solutions,
                                        &core::Type::RowNil,
                                        rest,
                                    )
                                    .map_err(|error| {
                                        Error::unification_error(env.source.clone(), error)
                                    })?;
                                    solve_constraint(env, type_inference_state, pos, constraint)
                                }
                                _ => match type_inference_state
                                    .evidence
                                    .find(&type_inference_state.type_solutions, constraint)
                                {
                                    None => {
                                        panic!(
                                        "impossible: cannot deduce HasField({:?}, {:?}) given {:?}",
                                        field, rest, type_inference_state.evidence
                                    )
                                    }
                                    Some(evidence) => Ok(evidence),
                                },
                            }
                        }
                        metavariables::Solution::Solved(sol) => solve_constraint(
                            env,
                            type_inference_state,
                            pos,
                            &Constraint::HasField {
                                field: field.clone(),
                                rest: sol,
                            },
                        ),
                    }
                }

                _ => {
                    let evidence_result = type_inference_state
                        .evidence
                        .find(&type_inference_state.type_solutions, constraint);
                    match evidence_result {
                        None => {
                            panic!(
                                "impossible: cannot deduce HasField({:?}, {:?})",
                                field, rest
                            )
                        }
                        Some(evidence) => Ok(evidence),
                    }
                }
            }?;

            Ok(new_evidence)
        }
        Constraint::DebugRecordFields(row) => {
            struct DebugRecordFieldsResult {
                fields: Vec<Expr>,
                field_offsets: Vec<Expr>,
                rest: Option<Rc<Expr>>,
            }

            fn go(
                env: Env,
                type_inference_state: &mut type_inference::State,
                pos: usize,
                constraint: &Constraint,
                mut record_fields: Vec<Expr>,
                mut field_offsets: Vec<Expr>,
                entire_row: &Type,
                current_row: &Type,
            ) -> Result<DebugRecordFieldsResult, Error> {
                match current_row {
                    Type::RowNil => Ok(DebugRecordFieldsResult {
                        fields: record_fields,
                        field_offsets,
                        rest: None,
                    }),
                    Type::RowCons(field_name, field_type, rest) => {
                        // debug_dict : { debug : <field_type> -> String }
                        let debug_dict = solve_constraint(
                            env,
                            type_inference_state,
                            pos,
                            &Constraint::from_type(&Type::mk_app(
                                &Type::Name(
                                    Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                                    Rc::from("Debug"),
                                ),
                                field_type,
                            )),
                        )?;

                        // field_offset : Int
                        let field_offset = solve_constraint(
                            env,
                            type_inference_state,
                            pos,
                            &Constraint::HasField {
                                field: field_name.clone(),
                                rest: entire_row.clone(),
                            },
                        )?;
                        field_offsets.push(field_offset.as_ref().clone());

                        // { field : String, value : String }
                        record_fields.push(Expr::mk_record(
                            vec![
                                (
                                    Expr::Int(0),
                                    Expr::String(vec![StringPart::from(field_name.as_ref())]),
                                ),
                                // debug_dict.debug record.<field_name>
                                (
                                    Expr::Int(1),
                                    Expr::App(
                                        // debug_dict.debug
                                        Rc::new(Expr::Project(debug_dict, Rc::new(Expr::Int(0)))),
                                        // record.<field_name>
                                        Rc::new(Expr::Project(Rc::new(Expr::Var(0)), field_offset)),
                                    ),
                                ),
                            ],
                            None,
                        ));

                        go(
                            env,
                            type_inference_state,
                            pos,
                            constraint,
                            record_fields,
                            field_offsets,
                            entire_row,
                            rest,
                        )
                    }
                    ty => {
                        let constraint = Constraint::DebugRecordFields(ty.clone());
                        let evidence_result = type_inference_state
                            .evidence
                            .find(&type_inference_state.type_solutions, &constraint);
                        match evidence_result {
                            None => Err(Error::cannot_deduce(
                                env.source.clone(),
                                fill_ty_names(
                                    env.type_variables,
                                    type_inference_state
                                        .zonk_type(constraint.to_type())
                                        .to_syntax(),
                                ),
                            )
                            .with_position(pos)),
                            Some(evidence) => Ok(DebugRecordFieldsResult {
                                fields: record_fields,
                                field_offsets,
                                rest: Some(evidence),
                            }),
                        }
                    }
                }
            }

            let zonked_row = type_inference_state.zonk_type(row.clone());

            let DebugRecordFieldsResult {
                fields,
                field_offsets,
                rest,
            } = go(
                env,
                type_inference_state,
                pos,
                constraint,
                Vec::new(),
                Vec::new(),
                &zonked_row,
                &zonked_row,
            )?;

            // \record ->
            //    <fields> ++ debugRecordFields ((\{ <fields>, ..rest } -> rest) record)
            Ok(Rc::from(Expr::mk_lam(
                true,
                match rest {
                    Some(debug_record_fields) => Expr::mk_binop(
                        Binop::Append,
                        Expr::Array(fields),
                        // debugRecordFields ((\{ f_1, f_2, ..., f_n, ..rest } -> rest) record)
                        Expr::App(
                            debug_record_fields,
                            /*
                            \x ->
                              case x of
                                { f_1, f_2, ..., f_n, ..rest } -> rest
                            */
                            Rc::new(Expr::mk_app(
                                Expr::mk_lam(
                                    true,
                                    Expr::mk_case(
                                        Expr::Var(0),
                                        vec![Branch {
                                            pattern: Pattern::Record {
                                                names: field_offsets,
                                                rest: true,
                                            },
                                            body: Expr::Var(0),
                                        }],
                                    ),
                                ),
                                Expr::Var(0),
                            )),
                        ),
                    ),
                    None => Expr::Array(fields),
                },
            )))
        }
        Constraint::DebugVariantCtor(row) => {
            fn go(
                env: Env,
                type_inference_state: &mut type_inference::State,
                pos: usize,
                constraint: &Constraint,
                mut case_branches: Vec<Branch<core::Expr>>,
                current_row: &Type,
            ) -> Result<Vec<Branch<core::Expr>>, Error> {
                match current_row {
                    Type::RowNil => Ok(case_branches),

                    /*
                    If the this meta is not mentioned in the type of the expression that generated
                    it, then it's ambiguous and we can default it to the empty row.
                    */
                    Type::Meta(kind, _) => {
                        debug_assert!(kind == &Kind::Row);

                        type_inference::unification::unify(
                            type_inference::unification::Env {
                                common_kinds: env.common_kinds,
                                types: env.types,
                                type_variables: env.type_variables,
                            },
                            &mut type_inference_state.kind_inference_state,
                            &mut type_inference_state.type_solutions,
                            &core::Type::RowNil,
                            current_row,
                        )
                        .map_err(|error| Error::unification_error(env.source.clone(), error))?;

                        Ok(case_branches)
                    }
                    Type::RowCons(field_name, field_type, rest) => {
                        // debug_dict : { debug : <field_type> -> String }
                        let debug_dict = solve_constraint(
                            env,
                            type_inference_state,
                            pos,
                            &Constraint::from_type(&Type::mk_app(
                                &Type::Name(
                                    Kind::mk_arrow(&Kind::Type, &Kind::Constraint),
                                    Rc::from("Debug"),
                                ),
                                field_type,
                            )),
                        )?;

                        // field_offset : Int
                        let field_offset = solve_constraint(
                            env,
                            type_inference_state,
                            pos,
                            &Constraint::HasField {
                                field: field_name.clone(),
                                /*
                                The HasField constraint for DebugRecordFields uses the entire
                                row, but for variants we only use the rest of the row.

                                See [note: peeling constructors when matching on variants] for
                                an explanation of why we do this.
                                */
                                rest: rest.as_ref().clone(),
                            },
                        )?;

                        /*
                        <field_name> value ->
                          { ctor = "<field name>", value = debug_dict.debug value }
                        */
                        case_branches.push(Branch {
                            pattern: Pattern::Variant { tag: field_offset },
                            body: Expr::mk_record(
                                vec![
                                    (
                                        Expr::Int(0),
                                        Expr::String(vec![StringPart::from(field_name.as_ref())]),
                                    ),
                                    // debug_dict.debug value
                                    (
                                        Expr::Int(1),
                                        Expr::App(
                                            // debug_dict.debug
                                            Rc::new(Expr::Project(
                                                debug_dict,
                                                Rc::new(Expr::Int(0)),
                                            )),
                                            // value
                                            Rc::new(Expr::Var(0)),
                                        ),
                                    ),
                                ],
                                None,
                            ),
                        });

                        go(
                            env,
                            type_inference_state,
                            pos,
                            constraint,
                            case_branches,
                            rest,
                        )
                    }
                    ty => {
                        let constraint = Constraint::DebugVariantCtor(ty.clone());
                        let evidence_result = type_inference_state
                            .evidence
                            .find(&type_inference_state.type_solutions, &constraint);
                        match evidence_result {
                            None => Err(Error::cannot_deduce(
                                env.source.clone(),
                                fill_ty_names(
                                    env.type_variables,
                                    type_inference_state
                                        .zonk_type(constraint.to_type())
                                        .to_syntax(),
                                ),
                            )
                            .with_position(pos)),
                            Some(evidence) => {
                                case_branches.push(Branch {
                                    pattern: Pattern::Name,
                                    body: Expr::App(evidence, Rc::new(Expr::Var(0))),
                                });
                                Ok(case_branches)
                            }
                        }
                    }
                }
            }

            let zonked_row = type_inference_state.zonk_type(row.clone());
            let case_branches = go(
                env,
                type_inference_state,
                pos,
                constraint,
                Vec::new(),
                &zonked_row,
            )?;

            Ok(
                /*
                \variant -> case variant of
                  <case_branches>
                */
                Rc::from(Expr::mk_lam(
                    true,
                    Expr::mk_case(Expr::Var(0), case_branches),
                )),
            )
        }
    }
}

pub fn solve_placeholder(
    env: Env,
    type_inference_state: &mut type_inference::State,
    p: Placeholder,
) -> Result<(Rc<core::Expr>, Constraint), Error> {
    let item = type_inference_state.evidence.environment[p.0].clone();
    match item.expr {
        None => {
            let expr = solve_constraint(env, type_inference_state, item.pos, &item.constraint)
                .map_err(|error| {
                    error.with_hint(ErrorHint::WhileSolving {
                        constraint: fill_ty_names(
                            env.type_variables,
                            type_inference_state
                                .zonk_type(item.constraint.to_type())
                                .to_syntax(),
                        ),
                    })
                })?;
            type_inference_state.evidence.environment[p.0].expr = Some(expr.clone());
            Ok((expr, item.constraint.clone()))
        }
        Some(evidence) => Ok((evidence, item.constraint.clone())),
    }
}
