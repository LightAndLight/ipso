pub mod evidence;
pub mod kind_inference;
pub mod metavariables;
#[cfg(test)]
mod test;
pub mod type_inference;

use diagnostic::{Location, Message};
use evidence::{solver::solve_placeholder, Constraint, Evidence};
use ipso_builtins as builtins;
use ipso_core::{self as core, CommonKinds};
use ipso_diagnostic::{self as diagnostic, Source};
use ipso_syntax::{self as syntax, kind::Kind, Spanned};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    rc::Rc,
    todo,
};
use syntax::{ModuleId, Modules};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BoundVars<A> {
    indices: HashMap<Rc<str>, Vec<usize>>,
    info: Vec<(Rc<str>, A)>,
}

impl<A> BoundVars<A> {
    fn new() -> Self {
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

#[derive(Debug, Clone)]
pub struct Implication {
    pub ty_vars: Vec<Kind>,
    pub antecedents: Vec<core::Type>,
    pub consequent: core::Type,
    pub evidence: core::Expr,
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
            ty_vars,
            antecedents,
            consequent,
            evidence: self.evidence.clone(),
        }
    }
}

pub struct Typechecker<'modules> {
    common_kinds: &'modules CommonKinds,
    source: Source,
    kind_solutions: kind_inference::Solutions,
    pub type_solutions: type_inference::Solutions,
    pub implications: Vec<Implication>,
    pub evidence: Evidence,
    type_context: HashMap<Rc<str>, Kind>,
    context: HashMap<String, core::TypeSig>,
    pub registered_bindings: HashMap<String, (core::TypeSig, Rc<core::Expr>)>,
    class_context: HashMap<Rc<str>, core::ClassDeclaration>,
    bound_vars: BoundVars<core::Type>,
    bound_tyvars: BoundVars<Kind>,
    position: Option<usize>,
    modules: &'modules Modules<core::Module>,
    /**
    Cached context for modules that have been imported.

    Ideally we would use a module path and item name to look up type signatures
    in `modules`, but that's currently too resource intensive.
    */
    module_context: HashMap<ModuleId, HashMap<String, core::TypeSig>>,
}

macro_rules! with_position {
    ($self:expr, $pos:expr, $val:expr) => {{
        let old = $self.position;
        $self.position = Some($pos);
        let res = $val;
        $self.position = old;
        res
    }};
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UnifyKindContext<A> {
    ty: syntax::Type<A>,
    has_kind: Kind,
    unifying_types: Option<UnifyTypeContext>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UnifyKindContextRefs<'a> {
    ty: &'a core::Type,
    has_kind: &'a Kind,
    unifying_types: Option<&'a UnifyTypeContextRefs<'a>>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UnifyTypeContext {
    pub expected: syntax::Type<Rc<str>>,
    pub actual: syntax::Type<Rc<str>>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UnifyTypeContextRefs<'a> {
    pub expected: &'a core::Type,
    pub actual: &'a core::Type,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SolveConstraintContext {
    pub constraint: syntax::Type<Rc<str>>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum TypeError {
    TypeError {
        error: type_inference::InferenceError,
    },
    DuplicateClassArgument {
        source: Source,
        pos: usize,
    },
    NotInScope {
        source: Source,
        pos: usize,
        name: String,
    },
    NotInModule {
        source: Source,
        pos: usize,
        item: String,
    },
    KindError {
        source: Source,
        pos: usize,
        error: kind_inference::InferenceError,
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
    CannotDeduce {
        source: Source,
        pos: usize,
        context: Option<SolveConstraintContext>,
    },
}

impl TypeError {
    pub fn source(&self) -> Source {
        match self {
            TypeError::TypeError { error } => error.source.clone(),
            TypeError::KindError { source, .. } => source.clone(),
            TypeError::NotInScope { source, .. } => source.clone(),
            TypeError::NotInModule { source, .. } => source.clone(),
            TypeError::DuplicateClassArgument { source, .. } => source.clone(),
            TypeError::NoSuchClass { source, .. } => source.clone(),
            TypeError::NotAMember { source, .. } => source.clone(),
            TypeError::CannotDeduce { source, .. } => source.clone(),
        }
    }

    pub fn position(&self) -> usize {
        match self {
            TypeError::TypeError { error } => error.position.unwrap_or(0),
            TypeError::KindError { pos, .. } => *pos,
            TypeError::NotInScope { pos, .. } => *pos,
            TypeError::NotInModule { pos, .. } => *pos,
            TypeError::DuplicateClassArgument { pos, .. } => *pos,
            TypeError::NoSuchClass { pos, .. } => *pos,
            TypeError::NotAMember { pos, .. } => *pos,
            TypeError::CannotDeduce { pos, .. } => *pos,
        }
    }

    pub fn message(&self) -> String {
        match self {
            TypeError::TypeError { error, .. } => match &error.info {
                type_inference::InferenceErrorInfo::UnificationError { error } => match error {
                    type_inference::UnificationError::Mismatch { expected, actual } => format!(
                        "expected type \"{}\", got type \"{}\"",
                        expected.render(),
                        actual.render()
                    ),
                    type_inference::UnificationError::Occurs { meta, ty } => format!(
                        "infinite type from equating ?{} with \"{}\"",
                        meta,
                        ty.render()
                    ),

                    type_inference::UnificationError::KindError { error } => {
                        render_kind_inference_error(error)
                    }
                },
                type_inference::InferenceErrorInfo::CompExprEndsWith { end } => {
                    String::from(match end {
                        type_inference::CompExprEnd::None => "empty computation expression",
                        type_inference::CompExprEnd::Bind => {
                            "computation expression ends with a bind"
                        }
                        type_inference::CompExprEnd::Let => {
                            "computation expression ends with a let"
                        }
                    })
                }

                type_inference::InferenceErrorInfo::NotInScope { .. } => {
                    String::from("variable not in scope")
                }
                type_inference::InferenceErrorInfo::DuplicateArgument { .. } => {
                    String::from("duplicate argument")
                }
                type_inference::InferenceErrorInfo::RedundantPattern => {
                    String::from("redundant pattern")
                }
            },
            TypeError::KindError { error, .. } => render_kind_inference_error(error),
            TypeError::NotInScope { .. } => String::from("not in scope"),
            TypeError::NotInModule { .. } => String::from("not in scope"),
            TypeError::DuplicateClassArgument { .. } => {
                String::from("duplicate type class argument")
            }
            TypeError::NoSuchClass { .. } => String::from("type class not in scope"),
            TypeError::NotAMember { cls, .. } => {
                format!("not a member of the {:?} type class", cls)
            }
            TypeError::CannotDeduce { context, .. } => match context {
                None => String::from("cannot deduce"),
                Some(context) => format!("cannot deduce \"{:}\"", context.constraint.render()),
            },
        }
    }

    pub fn addendum(&self) -> Option<String> {
        match self {
            TypeError::TypeError { error, .. } => match &error.info {
                type_inference::InferenceErrorInfo::UnificationError { .. } => None,
                type_inference::InferenceErrorInfo::CompExprEndsWith { .. } => None,
                type_inference::InferenceErrorInfo::NotInScope { .. } => None,
                type_inference::InferenceErrorInfo::DuplicateArgument { .. } => None,
                type_inference::InferenceErrorInfo::RedundantPattern { .. } => None,
            },
            TypeError::KindError { error, .. } => match error.info {
                kind_inference::InferenceErrorInfo::NotInScope { .. } => None,
                kind_inference::InferenceErrorInfo::UnificationError { .. } => {
                    error.hint.as_ref().map(|hint| match hint {
                        kind_inference::InferenceErrorHint::WhileChecking { ty, has_kind } => {
                            format!(
                                "While checking that \"{}\" has kind \"{}\"",
                                ty.render(),
                                has_kind.render()
                            )
                        }
                        kind_inference::InferenceErrorHint::WhileInferring { ty } => {
                            format!("While inferring the kind of \"{}\"", ty.render())
                        }
                    })
                }
            },
            TypeError::DuplicateClassArgument { .. } => None,
            TypeError::NotInScope { .. } => None,
            TypeError::NotInModule { .. } => None,
            TypeError::NoSuchClass { .. } => None,
            TypeError::NotAMember { .. } => None,
            TypeError::CannotDeduce { .. } => None,
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

fn render_kind_inference_error(error: &kind_inference::InferenceError) -> String {
    match &error.info {
        kind_inference::InferenceErrorInfo::NotInScope { .. } => String::from("type not in scope"),
        kind_inference::InferenceErrorInfo::UnificationError {
            error: unification_error,
        } => match unification_error {
            kind_inference::UnificationError::Mismatch { expected, actual } => {
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
            kind_inference::UnificationError::Occurs { meta, kind } => {
                format!(
                    "infinite kind from equating ?{} with \"{}\"",
                    meta,
                    kind.render()
                )
            }
        },
    }
}

#[macro_export]
macro_rules! with_tc {
    ($location:expr, $f:expr) => {{
        use ipso_core::CommonKinds;
        use ipso_syntax::Modules;

        let common_kinds = CommonKinds::default();
        let modules = Modules::new();
        let tc = Typechecker::new_with_builtins($location, &common_kinds, &modules);
        $f(tc)
    }};
}

#[macro_export]
macro_rules! current_dir_with_tc {
    ($f:expr) => {{
        crate::with_tc!(
            ipso_diagnostic::Source::Interactive {
                label: String::from("(typechecker)")
            },
            $f
        )
    }};
}

/// The results of typechecking a pattern
#[derive(Debug, PartialEq, Eq)]
pub struct CheckedPattern {
    /// The elaborated pattern
    pub pattern: core::Pattern,
    /// The variables bound by the pattern, and their types
    pub bindings: Vec<(Rc<str>, core::Type)>,
}

/// The results of inferring a type for a pattern
#[derive(Debug, PartialEq, Eq)]
pub struct InferredPattern {
    /// The elaborated pattern
    pub pattern: core::Pattern,
    /// The pattern's inferred type
    pub r#type: core::Type,
    /// The variables bound by the pattern, and their types
    pub bindings: Vec<(Rc<str>, core::Type)>,
}

impl<'modules> Typechecker<'modules> {
    pub fn new(
        source: Source,
        common_kinds: &'modules CommonKinds,
        modules: &'modules Modules<core::Module>,
    ) -> Self {
        Typechecker {
            common_kinds,
            source,
            kind_solutions: kind_inference::Solutions::new(),
            type_solutions: type_inference::Solutions::new(),
            implications: Vec::new(),
            evidence: Evidence::new(),
            type_context: HashMap::new(),
            context: HashMap::new(),
            registered_bindings: HashMap::new(),
            class_context: HashMap::new(),
            bound_vars: BoundVars::new(),
            bound_tyvars: BoundVars::new(),
            position: None,
            modules,
            module_context: HashMap::new(),
        }
    }

    pub fn new_with_builtins(
        location: Source,
        common_kinds: &'modules CommonKinds,
        modules: &'modules Modules<core::Module>,
    ) -> Self {
        let mut tc = Self::new(location, common_kinds, modules);
        tc.register_from_import(&builtins::builtins(tc.common_kinds), &syntax::Names::All);
        tc
    }

    fn eq_zonked_type(&self, t1: &core::Type, t2: &core::Type) -> bool {
        fn zonk_just_enough<'a>(tc: &'a Typechecker, t: &'a core::Type) -> &'a core::Type {
            match t {
                core::Type::Meta(_, n) => match &tc.type_solutions.get(*n) {
                    metavariables::Solution::Unsolved => t,
                    metavariables::Solution::Solved(sol) => zonk_just_enough(tc, sol),
                },
                t => t,
            }
        }
        let t2: &core::Type = zonk_just_enough(self, t2);
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
                            .all(|(a, b)| self.eq_zonked_type(a, b))
                }
                _ => false,
            },
            core::Type::App(_, a, b) => match t2 {
                core::Type::App(_, a2, b2) => {
                    self.eq_zonked_type(a, a2) && self.eq_zonked_type(b, b2)
                }
                _ => false,
            },
            core::Type::RowCons(a, b, c) => match t2 {
                core::Type::RowCons(a2, b2, c2) => {
                    a == a2 && self.eq_zonked_type(b, b2) && self.eq_zonked_type(c, c2)
                }
                _ => false,
            },
            core::Type::HasField(a, b) => match t2 {
                core::Type::HasField(a2, b2) => a == a2 && self.eq_zonked_type(b, b2),
                _ => false,
            },
            core::Type::Meta(_, n) => match &self.type_solutions.get(*n) {
                metavariables::Solution::Unsolved => match t2 {
                    core::Type::Meta(_, n2) => n == n2,
                    _ => false,
                },
                metavariables::Solution::Solved(sol) => self.eq_zonked_type(sol, t2),
            },
        }
    }

    pub fn eq_zonked_constraint(
        &self,
        c1: &evidence::Constraint,
        c2: &evidence::Constraint,
    ) -> bool {
        match c1 {
            evidence::Constraint::HasField { field, rest } => match c2 {
                evidence::Constraint::HasField {
                    field: field2,
                    rest: rest2,
                } => field == field2 && self.eq_zonked_type(rest, rest2),
                _ => false,
            },
            evidence::Constraint::Type(ty) => match c2 {
                evidence::Constraint::Type(ty2) => self.eq_zonked_type(ty, ty2),
                _ => false,
            },
        }
    }

    pub fn register_class(&mut self, decl: &core::ClassDeclaration) {
        let decl_name: Rc<str> = Rc::from(decl.name.as_ref());
        let decl_name_kind = decl
            .args
            .iter()
            .rev()
            .fold(Kind::Constraint, |acc, (_, arg_kind)| {
                Kind::mk_arrow(arg_kind, &acc)
            });
        let decl_ty = core::Type::unsafe_mk_name(decl_name.clone(), decl_name_kind);

        // generate constraint's kind
        let mut constraint_kind = Kind::Constraint;
        for (_, kind) in decl.args.iter().rev() {
            constraint_kind = Kind::mk_arrow(kind, &Kind::Constraint);
        }
        self.type_context.insert(decl_name.clone(), constraint_kind);

        // generate superclass accessors
        let applied_type =
            decl.args
                .iter()
                .enumerate()
                .fold(decl_ty, |acc, (arg_index, (_, arg_kind))| {
                    core::Type::app(acc, core::Type::unsafe_mk_var(arg_index, arg_kind.clone()))
                });
        self.implications
            .extend(
                decl.supers
                    .iter()
                    .enumerate()
                    .map(|(pos, superclass)| Implication {
                        ty_vars: decl.args.iter().map(|(_, kind)| kind.clone()).collect(),
                        antecedents: vec![applied_type.clone()],
                        consequent: superclass.clone(),
                        evidence: core::Expr::mk_lam(
                            true,
                            core::Expr::mk_project(core::Expr::Var(0), core::Expr::Int(pos as u32)),
                        ),
                    }),
            );

        // generate class members
        let decl_bindings = decl.get_bindings(self.common_kinds);
        self.context.extend(
            decl_bindings
                .iter()
                .map(|(name, (sig, _))| (name.clone(), sig.clone())),
        );
        self.registered_bindings.extend(decl_bindings);

        // update class context
        self.class_context.insert(decl_name, decl.clone());
    }

    pub fn register_instance(
        &mut self,
        ty_vars: &[(Rc<str>, Kind)],
        superclass_constructors: &[core::Expr],
        assumes: &[core::Type],
        head: &core::Type,
        members: &[core::InstanceMember],
    ) {
        let mut dictionary: Vec<core::Expr> = superclass_constructors.to_vec();
        dictionary.extend(members.iter().map(|member| member.body.clone()));

        for (ix, _assume) in assumes.iter().enumerate().rev() {
            for item in &mut dictionary {
                *item = core::Expr::mk_app((*item).clone(), core::Expr::Var(ix));
            }
        }

        let mut evidence = core::Expr::mk_record(
            dictionary
                .into_iter()
                .enumerate()
                .map(|(ix, val)| (core::Expr::Int(ix as u32), val))
                .collect(),
            None,
        );
        for _assume in assumes.iter() {
            evidence = core::Expr::mk_lam(true, evidence);
        }

        self.implications.push(Implication {
            ty_vars: ty_vars.iter().map(|(_, a)| a.clone()).collect(),
            antecedents: Vec::from(assumes),
            consequent: head.clone(),
            evidence,
        });
    }

    pub fn register_declaration(&mut self, decl: &core::Declaration) {
        match decl {
            core::Declaration::BuiltinType { name, kind } => {
                self.type_context
                    .insert(Rc::from(name.as_str()), kind.clone());
            }
            core::Declaration::Definition { name, sig, body } => {
                self.context.insert(name.clone(), sig.clone());
                self.registered_bindings
                    .insert(name.clone(), (sig.clone(), body.clone()));
            }
            core::Declaration::TypeAlias { name, args, body } => {
                todo!("register TypeAlias {:?}", (name, args, body))
            }
            core::Declaration::Class(decl) => self.register_class(decl),
            core::Declaration::Instance {
                ty_vars,
                superclass_constructors,
                assumes,
                head,
                members,
            } => self.register_instance(ty_vars, superclass_constructors, assumes, head, members),
        }
    }

    pub fn check_module(&mut self, module: &syntax::Module) -> Result<core::Module, TypeError> {
        let decls = module.decls.iter().fold(Ok(vec![]), |acc, decl| {
            acc.and_then(|mut decls| {
                self.check_declaration(decl).map(|m_decl| match m_decl {
                    Option::None => decls,
                    Option::Some(decl) => {
                        self.register_declaration(&decl);
                        decls.push(decl);
                        decls
                    }
                })
            })
        })?;
        Ok(core::Module { decls })
    }

    pub fn register_from_import(&mut self, module: &core::Module, names: &syntax::Names) {
        let should_import = |expected_name: &str| -> bool {
            match names {
                syntax::Names::All => true,
                syntax::Names::Names(names) => names.iter().any(|name| name.item == expected_name),
            }
        };
        for decl in &module.decls {
            match decl {
                core::Declaration::BuiltinType { name, kind: _ } => {
                    if should_import(name) {
                        self.register_declaration(decl);
                    }
                }
                core::Declaration::Definition {
                    name,
                    sig: _,
                    body: _,
                } => {
                    if should_import(name) {
                        self.register_declaration(decl);
                    }
                }
                core::Declaration::TypeAlias {
                    name,
                    args: _,
                    body: _,
                } => {
                    if should_import(name) {
                        self.register_declaration(decl);
                    }
                }
                core::Declaration::Class(core::ClassDeclaration { name, .. }) => {
                    if should_import(name) {
                        self.register_declaration(decl);
                    }
                }
                core::Declaration::Instance { .. } => {
                    self.register_declaration(decl);
                }
            }
        }
    }

    fn abstract_evidence(
        &mut self,
        mut expr: core::Expr,
    ) -> Result<(core::Expr, Vec<core::Type>), TypeError> {
        expr.subst_placeholder(&mut |p| {
            let (expr, _solved_constraint) = solve_placeholder(self, *p)?;
            Ok(expr)
        })?;

        let mut unsolved_constraints: Vec<(core::EVar, core::Type)> = Vec::new();
        let mut seen_evars: HashSet<core::EVar> = HashSet::new();
        for ev in expr.iter_evars() {
            if !seen_evars.contains(ev) {
                seen_evars.insert(*ev);
                let constraint = self.evidence.lookup_evar(ev).unwrap();
                unsolved_constraints.push((*ev, self.zonk_type(constraint.to_type())));
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

    fn generalise(
        &mut self,
        expr: core::Expr,
        ty: core::Type,
    ) -> Result<(core::Expr, core::TypeSig), TypeError> {
        let (expr, unsolved_constraints) = self.abstract_evidence(expr)?;

        let mut ty = self.zonk_type(ty);
        for constraint in unsolved_constraints.into_iter().rev() {
            ty = core::Type::mk_fatarrow(self.common_kinds, constraint, ty);
        }

        let ty_vars: Vec<(Rc<str>, Kind)> = self
            .bound_tyvars
            .info
            .iter()
            .map(|(name, kind)| (name.clone(), self.zonk_kind(true, kind.clone())))
            .collect();
        let sig = core::TypeSig::new(ty_vars, ty);

        Ok((expr, sig))
    }

    fn check_kind(
        &mut self,
        ty: &syntax::Type<Rc<str>>,
        kind: &Kind,
    ) -> Result<core::Type, TypeError> {
        let mut ctx = kind_inference::InferenceContext::new(
            self.common_kinds,
            &self.type_context,
            &self.bound_tyvars,
            &mut self.kind_solutions,
        );
        kind_inference::check(&mut ctx, ty, kind).map_err(|error| TypeError::KindError {
            source: self.source(),
            pos: self.current_position(),
            error,
        })
    }

    fn infer_kind(
        &mut self,
        pos: usize,
        ty: &syntax::Type<Rc<str>>,
    ) -> Result<(core::Type, Kind), TypeError> {
        let mut ctx = kind_inference::InferenceContext::new(
            self.common_kinds,
            &self.type_context,
            &self.bound_tyvars,
            &mut self.kind_solutions,
        );
        kind_inference::infer(&mut ctx, ty).map_err(|error| TypeError::KindError {
            source: self.source(),
            pos,
            error,
        })
    }

    fn check_definition(
        &mut self,
        name: &str,
        ty: &syntax::Type<Rc<str>>,
        args: &[Spanned<syntax::Pattern>],
        body: &Spanned<syntax::Expr>,
    ) -> Result<core::Declaration, TypeError> {
        let ty_var_kinds: Vec<(Rc<str>, Kind)> = {
            let mut seen_names: HashSet<&str> = HashSet::new();
            ty.iter_vars()
                .filter_map(|name| {
                    if !seen_names.contains(name.as_ref()) {
                        seen_names.insert(name);
                        Some((name.clone(), self.fresh_kind_meta()))
                    } else {
                        None
                    }
                })
                .collect()
        };

        self.bound_tyvars.insert(&ty_var_kinds);

        let ty = self.check_kind(ty, &Kind::Type)?;

        let _ = self.context.insert(
            name.to_string(),
            core::TypeSig::new(ty_var_kinds.clone(), ty.clone()),
        );

        let (constraints, ty) = ty.unwrap_constraints();
        for constraint in constraints {
            self.evidence.assume(
                /*
                TODO: use the constraint's textual position. unwrap constraints before
                type checking to get those positions.
                */
                0,
                evidence::Constraint::from_type(constraint),
            );
        }

        let arg_tys: Vec<type_inference::InferredPattern> = args
            .iter()
            .map(|arg| self.type_inference_context().infer_pattern(arg))
            .collect();
        let out_ty = self.fresh_type_meta(&Kind::Type);

        self.unify_type(
            ty,
            &arg_tys.iter().rev().fold(out_ty.clone(), |acc, el| {
                core::Type::mk_arrow(self.common_kinds, &el.ty(self.common_kinds), &acc)
            }),
        )?;

        let arg_bound_vars = arg_tys
            .iter()
            .flat_map(|arg_ty| arg_ty.names().into_iter())
            .collect::<Vec<_>>();
        self.bound_vars.insert(&arg_bound_vars);
        let body = self.check_type(body, &out_ty)?;
        self.bound_vars.delete(arg_bound_vars.len());

        let body = arg_tys.into_iter().rev().fold(body, |body, arg_ty| {
            let pattern = arg_ty.pattern();
            match &pattern {
                core::Pattern::Char(_)
                | core::Pattern::Int(_)
                | core::Pattern::String(_)
                | core::Pattern::Record { .. }
                | core::Pattern::Variant { .. } => core::Expr::mk_lam(
                    true,
                    core::Expr::mk_case(core::Expr::Var(0), vec![core::Branch { pattern, body }]),
                ),
                core::Pattern::Name => core::Expr::mk_lam(true, body),
                core::Pattern::Wildcard => core::Expr::mk_lam(false, body),
            }
        });

        let (body, sig) = self.generalise(body, ty.clone())?;
        self.evidence = Evidence::new();

        self.bound_tyvars.delete(ty_var_kinds.len());

        self.context.remove(name);

        Ok(core::Declaration::Definition {
            name: name.to_string(),
            sig,
            body: Rc::new(body),
        })
    }

    fn check_class_member(
        &mut self,
        class_args_kinds: &[(Rc<str>, Kind)],
        name: &str,
        ty: &syntax::Type<Rc<str>>,
    ) -> Result<core::ClassMember, TypeError> {
        let ty_var_kinds: Vec<(Rc<str>, Kind)> = {
            let mut seen_names: HashSet<&str> = class_args_kinds
                .iter()
                .map(|(name, _)| name.as_ref())
                .collect();
            ty.iter_vars()
                .filter_map(|name| {
                    if !seen_names.contains(name.as_ref()) {
                        seen_names.insert(name);
                        Some((name.clone(), self.fresh_kind_meta()))
                    } else {
                        None
                    }
                })
                .collect()
        };

        self.bound_tyvars.insert(&ty_var_kinds);
        let ty = self.check_kind(ty, &Kind::Type)?;
        self.bound_tyvars.delete(ty_var_kinds.len());

        let ty_vars: Vec<(Rc<str>, Kind)> = ty_var_kinds
            .into_iter()
            .map(|(name, kind)| (name, self.zonk_kind(true, kind)))
            .collect();
        let sig = core::TypeSig::new(ty_vars, self.zonk_type(ty));
        Ok(core::ClassMember {
            name: name.to_string(),
            sig,
        })
    }

    fn check_class(
        &mut self,
        supers: &[Spanned<syntax::Type<Rc<str>>>],
        name: &Rc<str>,
        args: &[Spanned<Rc<str>>],
        members: &[(String, syntax::Type<Rc<str>>)],
    ) -> Result<core::Declaration, TypeError> {
        let args_kinds: Vec<(Rc<str>, Kind)> = {
            let mut seen_names: HashSet<&str> = HashSet::new();
            args.iter()
                .map(|arg| {
                    if !seen_names.contains(arg.item.as_ref()) {
                        seen_names.insert(arg.item.as_ref());
                        Ok((arg.item.clone(), self.fresh_kind_meta()))
                    } else {
                        Err(TypeError::DuplicateClassArgument {
                            source: self.source(),
                            pos: arg.pos,
                        })
                    }
                })
                .collect::<Result<_, _>>()
        }?;

        self.bound_tyvars.insert(&args_kinds);

        let supers = supers
            .iter()
            .map(|superclass| {
                with_position!(self, superclass.pos, {
                    self.check_kind(&superclass.item, &Kind::Constraint)
                })
            })
            .collect::<Result<_, _>>()?;

        let members = members
            .iter()
            .map(|(member_name, member_type)| {
                self.check_class_member(&args_kinds, member_name, member_type)
            })
            .collect::<Result<_, _>>()?;

        self.bound_tyvars.delete(args_kinds.len());

        Ok(core::Declaration::Class(core::ClassDeclaration {
            supers,
            name: name.clone(),
            args: args_kinds
                .into_iter()
                .map(|(name, kind)| (name, self.zonk_kind(true, kind)))
                .collect::<Vec<(Rc<str>, Kind)>>(),
            members,
        }))
    }

    fn check_instance(
        &mut self,
        assumes: &[Spanned<syntax::Type<Rc<str>>>],
        name: &Spanned<Rc<str>>,
        args: &[Spanned<syntax::Type<Rc<str>>>],
        members: &[(
            Spanned<String>,
            Vec<Spanned<syntax::Pattern>>,
            Spanned<syntax::Expr>,
        )],
    ) -> Result<core::Declaration, TypeError> {
        let class_context = &self.class_context;
        let class_decl: core::ClassDeclaration = match class_context.get(&name.item) {
            None => Err(TypeError::NoSuchClass {
                source: self.source(),
                pos: name.pos,
            }),
            Some(class_decl) => Ok(class_decl.clone()),
        }?;

        let name_item: Rc<str> = Rc::from(name.item.as_ref());

        let head = args.iter().fold(syntax::Type::Name(name_item), |acc, el| {
            syntax::Type::mk_app(acc, el.item.clone())
        });

        let ty_var_kinds: Vec<(Rc<str>, Kind)> = {
            let mut seen_names: HashSet<&str> = HashSet::new();
            args.iter()
                .flat_map(|arg| arg.item.iter_vars())
                .filter_map(|name| {
                    if !seen_names.contains(name.as_ref()) {
                        seen_names.insert(name);
                        Some((name.clone(), self.fresh_kind_meta()))
                    } else {
                        None
                    }
                })
                .collect()
        };

        self.bound_tyvars.insert(&ty_var_kinds);

        let args: Vec<core::Type> = args
            .iter()
            .map(|arg| {
                let res = self.infer_kind(arg.pos, &arg.item)?;
                Ok(res.0)
            })
            .collect::<Result<_, TypeError>>()?;

        let assumes: Vec<core::Type> = assumes
            .iter()
            .map(|assume| {
                let constraint = self.check_kind(&assume.item, &Kind::Constraint)?;
                let _ = self
                    .evidence
                    .assume(assume.pos, evidence::Constraint::from_type(&constraint));
                Ok(constraint)
            })
            .collect::<Result<_, _>>()?;

        // locate evidence for superclasses
        let superclass_constructors: Vec<core::Expr> = {
            let mut superclass_constructors = Vec::new();

            for superclass in &class_decl.supers {
                let superclass = superclass.instantiate_many(&args);

                match evidence::solver::solve_constraint(
                    name.pos,
                    &Some(SolveConstraintContext {
                        constraint: self.fill_ty_names(superclass.to_syntax()),
                    }),
                    self,
                    &evidence::Constraint::from_type(&superclass),
                )
                .and_then(|evidence| self.abstract_evidence(evidence))
                {
                    Err(err) => {
                        return Err(err);
                    }
                    Ok((evidence, _)) => {
                        superclass_constructors.push(evidence);
                    }
                }
            }
            superclass_constructors
        };

        let instantiated_class_members: Vec<core::ClassMember> = class_decl
            .members
            .iter()
            .map(|class_member| core::ClassMember {
                name: class_member.name.clone(),
                sig: class_member.sig.clone().instantiate_many(&args),
            })
            .collect();

        let head = with_position!(self, name.pos, self.check_kind(&head, &Kind::Constraint))?;

        // type check members
        let mut new_members = Vec::with_capacity(members.len());
        for (member_name, member_args, member_body) in members {
            match instantiated_class_members
                .iter()
                .find(|class_member| class_member.name == member_name.item)
            {
                None => {
                    return Err(TypeError::NotAMember {
                        source: self.source(),
                        pos: member_name.pos,
                        cls: name.item.clone(),
                    })
                }
                Some(member_type) => {
                    self.bound_tyvars.insert(&member_type.sig.ty_vars);

                    match self
                        .check_type(
                            &Spanned {
                                pos: member_name.pos,
                                item: syntax::Expr::mk_lam(
                                    member_args.clone(),
                                    member_body.clone(),
                                ),
                            },
                            &member_type.sig.body,
                        )
                        .and_then(|member_body| {
                            self.generalise(member_body, member_type.sig.body.clone())
                        }) {
                        Err(err) => return Err(err),
                        Ok((member_body, _)) => {
                            self.bound_tyvars.delete(member_type.sig.ty_vars.len());
                            new_members.push(core::InstanceMember {
                                name: member_name.item.clone(),
                                body: member_body,
                            });
                        }
                    };
                }
            }
        }
        self.evidence = Evidence::new();

        self.bound_tyvars.delete(ty_var_kinds.len());

        let ty_vars = ty_var_kinds
            .into_iter()
            .map(|(name, kind)| (name, self.zonk_kind(true, kind)))
            .collect();

        Ok(core::Declaration::Instance {
            ty_vars,
            superclass_constructors,
            assumes: assumes
                .into_iter()
                .map(|assume| self.zonk_type(assume))
                .collect(),
            head: self.zonk_type(head),
            members: new_members,
        })
    }

    fn check_declaration(
        &mut self,
        decl: &syntax::Spanned<syntax::Declaration>,
    ) -> Result<Option<core::Declaration>, TypeError> {
        match &decl.item {
            syntax::Declaration::Definition {
                name,
                ty,
                args,
                body,
            } => self
                .check_definition(name, ty, args, body)
                .map(Option::Some),
            syntax::Declaration::TypeAlias { name, args, body } => {
                todo!("check type alias {:?}", (name, args, body))
            }

            syntax::Declaration::Import { resolved, .. } => {
                let id = resolved.unwrap_or_else(|| panic!("unresolved Import"));

                let signatures = self.modules.lookup(id).get_signatures(self.common_kinds);
                self.module_context.insert(id, signatures);

                Ok(None)
            }
            syntax::Declaration::FromImport { resolved, .. } => {
                let id = resolved.unwrap_or_else(|| panic!("unresolved FromImport"));

                let signatures = self.modules.lookup(id).get_signatures(self.common_kinds);
                self.module_context.insert(id, signatures);

                Ok(None)
            }

            syntax::Declaration::Class {
                supers,
                name,
                args,
                members,
            } => self
                .check_class(supers, name, args, members)
                .map(Option::Some),
            syntax::Declaration::Instance {
                assumes,
                name,
                args,
                members,
            } => self
                .check_instance(assumes, name, args, members)
                .map(Option::Some),
        }
    }

    pub fn source(&self) -> Source {
        self.source.clone()
    }

    fn current_position(&self) -> usize {
        self.position.unwrap_or(0)
    }

    pub fn zonk_constraint(&self, constraint: &Constraint) -> Constraint {
        match constraint {
            Constraint::HasField { field, rest } => Constraint::HasField {
                field: field.clone(),
                rest: self.zonk_type(rest.clone()),
            },
            Constraint::Type(ty) => Constraint::Type(self.zonk_type(ty.clone())),
        }
    }

    pub fn zonk_type(&self, ty: core::Type) -> core::Type {
        self.type_solutions.zonk(&self.kind_solutions, ty)
    }

    pub fn zonk_kind(&self, close_unsolved: bool, kind: Kind) -> Kind {
        self.kind_solutions.zonk(close_unsolved, kind)
    }

    pub fn fresh_type_meta(&mut self, kind: &Kind) -> core::Type {
        core::Type::Meta(kind.clone(), self.type_solutions.fresh_meta())
    }

    fn fresh_kind_meta(&mut self) -> Kind {
        Kind::Meta(self.kind_solutions.fresh_meta())
    }

    fn type_inference_context(&mut self) -> type_inference::InferenceContext {
        type_inference::InferenceContext::new(
            self.common_kinds,
            &self.source,
            &self.module_context,
            &self.type_context,
            &self.bound_tyvars,
            &mut self.kind_solutions,
            &mut self.type_solutions,
            &self.context,
            &mut self.bound_vars,
            &mut self.evidence,
        )
    }

    pub fn unify_type(
        &mut self,
        expected: &core::Type,
        actual: &core::Type,
    ) -> Result<(), TypeError> {
        self.type_inference_context()
            .unify(None, expected, actual)
            .map_err(|error| TypeError::TypeError { error })
    }

    pub fn fill_ty_names(&self, ty: syntax::Type<usize>) -> syntax::Type<Rc<str>> {
        ty.map(&mut |&ix| self.bound_tyvars.lookup_index(ix).unwrap().0.clone())
    }

    pub fn infer_type(
        &mut self,
        expr: &syntax::Spanned<syntax::Expr>,
    ) -> Result<(core::Expr, core::Type), TypeError> {
        type_inference::infer(&mut self.type_inference_context(), expr)
            .map_err(|error| TypeError::TypeError { error })
    }

    pub fn check_type(
        &mut self,
        expr: &syntax::Spanned<syntax::Expr>,
        ty: &core::Type,
    ) -> Result<core::Expr, TypeError> {
        type_inference::check(&mut self.type_inference_context(), expr, ty)
            .map_err(|error| TypeError::TypeError { error })
    }
}
