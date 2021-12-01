pub mod evidence;
pub mod substitution;
#[cfg(test)]
mod test;

use self::substitution::Substitution;
use diagnostic::{Location, Message};
use evidence::{solver::solve_placeholder, Constraint, Evidence};
use fnv::FnvHashSet;
use ipso_builtins as builtins;
use ipso_core::{self as core, CommonKinds, ModulePath};
use ipso_diagnostic::{self as diagnostic, Source};
use ipso_rope::Rope;
use ipso_syntax::{
    self as syntax,
    kind::{Kind, KindCompound},
    ModuleName, Spanned,
};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    path::Path,
    rc::Rc,
    todo,
};

#[derive(Debug, PartialEq, Eq)]
struct BoundVars<A> {
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
    kind_solutions: Vec<Option<Kind>>,
    pub type_solutions: Vec<(Kind, Option<core::Type>)>,
    pub implications: Vec<Implication>,
    pub evidence: Evidence,
    type_context: HashMap<Rc<str>, Kind>,
    context: HashMap<String, core::TypeSig>,
    pub registered_bindings: HashMap<String, (core::TypeSig, Rc<core::Expr>)>,
    module_context: HashMap<ModulePath, HashMap<String, core::TypeSig>>,
    module_unmapping: HashMap<ModuleName, ModulePath>,
    class_context: HashMap<Rc<str>, core::ClassDeclaration>,
    bound_vars: BoundVars<core::Type>,
    bound_tyvars: BoundVars<Kind>,
    position: Option<usize>,
    modules: &'modules HashMap<ModulePath, &'modules core::Module>,
    working_dir: &'modules Path,
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
    pub pos: usize,
    pub constraint: syntax::Type<Rc<str>>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum TypeError {
    DuplicateArgument {
        source: Source,
        pos: usize,
        name: String,
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
    KindMismatch {
        source: Source,
        pos: usize,
        context: UnifyKindContext<Rc<str>>,
        expected: Kind,
        actual: Kind,
    },
    TypeMismatch {
        source: Source,
        pos: usize,
        context: UnifyTypeContext,
        expected: syntax::Type<Rc<str>>,
        actual: syntax::Type<Rc<str>>,
    },
    RedundantPattern {
        source: Source,
        pos: usize,
    },
    KindOccurs {
        source: Source,
        pos: usize,
        meta: usize,
        kind: Kind,
    },
    TypeOccurs {
        source: Source,
        pos: usize,
        meta: usize,
        ty: syntax::Type<Rc<str>>,
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
        context: Option<SolveConstraintContext>,
    },
    ShadowedModuleName {
        source: Source,
        pos: usize,
    },
    EmptyCompExpr {
        source: Source,
        pos: usize,
    },
    CompExprEndsWithBind {
        source: Source,
        pos: usize,
    },
}

impl TypeError {
    pub fn source(&self) -> Source {
        match self {
            TypeError::KindMismatch { source, .. } => source.clone(),
            TypeError::TypeMismatch { source, .. } => source.clone(),
            TypeError::NotInScope { source, .. } => source.clone(),
            TypeError::NotInModule { source, .. } => source.clone(),
            TypeError::DuplicateArgument { source, .. } => source.clone(),
            TypeError::DuplicateClassArgument { source, .. } => source.clone(),
            TypeError::RedundantPattern { source, .. } => source.clone(),
            TypeError::KindOccurs { source, .. } => source.clone(),
            TypeError::TypeOccurs { source, .. } => source.clone(),
            TypeError::NoSuchClass { source, .. } => source.clone(),
            TypeError::NotAMember { source, .. } => source.clone(),
            TypeError::CannotDeduce { source, .. } => source.clone(),
            TypeError::ShadowedModuleName { source, .. } => source.clone(),
            TypeError::EmptyCompExpr { source, .. } => source.clone(),
            TypeError::CompExprEndsWithBind { source, .. } => source.clone(),
        }
    }

    pub fn position(&self) -> usize {
        match self {
            TypeError::KindMismatch { pos, .. } => *pos,
            TypeError::TypeMismatch { pos, .. } => *pos,
            TypeError::NotInScope { pos, .. } => *pos,
            TypeError::NotInModule { pos, .. } => *pos,
            TypeError::DuplicateArgument { pos, .. } => *pos,
            TypeError::DuplicateClassArgument { pos, .. } => *pos,
            TypeError::RedundantPattern { pos, .. } => *pos,
            TypeError::KindOccurs { pos, .. } => *pos,
            TypeError::TypeOccurs { pos, .. } => *pos,
            TypeError::NoSuchClass { pos, .. } => *pos,
            TypeError::NotAMember { pos, .. } => *pos,
            TypeError::CannotDeduce { context, .. } => match context {
                None => 0,
                Some(context) => context.pos,
            },
            TypeError::ShadowedModuleName { pos, .. } => *pos,
            TypeError::EmptyCompExpr { pos, .. } => *pos,
            TypeError::CompExprEndsWithBind { pos, .. } => *pos,
        }
    }

    pub fn message(&self) -> String {
        match self {
            TypeError::KindMismatch {
                expected, actual, ..
            } => {
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
            TypeError::TypeMismatch { context, .. } => {
                let mut message = String::from("expected type ");
                message.push('"');
                message.push_str(context.expected.render().as_str());
                message.push('"');
                message.push_str(", got type ");
                message.push('"');
                message.push_str(context.actual.render().as_str());
                message.push('"');
                message
            }
            TypeError::NotInScope { .. } => String::from("not in scope"),
            TypeError::NotInModule { .. } => String::from("not in scope"),
            TypeError::DuplicateArgument { .. } => String::from("duplicate argument"),
            TypeError::DuplicateClassArgument { .. } => {
                String::from("duplicate type class argument")
            }
            TypeError::RedundantPattern { .. } => String::from("redundant pattern"),
            TypeError::KindOccurs { meta, kind, .. } => {
                format!(
                    "infinite kind from equating ?{} with \"{}\"",
                    meta,
                    kind.render()
                )
            }
            TypeError::TypeOccurs { meta, ty, .. } => {
                format!(
                    "infinite type from equating ?{} with \"{}\"",
                    meta,
                    ty.render()
                )
            }
            TypeError::NoSuchClass { .. } => String::from("type class not in scope"),
            TypeError::NotAMember { cls, .. } => {
                format!("not a member of the {:?} type class", cls)
            }
            TypeError::CannotDeduce { context, .. } => match context {
                None => String::from("cannot deduce"),
                Some(context) => format!("cannot deduce \"{:}\"", context.constraint.render()),
            },
            TypeError::ShadowedModuleName { .. } => String::from("shadowed module name"),
            TypeError::EmptyCompExpr { .. } => String::from("empty computation expression"),
            TypeError::CompExprEndsWithBind { .. } => {
                String::from("computation expression ends with a bind")
            }
        }
    }

    pub fn addendum(&self) -> Option<String> {
        match self {
            TypeError::KindMismatch { context, .. } => {
                let UnifyKindContext {
                    ty,
                    has_kind,
                    unifying_types,
                } = context;
                {
                    let mut str = String::new();
                    str.push_str(
                        format!(
                            "While checking that \"{}\" has kind \"{}\"",
                            ty.render(),
                            has_kind.render()
                        )
                        .as_str(),
                    );
                    match unifying_types {
                        None => {}
                        Some(context) => str.push_str(
                            format!(
                                "\nWhile unifying \"{}\" with \"{}\"",
                                context.expected.render(),
                                context.actual.render()
                            )
                            .as_str(),
                        ),
                    }
                    Some(str)
                }
            }
            TypeError::DuplicateArgument { .. } => None,
            TypeError::DuplicateClassArgument { .. } => None,
            TypeError::TypeMismatch { .. } => None,
            TypeError::RedundantPattern { .. } => None,
            TypeError::NotInScope { .. } => None,
            TypeError::NotInModule { .. } => None,
            TypeError::KindOccurs { .. } => None,
            TypeError::TypeOccurs { .. } => None,
            TypeError::NoSuchClass { .. } => None,
            TypeError::NotAMember { .. } => None,
            TypeError::CannotDeduce { .. } => None,
            TypeError::ShadowedModuleName { .. } => None,
            TypeError::EmptyCompExpr { .. } => None,
            TypeError::CompExprEndsWithBind { .. } => None,
        }
    }

    pub fn report(&self, diagnostic: &mut diagnostic::Diagnostic) {
        diagnostic.item(
            Some(Location {
                source: self.source(),
                offset: self.position(),
            }),
            Message {
                content: self.message(),
                addendum: self.addendum(),
            },
        )
    }
}

#[macro_export]
macro_rules! with_tc {
    ($path:expr, $location:expr, $f:expr) => {{
        use ipso_core::CommonKinds;
        use std::collections::HashMap;
        let common_kinds = CommonKinds::default();
        let modules = HashMap::new();
        let tc = Typechecker::new_with_builtins($path, $location, &common_kinds, &modules);
        $f(tc)
    }};
}

#[macro_export]
macro_rules! current_dir_with_tc {
    ($f:expr) => {{
        let path = std::env::current_dir().unwrap();
        crate::with_tc!(
            path.as_path(),
            ipso_diagnostic::Source::Interactive {
                label: String::from("(typechecker)")
            },
            $f
        )
    }};
}

impl<'modules> Typechecker<'modules> {
    pub fn new(
        working_dir: &'modules Path,
        source: Source,
        common_kinds: &'modules CommonKinds,
        modules: &'modules HashMap<ModulePath, &'modules core::Module>,
    ) -> Self {
        Typechecker {
            common_kinds,
            source,
            kind_solutions: Vec::new(),
            type_solutions: Vec::new(),
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
            module_unmapping: HashMap::new(),
            working_dir,
        }
    }

    pub fn new_with_builtins(
        working_dir: &'modules Path,
        location: Source,
        common_kinds: &'modules CommonKinds,
        modules: &'modules HashMap<ModulePath, &'modules core::Module>,
    ) -> Self {
        let mut tc = Self::new(working_dir, location, common_kinds, modules);
        tc.register_from_import(&builtins::builtins(tc.common_kinds), &syntax::Names::All);
        tc
    }

    fn eq_zonked_type(&self, t1: &core::Type, t2: &core::Type) -> bool {
        fn zonk_just_enough<'a>(tc: &'a Typechecker, t: &'a core::Type) -> &'a core::Type {
            match t {
                core::Type::Meta(_, n) => match &tc.type_solutions[*n].1 {
                    None => t,
                    Some(sol) => zonk_just_enough(tc, sol),
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
            core::Type::Meta(_, n) => match &self.type_solutions[*n].1 {
                None => match t2 {
                    core::Type::Meta(_, n2) => n == n2,
                    _ => false,
                },
                Some(sol) => self.eq_zonked_type(sol, t2),
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
                Kind::mk_arrow(arg_kind.clone(), acc)
            });
        let decl_ty = core::Type::unsafe_mk_name(decl_name.clone(), decl_name_kind);

        // generate constraint's kind
        let mut constraint_kind = Kind::Constraint;
        for (_, kind) in decl.args.iter().rev() {
            constraint_kind = Kind::mk_arrow(kind.clone(), Kind::Constraint);
        }
        self.type_context.insert(decl_name.clone(), constraint_kind);

        // generate superclass accessors
        let applied_type =
            decl.args
                .iter()
                .enumerate()
                .fold(decl_ty, |acc, (arg_index, (_, arg_kind))| {
                    core::Type::mk_app(acc, core::Type::unsafe_mk_var(arg_index, arg_kind.clone()))
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
        let mut module_mapping = HashMap::new();
        let decls = module.decls.iter().fold(Ok(vec![]), |acc, decl| {
            acc.and_then(|mut decls| {
                self.check_declaration(&mut module_mapping, decl)
                    .map(|m_decl| match m_decl {
                        Option::None => decls,
                        Option::Some(decl) => {
                            self.register_declaration(&decl);
                            decls.push(decl);
                            decls
                        }
                    })
            })
        })?;
        Ok(core::Module {
            module_mapping,
            decls,
        })
    }

    pub fn register_from_import(&mut self, module: &core::Module, names: &syntax::Names) {
        let should_import = |name: &String| -> bool {
            match names {
                syntax::Names::All => true,
                syntax::Names::Names(names) => names.contains(name),
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
                core::Declaration::Class(core::ClassDeclaration {
                    supers,
                    name,
                    args,
                    members,
                }) => todo!("import type class {:?}", (supers, name, args, members)),
                core::Declaration::Instance {
                    ty_vars,
                    superclass_constructors,
                    assumes,
                    head,
                    members,
                } => todo!(
                    "import type class instance {:?}",
                    (ty_vars, superclass_constructors, assumes, head, members)
                ),
            }
        }
    }

    pub fn check_kind(
        &mut self,
        context: Option<&UnifyTypeContextRefs>,
        ty: syntax::Type<usize>,
        kind: &Kind,
    ) -> Result<core::Type, TypeError> {
        let expected = kind;
        let (ty, actual) = self.infer_kind(ty)?;
        let context = UnifyKindContextRefs {
            ty: &ty,
            has_kind: expected,
            unifying_types: context,
        };
        self.unify_kind(&context, expected, &actual)?;
        Ok(ty)
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
                unsolved_constraints.push((*ev, self.zonk_type(&constraint.to_type())));
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

        let mut ty = self.zonk_type(&ty);
        for constraint in unsolved_constraints.into_iter().rev() {
            ty = core::Type::mk_fatarrow(self.common_kinds, constraint, ty);
        }

        let ty_vars: Vec<(Rc<str>, Kind)> = self
            .bound_tyvars
            .info
            .iter()
            .map(|(name, kind)| (name.clone(), self.zonk_kind(true, kind)))
            .collect();
        let sig = core::TypeSig { ty_vars, body: ty };

        Ok((expr, sig))
    }

    fn check_definition(
        &mut self,
        pos: usize,
        name: &str,
        ty: &syntax::Type<Rc<str>>,
        args: &[syntax::Pattern],
        body: &Spanned<syntax::Expr>,
    ) -> Result<core::Declaration, TypeError> {
        let ty_var_positions: HashMap<Rc<str>, usize> = {
            let mut vars = HashMap::new();
            for var in ty.iter_vars() {
                match vars.get(var) {
                    None => {
                        vars.insert(var.clone(), vars.len());
                    }
                    Some(_) => {}
                }
            }
            vars
        };
        let ty_var_kinds_len = ty_var_positions.len();
        let (ty, ty_var_kinds) = {
            let mut kinds = Vec::new();
            let ty = ty.map(&mut |name: &Rc<str>| match ty_var_positions.get(name) {
                None => {
                    panic!("impossible")
                }
                Some(&pos) => {
                    if kinds.len() <= pos {
                        kinds.push((name.clone(), self.fresh_kindvar()));
                    };
                    ty_var_kinds_len - 1 - pos
                }
            });
            (ty, kinds)
        };

        self.bound_tyvars.insert(&ty_var_kinds);

        let ty = self.check_kind(None, ty, &Kind::Type)?;

        let _ = self.context.insert(
            name.to_string(),
            core::TypeSig {
                ty_vars: ty_var_kinds.clone(),
                body: ty.clone(),
            },
        );

        let (constraints, ty) = ty.unwrap_constraints();
        for constraint in constraints {
            self.evidence
                .assume(None, evidence::Constraint::from_type(constraint));
        }

        let body = self.check_expr(
            &syntax::Spanned {
                pos,
                item: syntax::Expr::mk_lam(args.to_vec(), body.clone()),
            },
            ty,
        )?;

        let (body, sig) = self.generalise(body, ty.clone())?;
        self.evidence = Evidence::new();

        self.bound_tyvars.delete(ty_var_kinds_len);

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
        type_: &syntax::Type<Rc<str>>,
    ) -> Result<core::ClassMember, TypeError> {
        let class_args: Vec<Rc<str>> = class_args_kinds.iter().map(|(x, _)| x.clone()).collect();
        let (type_, ty_vars) = type_.abstract_vars(&class_args);

        let class_args_kinds_map: HashMap<Rc<str>, Kind> = class_args_kinds
            .iter()
            .map(|(a, b)| (a.clone(), b.clone()))
            .collect();
        let ty_var_kinds: Vec<(Rc<str>, Kind)> = ty_vars
            .iter()
            .map(|var| {
                (
                    var.clone(),
                    match class_args_kinds_map.get(var) {
                        None => self.fresh_kindvar(),
                        Some(kind) => kind.clone(),
                    },
                )
            })
            .collect();

        self.bound_tyvars.insert(&ty_var_kinds);
        let type_ = self.check_kind(None, type_, &Kind::Type)?;
        self.bound_tyvars.delete(ty_var_kinds.len());

        let ty_vars: Vec<(Rc<str>, Kind)> = ty_var_kinds
            .iter()
            .map(|(a, b)| (a.clone(), self.zonk_kind(true, b)))
            .collect();
        let sig = core::TypeSig {
            ty_vars,
            body: self.zonk_type(&type_),
        };
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
        let args_len = args.len();
        let arg_names: Vec<Rc<str>> = args.iter().map(|x| x.item.clone()).collect();
        let args_kinds = {
            let mut seen: HashSet<Rc<str>> = HashSet::new();
            let mut args_kinds = Vec::with_capacity(args_len);
            for arg in args.iter() {
                if seen.contains(&arg.item) {
                    return Err(TypeError::DuplicateClassArgument {
                        source: self.source(),
                        pos: arg.pos,
                    });
                } else {
                    seen.insert(arg.item.clone());
                }
                args_kinds.push((arg.item.clone(), self.fresh_kindvar()))
            }
            args_kinds
        };

        let mut new_supers = Vec::with_capacity(supers.len());
        for s in supers {
            // abstract over variables
            let (s_item, _) = s.item.abstract_vars(&arg_names);
            with_position!(self, s.pos, {
                self.bound_tyvars.insert(&args_kinds);
                match self.check_kind(None, s_item, &Kind::Constraint) {
                    Err(err) => {
                        return Err(err);
                    }
                    Ok(s_item) => {
                        new_supers.push(s_item);
                        self.bound_tyvars.delete(args_len);
                    }
                }
            })
        }

        let mut checked_members = Vec::with_capacity(members.len());
        for (member_name, member_type) in members {
            match self.check_class_member(&args_kinds, member_name, member_type) {
                Err(err) => return Err(err),
                Ok(checked_member) => {
                    checked_members.push(checked_member);
                }
            }
        }

        Ok(core::Declaration::Class(core::ClassDeclaration {
            supers: new_supers,
            name: name.clone(),
            args: args_kinds
                .into_iter()
                .map(|(name, kind)| (name, self.zonk_kind(true, &kind)))
                .collect::<Vec<(Rc<str>, Kind)>>(),
            members: checked_members,
        }))
    }

    fn check_instance(
        &mut self,
        assumes: &[Spanned<syntax::Type<Rc<str>>>],
        name: &Spanned<Rc<str>>,
        args: &[syntax::Type<Rc<str>>],
        members: &[(Spanned<String>, Vec<syntax::Pattern>, Spanned<syntax::Expr>)],
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

        let (head, ty_vars) = args
            .iter()
            .fold(syntax::Type::Name(name_item), |acc, el| {
                syntax::Type::mk_app(acc, el.clone())
            })
            .abstract_vars(&Vec::new());

        let ty_var_kinds: Vec<(Rc<str>, Kind)> = ty_vars
            .iter()
            .map(|var| (var.clone(), self.fresh_kindvar()))
            .collect();

        self.bound_tyvars.insert(&ty_var_kinds);

        let (_, args) = head.unwrap_app();
        let args: Vec<core::Type> = args
            .into_iter()
            .map(|arg| {
                let res = self.infer_kind(arg.clone())?;
                Ok(res.0)
            })
            .collect::<Result<_, TypeError>>()?;

        let assumes: Vec<core::Type> = assumes
            .iter()
            .map(|assume| {
                let assume = assume.item.abstract_vars(&ty_vars).0;
                self.check_kind(None, assume, &Kind::Constraint)
            })
            .collect::<Result<_, TypeError>>()?;

        // generate evidence for assumptions
        assumes.iter().for_each(|constraint| {
            let _ = self
                .evidence
                .assume(None, evidence::Constraint::from_type(constraint));
        });

        // locate evidence for superclasses
        let superclass_constructors: Vec<core::Expr> = {
            let mut superclass_constructors = Vec::new();

            for superclass in &class_decl.supers {
                let superclass = superclass.instantiate_many(&args);

                match evidence::solver::solve_constraint(
                    &Some(SolveConstraintContext {
                        pos: name.pos,
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

        let head = with_position!(
            self,
            name.pos,
            self.check_kind(None, head, &Kind::Constraint)
        )?;

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
                        .check_expr(
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
            .map(|(a, b)| (a, self.zonk_kind(true, &b)))
            .collect();

        Ok(core::Declaration::Instance {
            ty_vars,
            superclass_constructors,
            assumes: assumes
                .iter()
                .map(|assume| self.zonk_type(assume))
                .collect(),
            head: self.zonk_type(&head),
            members: new_members,
        })
    }

    fn check_import(
        &mut self,
        module_mapping: &mut HashMap<ModulePath, core::ModuleUsage>,
        module: &Spanned<String>,
        name: &Option<Spanned<String>>,
    ) -> Result<(), TypeError> {
        let path =
            ModulePath::from_module(self.working_dir, &ModuleName(vec![module.item.clone()]));

        let actual_name = match name {
            None => module,
            Some(name) => name,
        };

        match module_mapping.get(&path) {
            None => {
                let signatures = self
                    .modules
                    .get(&path)
                    .unwrap()
                    .get_signatures(self.common_kinds);
                self.module_context.insert(path.clone(), signatures);
                self.module_unmapping
                    .insert(ModuleName(vec![actual_name.item.clone()]), path.clone());
                module_mapping.insert(path, core::ModuleUsage::Named(actual_name.item.clone()));
                Ok(())
            }
            Some(_) => Err(TypeError::ShadowedModuleName {
                source: self.source(),
                pos: actual_name.pos,
            }),
        }
    }

    fn check_declaration(
        &mut self,
        module_mapping: &mut HashMap<ModulePath, core::ModuleUsage>,
        decl: &syntax::Spanned<syntax::Declaration>,
    ) -> Result<Option<core::Declaration>, TypeError> {
        match &decl.item {
            syntax::Declaration::Definition {
                name,
                ty,
                args,
                body,
            } => self
                .check_definition(decl.pos, name, ty, args, body)
                .map(Option::Some),
            syntax::Declaration::TypeAlias { name, args, body } => {
                todo!("check type alias {:?}", (name, args, body))
            }
            syntax::Declaration::Import { module, name } => self
                .check_import(module_mapping, module, name)
                .map(|()| Option::None),
            syntax::Declaration::FromImport { module, names } => {
                todo!("check from-import {:?}", (module, names))
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

    fn lookup_var(&self, name: &str) -> Option<(usize, core::Type)> {
        self.bound_vars
            .lookup_name(name)
            .map(|(ix, ty)| (ix, ty.clone()))
    }

    fn lookup_name(&self, name: &str) -> Option<core::TypeSig> {
        self.context.get(name).cloned()
    }

    pub fn zonk_constraint(&self, constraint: &Constraint) -> Constraint {
        match constraint {
            Constraint::HasField { field, rest } => Constraint::HasField {
                field: field.clone(),
                rest: self.zonk_type(rest),
            },
            Constraint::Type(ty) => Constraint::Type(self.zonk_type(ty)),
        }
    }

    pub fn zonk_type(&self, ty: &core::Type) -> core::Type {
        match ty {
            core::Type::Name(k, n) => core::Type::Name(self.zonk_kind(false, k), n.clone()),
            core::Type::Var(k, n) => core::Type::Var(self.zonk_kind(false, k), *n),
            core::Type::Bool => core::Type::Bool,
            core::Type::Int => core::Type::Int,
            core::Type::Char => core::Type::Char,
            core::Type::String => core::Type::String,
            core::Type::Bytes => core::Type::Bytes,
            core::Type::Constraints(cs) => {
                core::Type::Constraints(cs.iter().map(|c| self.zonk_type(c)).collect())
            }
            core::Type::App(k, a, b) => core::Type::App(
                self.zonk_kind(false, k),
                Rc::new(self.zonk_type(a)),
                Rc::new(self.zonk_type(b)),
            ),
            core::Type::RowNil => core::Type::RowNil,
            core::Type::Unit => core::Type::Unit,
            core::Type::RowCons(field, ty, rest) => {
                core::Type::mk_rowcons(field.clone(), self.zonk_type(ty), self.zonk_type(rest))
            }
            core::Type::HasField(field, rest) => {
                core::Type::mk_hasfield(field.clone(), self.zonk_type(rest))
            }
            core::Type::Meta(k, n) => match &self.type_solutions[*n].1 {
                None => core::Type::Meta(self.zonk_kind(false, k), *n),
                Some(ty) => self.zonk_type(ty),
            },
            // These types have known kinds that don't need to be zonked
            core::Type::Arrow(k) => core::Type::Arrow(k.clone()),
            core::Type::FatArrow(k) => core::Type::FatArrow(k.clone()),
            core::Type::Array(k) => core::Type::Array(k.clone()),
            core::Type::Record(k) => core::Type::Record(k.clone()),
            core::Type::Variant(k) => core::Type::Variant(k.clone()),
            core::Type::IO(k) => core::Type::IO(k.clone()),
        }
    }

    pub fn zonk_kind(&self, close_unsolved: bool, kind: &Kind) -> Kind {
        match kind {
            Kind::Ref(kind) => match kind.as_ref() {
                KindCompound::Arrow(a, b) => Kind::Ref(Rc::new(KindCompound::mk_arrow(
                    self.zonk_kind(close_unsolved, a),
                    self.zonk_kind(close_unsolved, b),
                ))),
            },
            Kind::Type => Kind::Type,
            Kind::Row => Kind::Row,
            Kind::Constraint => Kind::Constraint,
            Kind::Meta(m) => match &self.kind_solutions[*m] {
                None => {
                    if close_unsolved {
                        Kind::Type
                    } else {
                        Kind::Meta(*m)
                    }
                }
                Some(kind) => self.zonk_kind(close_unsolved, kind),
            },
        }
    }

    fn fresh_kindvar(&mut self) -> Kind {
        let n = self.kind_solutions.len();
        self.kind_solutions.push(None);
        Kind::Meta(n)
    }

    pub fn fill_ty_names(&self, ty: syntax::Type<usize>) -> syntax::Type<Rc<str>> {
        ty.map(&mut |&ix| self.bound_tyvars.lookup_index(ix).unwrap().0.clone())
    }

    fn kind_mismatch<A>(
        &self,
        context: &UnifyKindContextRefs,
        expected: &Kind,
        actual: &Kind,
    ) -> Result<A, TypeError> {
        let context = UnifyKindContext {
            ty: self.fill_ty_names(self.zonk_type(context.ty).to_syntax()),
            has_kind: self.zonk_kind(false, context.has_kind),
            unifying_types: context.unifying_types.map(|x| UnifyTypeContext {
                expected: self.fill_ty_names(self.zonk_type(x.expected).to_syntax()),
                actual: self.fill_ty_names(self.zonk_type(x.actual).to_syntax()),
            }),
        };
        Err(TypeError::KindMismatch {
            source: self.source(),
            pos: self.current_position(),
            context,
            expected: expected.clone(),
            actual: actual.clone(),
        })
    }

    fn unify_kind(
        &mut self,
        context: &UnifyKindContextRefs,
        expected: &Kind,
        actual: &Kind,
    ) -> Result<(), TypeError> {
        match expected {
            Kind::Ref(expected) => match expected.as_ref() {
                KindCompound::Arrow(expected_a, expected_b) => match actual {
                    Kind::Ref(actual) => match actual.as_ref() {
                        KindCompound::Arrow(actual_a, actual_b) => {
                            self.unify_kind(context, expected_a, actual_a)?;
                            self.unify_kind(context, expected_b, actual_b)
                        }
                    },
                    Kind::Meta(m) => {
                        self.solve_kindvar_right(context, &Kind::Ref(expected.clone()), *m)
                    }
                    _ => self.kind_mismatch(context, &Kind::Ref(expected.clone()), actual),
                },
            },
            Kind::Type => match actual {
                Kind::Type => Ok(()),
                Kind::Meta(m) => self.solve_kindvar_right(context, expected, *m),
                _ => self.kind_mismatch(context, expected, actual),
            },
            Kind::Row => match actual {
                Kind::Row => Ok(()),
                Kind::Meta(m) => self.solve_kindvar_right(context, expected, *m),
                _ => self.kind_mismatch(context, expected, actual),
            },
            Kind::Constraint => match actual {
                Kind::Constraint => Ok(()),
                Kind::Meta(m) => self.solve_kindvar_right(context, expected, *m),
                _ => self.kind_mismatch(context, expected, actual),
            },
            Kind::Meta(expected_m) => match actual {
                Kind::Meta(actual_m) if expected_m == actual_m => Ok(()),
                actual => self.solve_kindvar_left(context, *expected_m, actual),
            },
        }
    }

    fn occurs_kind(&self, meta: usize, kind: &Kind) -> Result<(), TypeError> {
        match kind.iter_metas().find(|&other| {
            meta == other
                || match &self.kind_solutions[other] {
                    None => false,
                    Some(kind) => self.occurs_kind(meta, kind).is_err(),
                }
        }) {
            None => Ok(()),
            Some(_) => Err(TypeError::KindOccurs {
                source: self.source(),
                pos: self.current_position(),
                meta,
                kind: self.zonk_kind(false, kind),
            }),
        }
    }

    fn solve_kindvar_right(
        &mut self,
        context: &UnifyKindContextRefs,
        expected: &Kind,
        meta: usize,
    ) -> Result<(), TypeError> {
        match self.kind_solutions[meta].clone() {
            None => {
                let _ = self.occurs_kind(meta, expected)?;
                self.kind_solutions[meta] = Some(expected.clone());
                Ok(())
            }
            Some(actual) => self.unify_kind(context, expected, &actual),
        }
    }

    fn solve_kindvar_left(
        &mut self,
        context: &UnifyKindContextRefs,
        meta: usize,
        actual: &Kind,
    ) -> Result<(), TypeError> {
        match self.kind_solutions[meta].clone() {
            None => {
                let _ = self.occurs_kind(meta, actual)?;
                self.kind_solutions[meta] = Some(actual.clone());
                Ok(())
            }
            Some(expected) => self.unify_kind(context, &expected, actual),
        }
    }

    fn not_in_scope<A>(&self, name: &str) -> Result<A, TypeError> {
        Err(TypeError::NotInScope {
            source: self.source(),
            pos: self.current_position(),
            name: name.to_string(),
        })
    }

    fn type_mismatch<A>(
        &self,
        context: &UnifyTypeContextRefs,
        expected: core::Type,
        actual: core::Type,
    ) -> Result<A, TypeError> {
        let context = UnifyTypeContext {
            expected: self.fill_ty_names(self.zonk_type(context.expected).to_syntax()),
            actual: self.fill_ty_names(self.zonk_type(context.actual).to_syntax()),
        };
        Err(TypeError::TypeMismatch {
            source: self.source(),
            pos: self.current_position(),
            context,
            expected: self.fill_ty_names(expected.to_syntax()),
            actual: self.fill_ty_names(actual.to_syntax()),
        })
    }

    fn occurs_type(&self, meta: usize, ty: &core::Type) -> Result<(), TypeError> {
        debug_assert!(
            match ty {
                core::Type::Meta(_, n) => self.type_solutions[*n].1 == None,
                _ => true,
            },
            "ty is a meta with a solution"
        );

        fn metas_set(tc: &Typechecker, ty: &core::Type, set: &mut FnvHashSet<usize>) {
            for meta in ty.iter_metas() {
                match &tc.type_solutions[meta].1 {
                    None => {
                        set.insert(meta);
                    }
                    Some(ty) => metas_set(tc, ty, set),
                }
            }
        }

        let set = {
            let mut set = FnvHashSet::with_hasher(Default::default());
            metas_set(self, ty, &mut set);
            set
        };

        if set.contains(&meta) {
            Err(TypeError::TypeOccurs {
                source: self.source(),
                pos: self.current_position(),
                meta,
                ty: self.fill_ty_names(self.zonk_type(ty).to_syntax()),
            })
        } else {
            Ok(())
        }
    }

    pub fn solve_typevar_right(
        &mut self,
        context: &UnifyTypeContextRefs,
        expected: &core::Type,
        meta: usize,
    ) -> Result<(), TypeError> {
        match &self.type_solutions[meta].1.clone() {
            None => {
                self.type_solutions[meta].1 = Some(expected.clone());
                Ok(())
            }
            Some(actual) => self.unify_type(context, expected, actual),
        }
    }

    pub fn solve_typevar_left(
        &mut self,
        context: &UnifyTypeContextRefs,
        meta: usize,
        actual: &core::Type,
    ) -> Result<(), TypeError> {
        match &self.type_solutions[meta].1.clone() {
            None => {
                self.type_solutions[meta].1 = Some(actual.clone());
                Ok(())
            }
            Some(expected) => self.unify_type(context, expected, actual),
        }
    }

    fn lookup_typevar(&self, n: usize) -> Result<Kind, TypeError> {
        match self.type_solutions.get(n) {
            None => panic!("missing kind for type var: ?{}", n),
            Some((k, _)) => Ok(k.clone()),
        }
    }

    fn infer_kind(&mut self, ty: syntax::Type<usize>) -> Result<(core::Type, Kind), TypeError> {
        match ty {
            syntax::Type::Name(n) => match self.type_context.get(n.as_ref()) {
                None => self.not_in_scope(n.as_ref()),
                Some(kind) => Ok((core::Type::Name(kind.clone(), n), kind.clone())),
            },
            syntax::Type::Var(ix) => match self.bound_tyvars.lookup_index(ix) {
                None => {
                    panic!("missing tyvar {:?}", ix);
                }
                Some((_, kind)) => Ok((core::Type::Var(kind.clone(), ix), kind.clone())),
            },
            syntax::Type::Bool => Ok((core::Type::Bool, Kind::Type)),
            syntax::Type::Int => Ok((core::Type::Int, Kind::Type)),
            syntax::Type::Char => Ok((core::Type::Char, Kind::Type)),
            syntax::Type::String => Ok((core::Type::String, Kind::Type)),
            syntax::Type::Bytes => Ok((core::Type::Bytes, Kind::Type)),
            syntax::Type::Arrow => Ok((
                core::Type::mk_arrow_ctor(self.common_kinds),
                self.common_kinds.type_to_type_to_type.clone(),
            )),
            syntax::Type::FatArrow => Ok((
                core::Type::mk_fatarrow_ctor(self.common_kinds),
                self.common_kinds.constraint_to_type_to_type.clone(),
            )),
            syntax::Type::Constraints(constraints) => {
                let constraints = constraints
                    .into_iter()
                    .map(|constraint| self.check_kind(None, constraint, &Kind::Constraint))
                    .collect::<Result<_, TypeError>>()?;
                Ok((core::Type::Constraints(constraints), Kind::Constraint))
            }
            syntax::Type::Array => Ok((
                core::Type::mk_array(self.common_kinds),
                self.common_kinds.type_to_type.clone(),
            )),
            syntax::Type::Record => Ok((
                core::Type::mk_record_ctor(self.common_kinds),
                self.common_kinds.row_to_type.clone(),
            )),
            syntax::Type::Variant => Ok((
                core::Type::mk_variant_ctor(self.common_kinds),
                self.common_kinds.row_to_type.clone(),
            )),
            syntax::Type::IO => Ok((
                core::Type::mk_io(self.common_kinds),
                self.common_kinds.type_to_type.clone(),
            )),
            syntax::Type::App(a, b) => {
                let in_kind = self.fresh_kindvar();
                let out_kind = self.fresh_kindvar();
                let a = self.check_kind(
                    None,
                    a.as_ref().clone(),
                    &Kind::Ref(Rc::new(KindCompound::mk_arrow(
                        in_kind.clone(),
                        out_kind.clone(),
                    ))),
                )?;
                let b = self.check_kind(None, b.as_ref().clone(), &in_kind)?;
                Ok((
                    core::Type::App(out_kind.clone(), Rc::new(a), Rc::new(b)),
                    out_kind,
                ))
            }
            syntax::Type::RowNil => Ok((core::Type::RowNil, Kind::Row)),
            syntax::Type::RowCons(field, ty, rest) => {
                let ty = self.check_kind(None, ty.as_ref().clone(), &Kind::Type)?;
                let rest = self.check_kind(None, rest.as_ref().clone(), &Kind::Row)?;
                Ok((core::Type::mk_rowcons(field, ty, rest), Kind::Row))
            }
            syntax::Type::HasField(field, rest) => {
                let rest = self.check_kind(None, rest.as_ref().clone(), &Kind::Row)?;
                Ok((core::Type::mk_hasfield(field, rest), Kind::Constraint))
            }
            syntax::Type::Unit => Ok((core::Type::Unit, Kind::Type)),
            syntax::Type::Meta(n) => {
                let kind = self.lookup_typevar(n)?;
                Ok((core::Type::Meta(kind.clone(), n), kind))
            }
        }
    }

    pub fn fresh_typevar(&mut self, kind: Kind) -> core::Type {
        let n = self.type_solutions.len();
        self.type_solutions.push((kind.clone(), None));
        core::Type::Meta(kind, n)
    }

    fn walk(&self, ty: &core::Type) -> core::Type {
        match ty {
            core::Type::Meta(_, n) => match &self.type_solutions[*n].1 {
                None => ty.clone(),
                Some(ty) => self.walk(ty),
            },
            _ => ty.clone(),
        }
    }

    pub fn unify_type_subst(
        &mut self,
        subst: &mut Substitution,
        context: &UnifyTypeContextRefs,
        expected: &core::Type,
        actual: &core::Type,
    ) -> Result<(), TypeError> {
        let expected = self.walk(expected);
        let actual = self.walk(actual);

        let expected_kind = expected.kind();
        let actual_kind = actual.kind();
        self.unify_kind(
            &UnifyKindContextRefs {
                ty: &actual,
                has_kind: &expected_kind,
                unifying_types: Some(context),
            },
            &expected_kind,
            &actual_kind,
        )?;

        match &expected {
            core::Type::App(_, a1, b1) => match actual {
                core::Type::App(_, a2, b2) => {
                    self.unify_type_subst(subst, context, a1, &a2)?;
                    self.unify_type_subst(subst, context, b1, &b2)?;
                    Ok(())
                }
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                actual => self.type_mismatch(context, expected, actual),
            },
            core::Type::Name(_, n) => match actual {
                core::Type::Name(_, nn) if *n == nn => Ok(()),
                core::Type::Meta(_, nn) => subst.subst_right(self, context, expected, nn),
                actual => self.type_mismatch(context, expected, actual),
            },
            core::Type::Var(_, n) => match actual {
                core::Type::Var(_, nn) if *n == nn => Ok(()),
                core::Type::Meta(_, nn) => subst.subst_right(self, context, expected, nn),
                actual => self.type_mismatch(context, expected, actual),
            },
            core::Type::Bool => match actual {
                core::Type::Bool => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::Int => match actual {
                core::Type::Int => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::Char => match actual {
                core::Type::Char => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::String => match actual {
                core::Type::String => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::Bytes => match actual {
                core::Type::Bytes => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::Array(_) => match actual {
                core::Type::Array(_) => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::Arrow(_) => match actual {
                core::Type::Arrow(_) => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::FatArrow(_) => match actual {
                core::Type::FatArrow(_) => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::Constraints(constraints1) => match actual {
                core::Type::Constraints(constraints2) => {
                    for (c1, c2) in constraints1.iter().zip(constraints2.into_iter()) {
                        if let Err(err) = self.unify_type_subst(subst, context, c1, &c2) {
                            return Err(err);
                        }
                    }
                    Ok(())
                }
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                actual => self.type_mismatch(context, expected, actual),
            },
            core::Type::Record(_) => match actual {
                core::Type::Record(_) => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::Variant(_) => match actual {
                core::Type::Variant(_) => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::IO(_) => match actual {
                core::Type::IO(_) => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::RowNil => match actual {
                core::Type::RowNil => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::RowCons(_, _, _) => match actual {
                core::Type::RowCons(_, _, _) => {
                    let row_parts_1 = expected.unwrap_rows();
                    let row_parts_2 = actual.unwrap_rows();

                    let mut rows2_remaining = Rope::from_vec(&row_parts_2.fields);

                    let mut sames: Vec<(&Rc<str>, &core::Type, &core::Type)> = Vec::new();
                    let mut not_in_rows2: Vec<(Rc<str>, core::Type)> = Vec::new();

                    for (field1, ty1) in row_parts_1.fields {
                        match rows2_remaining.iter().find(|(field2, _)| field1 == *field2) {
                            None => {
                                not_in_rows2.push((field1.clone(), ty1.clone()));
                            }
                            Some((_, ty2)) => {
                                rows2_remaining =
                                    match rows2_remaining.delete_first(|(f, _)| *f == field1) {
                                        Err(new) => new,
                                        Ok(new) => new,
                                    };
                                sames.push((field1, ty1, ty2));
                            }
                        }
                    }

                    // every field in rows1 that has a partner in rows2 has been deleted from rows2
                    // therefore whatever's left in rows2_remaining is necessarily not in rows_1
                    let not_in_rows1: Vec<(Rc<str>, core::Type)> = rows2_remaining
                        .iter()
                        .map(|(a, b)| ((*a).clone(), (*b).clone()))
                        .collect();

                    // now we're working with: sames, not_in_rows1, not_in_rows2
                    //
                    // unify sames
                    for (_field, ty1, ty2) in sames {
                        match self.unify_type_subst(subst, context, ty1, ty2) {
                            Err(err) => return Err(err),
                            Ok(()) => {}
                        }
                    }

                    let rest3 = Some(self.fresh_typevar(Kind::Row));
                    self.unify_type_subst(
                        subst,
                        context,
                        match row_parts_1.rest {
                            None => &core::Type::RowNil,
                            Some(ty) => ty,
                        },
                        &core::Type::mk_rows(not_in_rows1, rest3.clone()),
                    )?;
                    self.unify_type_subst(
                        subst,
                        context,
                        &core::Type::mk_rows(not_in_rows2, rest3),
                        match row_parts_2.rest {
                            None => &core::Type::RowNil,
                            Some(ty) => ty,
                        },
                    )?;

                    Ok(())
                }
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                actual => self.type_mismatch(context, expected, actual),
            },
            core::Type::HasField(field, rest) => match actual {
                core::Type::HasField(field2, rest2) if *field == field2 => {
                    self.unify_type_subst(subst, context, rest, &rest2)
                }
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::Unit => match actual {
                core::Type::Unit => Ok(()),
                core::Type::Meta(_, n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            core::Type::Meta(_, n) => match actual {
                core::Type::Meta(_, nn) if *n == nn => Ok(()),
                _ => subst.subst_left(self, context, *n, actual),
            },
        }
    }

    pub fn commit_substitutions(&mut self, subst: Substitution) {
        for (var, ty) in subst.into_hashmap().into_iter() {
            debug_assert!(self.type_solutions[var].1 == None);
            self.type_solutions[var].1 = Some(ty);
        }
    }

    pub fn unify_type(
        &mut self,
        context: &UnifyTypeContextRefs,
        expected: &core::Type,
        actual: &core::Type,
    ) -> Result<(), TypeError> {
        let mut subst = Substitution::new();
        self.unify_type_subst(&mut subst, context, expected, actual)?;
        self.commit_substitutions(subst);
        Ok(())
    }

    fn check_duplicate_args(&self, args: &[syntax::Pattern]) -> Result<(), TypeError> {
        let mut arg_names_spanned: Vec<&Spanned<String>> = Vec::new();
        for arg in args {
            arg_names_spanned.extend(arg.get_arg_names());
        }
        let mut seen: HashSet<&String> = HashSet::new();
        for arg in arg_names_spanned {
            if seen.contains(&arg.item) {
                return Err(TypeError::DuplicateArgument {
                    source: self.source(),
                    pos: arg.pos,
                    name: arg.item.clone(),
                });
            } else {
                seen.insert(&arg.item);
            }
        }
        Ok(())
    }

    fn infer_variant_pattern(
        &mut self,
        name: &Rc<str>,
        arg: &Spanned<String>,
    ) -> (core::Pattern, core::Type, Vec<(Rc<str>, core::Type)>) {
        let arg_ty = self.fresh_typevar(Kind::Type);
        let rest_row = self.fresh_typevar(Kind::Row);
        let ty = core::Type::mk_variant(
            self.common_kinds,
            vec![(name.clone(), arg_ty.clone())],
            Some(rest_row.clone()),
        );

        let tag = self.evidence.placeholder(
            Some(self.current_position()),
            evidence::Constraint::HasField {
                field: name.clone(),
                rest: rest_row,
            },
        );
        (
            core::Pattern::mk_variant(core::Expr::Placeholder(tag)),
            ty,
            vec![(Rc::from(arg.item.as_ref()), arg_ty)],
        )
    }

    fn infer_wildcard_pattern(
        &mut self,
    ) -> (core::Pattern, core::Type, Vec<(Rc<str>, core::Type)>) {
        (
            core::Pattern::Wildcard,
            self.fresh_typevar(Kind::Type),
            Vec::new(),
        )
    }

    fn infer_name_pattern(
        &mut self,
        name: &Spanned<String>,
    ) -> (core::Pattern, core::Type, Vec<(Rc<str>, core::Type)>) {
        let ty = self.fresh_typevar(Kind::Type);
        (
            core::Pattern::Name,
            ty.clone(),
            vec![(Rc::from(name.item.as_ref()), ty)],
        )
    }

    fn infer_record_pattern(
        &mut self,
        names: &[Spanned<String>],
        rest: &Option<Spanned<String>>,
    ) -> (core::Pattern, core::Type, Vec<(Rc<str>, core::Type)>) {
        let mut names_tys: Vec<(Spanned<Rc<str>>, core::Type)> = names
            .iter()
            .map(|name| {
                let name = Spanned {
                    pos: name.pos,
                    item: Rc::from(name.item.as_ref()),
                };
                (name, self.fresh_typevar(Kind::Type))
            })
            .collect();
        let rest_row: Option<(Spanned<String>, core::Type)> = rest
            .as_ref()
            .map(|name| (name.clone(), self.fresh_typevar(Kind::Row)));
        let ty = core::Type::mk_record(
            self.common_kinds,
            names_tys
                .iter()
                .map(|(name, ty)| (name.item.clone(), ty.clone()))
                .collect(),
            rest_row.as_ref().map(|x| x.1.clone()),
        );

        let mut extending_row = match &rest_row {
            Some((_, row)) => row.clone(),
            None => core::Type::RowNil,
        };
        let mut names_placeholders = Vec::new();
        for (name, ty) in names_tys.iter().rev() {
            let p = self.evidence.placeholder(
                Some(name.pos),
                Constraint::HasField {
                    field: name.item.clone(),
                    rest: extending_row.clone(),
                },
            );
            names_placeholders.push(core::Expr::Placeholder(p));
            extending_row = core::Type::mk_rowcons(name.item.clone(), ty.clone(), extending_row);
        }
        names_placeholders.reverse();

        rest_row.iter().for_each(|(rest_name, rest_row)| {
            names_tys.push((
                Spanned {
                    pos: rest_name.pos,
                    item: Rc::from(rest_name.item.as_ref()),
                },
                core::Type::mk_record(self.common_kinds, Vec::new(), Some(rest_row.clone())),
            ))
        });

        (
            core::Pattern::Record {
                names: names_placeholders,
                rest: rest.is_some(),
            },
            ty,
            names_tys.into_iter().map(|(a, b)| (a.item, b)).collect(),
        )
    }

    fn infer_pattern(
        &mut self,
        arg: &syntax::Pattern,
    ) -> (core::Pattern, core::Type, Vec<(Rc<str>, core::Type)>) {
        match arg {
            syntax::Pattern::Wildcard => self.infer_wildcard_pattern(),
            syntax::Pattern::Name(n) => self.infer_name_pattern(n),
            syntax::Pattern::Record { names, rest } => self.infer_record_pattern(names, rest),
            syntax::Pattern::Variant { name, arg } => {
                self.infer_variant_pattern(&Rc::from(name.as_ref()), arg)
            }
        }
    }

    #[allow(clippy::type_complexity)]
    fn infer_variant_pattern_case(
        &mut self,
        m_expr_rows: &mut Option<core::Type>,
        expected_pattern_ty: &mut core::Type,
        seen_ctors: &mut HashSet<Rc<str>>,
        name: &str,
        arg: &Spanned<String>,
    ) -> Result<(core::Pattern, core::Type, Vec<(Rc<str>, core::Type)>), TypeError> {
        let name: Rc<str> = Rc::from(name);

        let arg_ty = self.fresh_typevar(Kind::Type);
        let rest_row = self.fresh_typevar(Kind::Row);
        let pattern_rows =
            core::Type::mk_rows(vec![(name.clone(), arg_ty.clone())], Some(rest_row.clone()));
        let expr_rows: core::Type = match &m_expr_rows {
            None => {
                *m_expr_rows = Some(pattern_rows.clone());
                pattern_rows.clone()
            }
            Some(rows) => rows.clone(),
        };

        let tag = core::Expr::Placeholder(self.evidence.placeholder(
            None,
            evidence::Constraint::HasField {
                field: name.clone(),
                rest: expr_rows,
            },
        ));

        let pattern_ty =
            core::Type::mk_app(core::Type::mk_variant_ctor(self.common_kinds), pattern_rows);
        let context: UnifyTypeContextRefs = UnifyTypeContextRefs {
            expected: expected_pattern_ty,
            actual: &pattern_ty,
        };
        match self.unify_type(&context, expected_pattern_ty, &pattern_ty) {
            Err(err) => {
                return Err(err);
            }
            Ok(()) => {}
        }
        *expected_pattern_ty =
            core::Type::mk_variant(self.common_kinds, Vec::new(), Some(rest_row));

        seen_ctors.insert(name);

        let pattern_core = core::Pattern::mk_variant(tag);
        let pattern_binds: Vec<(Rc<str>, core::Type)> = vec![(Rc::from(arg.item.as_ref()), arg_ty)];
        Ok((pattern_core, pattern_ty, pattern_binds))
    }

    #[allow(clippy::type_complexity)]
    fn check_pattern(
        &mut self,
        arg: &syntax::Pattern,
        expected: &core::Type,
    ) -> Result<(core::Pattern, Vec<(Rc<str>, core::Type)>), TypeError> {
        let (pat, actual, binds) = self.infer_pattern(arg);
        let context = UnifyTypeContextRefs {
            expected,
            actual: &actual,
        };
        self.unify_type(&context, expected, &actual)?;
        Ok((pat, binds))
    }

    fn check_string_part(
        &mut self,
        part: &syntax::StringPart,
    ) -> Result<core::StringPart, TypeError> {
        match part {
            syntax::StringPart::String(s) => Ok(core::StringPart::String(s.clone())),
            syntax::StringPart::Expr(e) => {
                let e_core = self.check_expr(e, &core::Type::String)?;
                Ok(core::StringPart::Expr(e_core))
            }
        }
    }

    fn instantiate(&mut self, expr: core::Expr, sig: core::TypeSig) -> (core::Expr, core::Type) {
        let metas: Vec<core::Type> = sig
            .ty_vars
            .into_iter()
            .map(|(_, kind)| self.fresh_typevar(kind))
            .collect();
        let ty = sig.body.subst(&|&ix| metas[metas.len() - 1 - ix].clone());
        let (constraints, ty) = ty.unwrap_constraints();
        let mut expr = expr;
        for constraint in constraints {
            let p = self.evidence.placeholder(
                Some(self.current_position()),
                evidence::Constraint::from_type(constraint),
            );
            expr = core::Expr::mk_app(expr, core::Expr::Placeholder(p));
        }
        (expr, ty.clone())
    }

    fn infer_expr(
        &mut self,
        expr: &syntax::Spanned<syntax::Expr>,
    ) -> Result<(core::Expr, core::Type), TypeError> {
        with_position!(
            self,
            expr.pos,
            match &expr.item {
                syntax::Expr::Var(name) => {
                    match self.lookup_var(name) {
                        Some(entry) => Ok((core::Expr::Var(entry.0), entry.1)),
                        None => match self.lookup_name(name) {
                            Some(sig) => {
                                let (expr, ty) =
                                    self.instantiate(core::Expr::Name(name.clone()), sig);
                                Ok((expr, ty))
                            }
                            None => self.not_in_scope(name),
                        },
                    }
                }
                syntax::Expr::Module { name, item } => {
                    let sig = match self
                        .module_context
                        .get(self.module_unmapping.get(name).unwrap())
                    {
                        None => {
                            // a module accessor will only be desugared if the module was in scope,
                            // so we should never get here during a program run
                            panic!("module not in scope: {:?}", name)
                        }
                        Some(defs) => match defs.get(item) {
                            None => Err(TypeError::NotInModule {
                                source: self.source(),
                                pos: self.current_position(),
                                item: item.clone(),
                            }),
                            Some(sig) => Ok(sig.clone()),
                        },
                    }?;
                    Ok(self.instantiate(core::Expr::Module(name.clone(), item.clone()), sig))
                }
                syntax::Expr::App(f, x) => {
                    let (f_core, f_ty) = self.infer_expr(f)?;
                    let in_ty = self.fresh_typevar(Kind::Type);
                    let out_ty = self.fresh_typevar(Kind::Type);
                    let expected =
                        core::Type::mk_arrow(self.common_kinds, in_ty.clone(), out_ty.clone());
                    let actual = f_ty;
                    let context = UnifyTypeContextRefs {
                        expected: &expected,
                        actual: &actual,
                    };
                    self.unify_type(&context, &expected, &actual)?;
                    let x_core = self.check_expr(x, &in_ty)?;
                    Ok((core::Expr::mk_app(f_core, x_core), out_ty))
                }
                syntax::Expr::Lam { args, body } => {
                    let in_ty = self.fresh_typevar(Kind::Type);
                    let out_ty = self.fresh_typevar(Kind::Type);
                    let ty = core::Type::mk_arrow(self.common_kinds, in_ty, out_ty);
                    let expr_core = self.check_expr(
                        &Spanned {
                            pos: expr.pos,
                            item: syntax::Expr::Lam {
                                args: args.clone(),
                                body: body.clone(),
                            },
                        },
                        &ty,
                    )?;
                    Ok((expr_core, ty))
                }
                syntax::Expr::Let { name, value, rest } => {
                    let (value_core, value_ty) = self.infer_expr(value)?;

                    self.bound_vars.insert(&[(name.clone(), value_ty)]);
                    let (rest_core, rest_ty) = self.infer_expr(rest)?;
                    self.bound_vars.delete(1);

                    Ok((
                        core::Expr::Let {
                            value: Rc::new(value_core),
                            rest: Rc::new(rest_core),
                        },
                        rest_ty,
                    ))
                }
                syntax::Expr::True => Ok((core::Expr::True, core::Type::Bool)),
                syntax::Expr::False => Ok((core::Expr::False, core::Type::Bool)),
                syntax::Expr::IfThenElse(cond, then_, else_) => {
                    let cond_core = self.check_expr(cond, &core::Type::Bool)?;
                    let (then_core, then_ty) = self.infer_expr(then_)?;
                    let else_core = self.check_expr(else_, &then_ty)?;
                    Ok((
                        core::Expr::mk_ifthenelse(cond_core, then_core, else_core),
                        then_ty,
                    ))
                }
                syntax::Expr::Unit => Ok((core::Expr::Unit, core::Type::Unit)),
                syntax::Expr::Int(n) => Ok((core::Expr::Int(*n), core::Type::Int)),
                syntax::Expr::Char(c) => Ok((core::Expr::Char(*c), core::Type::Char)),
                syntax::Expr::String(parts) => {
                    let mut parts_core = Vec::new();
                    for part in parts {
                        match self.check_string_part(part) {
                            Err(err) => return Err(err),
                            Ok(part_core) => parts_core.push(part_core),
                        }
                    }
                    Ok((core::Expr::String(parts_core), core::Type::String))
                }
                syntax::Expr::Array(items) => {
                    let mut items_iter = items.iter();
                    match items_iter.next() {
                        Some(first) => {
                            let (first_core, first_ty) = self.infer_expr(first)?;
                            let mut items_core = vec![first_core];
                            for item in items_iter {
                                let item_core = self.check_expr(item, &first_ty)?;
                                items_core.push(item_core);
                            }
                            Ok((
                                core::Expr::Array(items_core),
                                core::Type::mk_app(
                                    core::Type::mk_array(self.common_kinds),
                                    first_ty,
                                ),
                            ))
                        }
                        None => {
                            let ty_var = self.fresh_typevar(Kind::Type);
                            Ok((
                                core::Expr::Array(Vec::new()),
                                core::Type::mk_app(core::Type::mk_array(self.common_kinds), ty_var),
                            ))
                        }
                    }
                }
                syntax::Expr::Record { fields, rest } => {
                    let mut fields_result: Vec<(Rc<str>, core::Expr, core::Type)> = Vec::new();
                    let mut fields_rows: Vec<(Rc<str>, core::Type)> = Vec::new();
                    for (field, expr) in fields {
                        match self.infer_expr(expr) {
                            Err(err) => return Err(err),
                            Ok((expr_core, expr_ty)) => {
                                let field: Rc<str> = Rc::from(field.as_ref());
                                fields_result.push((field.clone(), expr_core, expr_ty.clone()));
                                fields_rows.push((field, expr_ty));
                            }
                        }
                    }

                    let rest_row_var = self.fresh_typevar(Kind::Row);
                    let mut extending_row = rest_row_var.clone();
                    let mut fields_core = Vec::with_capacity(fields_result.len());
                    for (field, expr_core, expr_ty) in fields_result.into_iter().rev() {
                        let index = self.evidence.placeholder(
                            None,
                            evidence::Constraint::HasField {
                                field: field.clone(),
                                rest: extending_row.clone(),
                            },
                        );
                        fields_core.push((core::Expr::Placeholder(index), expr_core));
                        extending_row = core::Type::mk_rowcons(field, expr_ty, extending_row);
                    }
                    // we did a right fold to build extending_row, but we want fields_core too look like we did a left fold
                    fields_core.reverse();

                    let mut rest_core = None;
                    let mut rest_row = None;
                    let _ = match rest {
                        None => {
                            let expected =
                                core::Type::mk_record(self.common_kinds, fields_rows.clone(), None);
                            let actual = core::Type::mk_record(
                                self.common_kinds,
                                fields_rows.clone(),
                                Some(rest_row_var.clone()),
                            );
                            let context = UnifyTypeContextRefs {
                                expected: &expected,
                                actual: &actual,
                            };
                            self.unify_type(&context, &core::Type::RowNil, &rest_row_var)
                        }
                        Some(expr) => {
                            let expr_core = self.check_expr(
                                expr,
                                &core::Type::mk_app(
                                    core::Type::mk_record_ctor(self.common_kinds),
                                    rest_row_var.clone(),
                                ),
                            )?;
                            rest_core = Some(expr_core);
                            rest_row = Some(rest_row_var);
                            Ok(())
                        }
                    }?;

                    Ok((
                        core::Expr::mk_record(fields_core, rest_core),
                        core::Type::mk_record(self.common_kinds, fields_rows, rest_row),
                    ))
                }
                syntax::Expr::Project(expr, field) => {
                    let field: Rc<str> = Rc::from(field.as_ref());
                    let out_ty = self.fresh_typevar(Kind::Type);
                    let rest = self.fresh_typevar(Kind::Row);
                    let rows =
                        core::Type::mk_rows(vec![(field.clone(), out_ty.clone())], Some(rest));
                    let expr_core = self.check_expr(
                        expr,
                        &core::Type::mk_app(
                            core::Type::mk_record_ctor(self.common_kinds),
                            rows.clone(),
                        ),
                    )?;
                    let offset = self
                        .evidence
                        .placeholder(None, evidence::Constraint::HasField { field, rest: rows });
                    Ok((
                        core::Expr::mk_project(expr_core, core::Expr::Placeholder(offset)),
                        out_ty,
                    ))
                }
                syntax::Expr::Variant(ctor) => {
                    let ctor: Rc<str> = Rc::from(ctor.as_ref());

                    let arg_ty = self.fresh_typevar(Kind::Type);
                    let rest = self.fresh_typevar(Kind::Row);
                    let tag = self.evidence.placeholder(
                        None,
                        evidence::Constraint::HasField {
                            field: ctor.clone(),
                            rest: core::Type::mk_rows(vec![], Some(rest.clone())),
                        },
                    );
                    Ok((
                        core::Expr::mk_variant(core::Expr::Placeholder(tag)),
                        core::Type::mk_arrow(
                            self.common_kinds,
                            arg_ty.clone(),
                            core::Type::mk_variant(
                                self.common_kinds,
                                vec![(ctor, arg_ty)],
                                Some(rest),
                            ),
                        ),
                    ))
                }
                syntax::Expr::Embed(ctor, rest) => {
                    let ctor: Rc<str> = Rc::from(ctor.as_ref());

                    let arg_ty = self.fresh_typevar(Kind::Type);
                    let rest_rows = self.fresh_typevar(Kind::Row);
                    let rest_core = self.check_expr(
                        rest,
                        &core::Type::mk_app(
                            core::Type::mk_variant_ctor(self.common_kinds),
                            rest_rows.clone(),
                        ),
                    )?;
                    let tag = core::Expr::Placeholder(self.evidence.placeholder(
                        None,
                        evidence::Constraint::HasField {
                            field: ctor.clone(),
                            rest: rest_rows.clone(),
                        },
                    ));
                    Ok((
                        core::Expr::mk_embed(tag, rest_core),
                        core::Type::mk_app(
                            core::Type::mk_variant_ctor(self.common_kinds),
                            core::Type::mk_rowcons(ctor, arg_ty, rest_rows),
                        ),
                    ))
                }
                syntax::Expr::Binop(op, left, right) => {
                    match op {
                        syntax::Binop::Add => {
                            let left_core = self.check_expr(left, &core::Type::Int)?;
                            let right_core = self.check_expr(right, &core::Type::Int)?;
                            Ok((
                                core::Expr::mk_binop(*op, left_core, right_core),
                                core::Type::Int,
                            ))
                        }
                        syntax::Binop::Multiply => {
                            let left_core = self.check_expr(left, &core::Type::Int)?;
                            let right_core = self.check_expr(right, &core::Type::Int)?;
                            Ok((
                                core::Expr::mk_binop(*op, left_core, right_core),
                                core::Type::Int,
                            ))
                        }
                        syntax::Binop::Subtract => {
                            let left_core = self.check_expr(left, &core::Type::Int)?;
                            let right_core = self.check_expr(right, &core::Type::Int)?;
                            Ok((
                                core::Expr::mk_binop(*op, left_core, right_core),
                                core::Type::Int,
                            ))
                        }
                        syntax::Binop::Divide => {
                            let left_core = self.check_expr(left, &core::Type::Int)?;
                            let right_core = self.check_expr(right, &core::Type::Int)?;
                            Ok((
                                core::Expr::mk_binop(*op, left_core, right_core),
                                core::Type::Int,
                            ))
                        }

                        syntax::Binop::Append => {
                            let item_ty = self.fresh_typevar(Kind::Type);
                            let expected = core::Type::mk_app(
                                core::Type::mk_array(self.common_kinds),
                                item_ty,
                            );
                            let left_core = self.check_expr(left, &expected)?;
                            let right_core = self.check_expr(right, &expected)?;
                            Ok((core::Expr::mk_binop(*op, left_core, right_core), expected))
                        }

                        syntax::Binop::Or => {
                            let left_core = self.check_expr(left, &core::Type::Bool)?;
                            let right_core = self.check_expr(right, &core::Type::Bool)?;
                            Ok((
                                core::Expr::mk_binop(*op, left_core, right_core),
                                core::Type::Int,
                            ))
                        }
                        syntax::Binop::And => {
                            let left_core = self.check_expr(left, &core::Type::Bool)?;
                            let right_core = self.check_expr(right, &core::Type::Bool)?;
                            Ok((
                                core::Expr::mk_binop(*op, left_core, right_core),
                                core::Type::Int,
                            ))
                        }

                        syntax::Binop::Eq => {
                            todo!("==")
                        }
                        syntax::Binop::Neq => {
                            todo!("!=")
                        }

                        syntax::Binop::Gt => {
                            todo!(">")
                        }
                        syntax::Binop::Gte => {
                            todo!(">=")
                        }
                        syntax::Binop::Lt => {
                            todo!("<")
                        }
                        syntax::Binop::Lte => {
                            todo!("<=")
                        }
                    }
                }
                syntax::Expr::Case(expr, branches) => {
                    let (expr_core, expr_ty) = self.infer_expr(expr)?;
                    let mut branches_core = Vec::new();

                    let expected_body_ty = self.fresh_typevar(Kind::Type);
                    let mut expected_pattern_ty = expr_ty.clone();
                    let mut seen_fallthrough = false;
                    let mut matching_variant = false;
                    let mut seen_ctors: HashSet<Rc<str>> = HashSet::new();
                    let mut expr_rows = None;
                    for branch in branches {
                        if seen_fallthrough
                            || match &branch.pattern.item {
                                syntax::Pattern::Variant { name, .. } => {
                                    seen_ctors.contains(name.as_str())
                                }
                                _ => false,
                            }
                        {
                            return Err(TypeError::RedundantPattern {
                                source: self.source(),
                                pos: branch.pattern.pos,
                            });
                        }
                        let (pattern_core, _pattern_ty, pattern_binds) =
                            with_position!(self, branch.pattern.pos, {
                                let ((pattern_core, pattern_ty, pattern_binds), saw_variant) =
                                    match &branch.pattern.item {
                                        syntax::Pattern::Wildcard => {
                                            Ok((self.infer_wildcard_pattern(), false))
                                        }
                                        syntax::Pattern::Name(n) => {
                                            Ok((self.infer_name_pattern(n), false))
                                        }
                                        syntax::Pattern::Record { names, rest } => {
                                            Ok((self.infer_record_pattern(names, rest), false))
                                        }
                                        syntax::Pattern::Variant { name, arg } => {
                                            let res = self.infer_variant_pattern_case(
                                                &mut expr_rows,
                                                &mut expected_pattern_ty,
                                                &mut seen_ctors,
                                                name,
                                                arg,
                                            )?;
                                            Ok((res, true))
                                        }
                                    }?;
                                if !saw_variant {
                                    let context: UnifyTypeContextRefs = UnifyTypeContextRefs {
                                        expected: &expected_pattern_ty,
                                        actual: &pattern_ty,
                                    };
                                    match self.unify_type(
                                        &context,
                                        &expected_pattern_ty,
                                        &pattern_ty,
                                    ) {
                                        Err(err) => {
                                            return Err(err);
                                        }
                                        Ok(()) => {}
                                    }
                                }
                                match branch.pattern.item {
                                    syntax::Pattern::Wildcard
                                    | syntax::Pattern::Name(_)
                                    | syntax::Pattern::Record { .. } => {
                                        seen_fallthrough = true;
                                    }
                                    syntax::Pattern::Variant { .. } => {
                                        matching_variant = true;
                                    }
                                }
                                Ok((pattern_core, pattern_ty, pattern_binds))
                            })?;
                        self.bound_vars.insert(&pattern_binds);
                        let body_core = self.check_expr(&branch.body, &expected_body_ty)?;
                        self.bound_vars.delete(pattern_binds.len());
                        branches_core.push(core::Branch {
                            pattern: pattern_core,
                            body: body_core,
                        });
                    }

                    if matching_variant && !seen_fallthrough {
                        let expr_ty = self.zonk_type(&expr_ty);
                        let row_parts = expr_ty.unwrap_variant().unwrap();
                        let ctors: Vec<(Rc<str>, core::Type)> = row_parts
                            .fields
                            .iter()
                            .map(|(x, y)| ((*x).clone(), (*y).clone()))
                            .collect();
                        let rest: Option<core::Type> = row_parts.rest.cloned();
                        let context = UnifyTypeContextRefs {
                            expected: &core::Type::mk_variant(
                                self.common_kinds,
                                ctors.clone(),
                                None,
                            ),
                            actual: &core::Type::mk_variant(self.common_kinds, ctors, rest),
                        };
                        let _ = self.unify_type(
                            &context,
                            &expected_pattern_ty,
                            &core::Type::mk_variant(self.common_kinds, Vec::new(), None),
                        )?;
                    }

                    Ok((
                        core::Expr::mk_case(expr_core, branches_core),
                        expected_body_ty,
                    ))
                }
                syntax::Expr::Comp(lines) => {
                    if lines.is_empty() {
                        return Err(TypeError::EmptyCompExpr {
                            source: self.source(),
                            pos: self.current_position(),
                        });
                    }

                    enum CheckedCompLine {
                        Expr(core::Expr),
                        Bind {
                            vars_bound: usize,
                            value: core::Expr,
                        },
                        Return(core::Expr),
                    }

                    let mut ret_ty = None;
                    let mut checked_lines: Vec<CheckedCompLine> = lines
                        .iter()
                        .map(|line| match line {
                            syntax::CompLine::Expr(value) => {
                                let ret_ty_var = self.fresh_typevar(Kind::Type);
                                let value = self.check_expr(
                                    value,
                                    &core::Type::mk_app(
                                        core::Type::mk_io(self.common_kinds),
                                        ret_ty_var.clone(),
                                    ),
                                )?;

                                ret_ty = Some(ret_ty_var);
                                Ok(CheckedCompLine::Expr(value))
                            }
                            syntax::CompLine::Bind(name, value) => {
                                let name_ty = self.fresh_typevar(Kind::Type);
                                let value = self.check_expr(
                                    value,
                                    &core::Type::mk_app(
                                        core::Type::mk_io(self.common_kinds),
                                        name_ty.clone(),
                                    ),
                                )?;

                                // [note: checking `bind`s]
                                //
                                // Register the variables bound by this line so the variables
                                // can be references by subsequent lines.
                                self.bound_vars.insert(&[(name.clone(), name_ty)]);

                                ret_ty = None;
                                Ok(CheckedCompLine::Bind {
                                    vars_bound: 1,
                                    value,
                                })
                            }
                            syntax::CompLine::Return(value) => {
                                let (value, value_ty) = self.infer_expr(value)?;

                                ret_ty = Some(value_ty);
                                Ok(CheckedCompLine::Return(value))
                            }
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    match ret_ty {
                        None => {
                            debug_assert!(matches!(
                                checked_lines.last().unwrap(),
                                CheckedCompLine::Bind { .. }
                            ));

                            Err(TypeError::CompExprEndsWithBind {
                                source: self.source(),
                                pos: self.current_position(),
                            })
                        }
                        Some(ret_ty) => {
                            debug_assert!(!matches!(
                                checked_lines.last().unwrap(),
                                CheckedCompLine::Bind { .. }
                            ));

                            let mut desugared: core::Expr = match checked_lines.pop().unwrap() {
                                CheckedCompLine::Bind { .. } => unreachable!(),
                                CheckedCompLine::Expr(value) => value,
                                CheckedCompLine::Return(value) => core::Expr::mk_app(
                                    core::Expr::Name(String::from("pureIO")),
                                    value,
                                ),
                            };

                            for checked_line in checked_lines.into_iter().rev() {
                                match checked_line {
                                    CheckedCompLine::Expr(value) => {
                                        // Desugar[comp { value; rest }] -> bindIO value (\_ -> Desugar[rest])
                                        desugared = core::Expr::mk_app(
                                            core::Expr::mk_app(
                                                core::Expr::Name(String::from("bindIO")),
                                                value,
                                            ),
                                            core::Expr::mk_lam(false, desugared),
                                        );
                                    }
                                    CheckedCompLine::Bind { vars_bound, value } => {
                                        // Delete the variables that were bound in [note: checking `bind`s]
                                        self.bound_vars.delete(vars_bound);

                                        // Desugar[comp { bind name <- value; rest }] -> bindIO value (\name -> Desugar[rest])
                                        desugared = core::Expr::mk_app(
                                            core::Expr::mk_app(
                                                core::Expr::Name(String::from("bindIO")),
                                                value,
                                            ),
                                            core::Expr::mk_lam(true, desugared),
                                        );
                                    }
                                    CheckedCompLine::Return(value) => {
                                        // Desugar[comp { return value; rest }] -> bindIO (pureIO value) (\_ -> Desugar[rest])
                                        desugared = core::Expr::mk_app(
                                            core::Expr::mk_app(
                                                core::Expr::Name(String::from("bindIO")),
                                                core::Expr::mk_app(
                                                    core::Expr::Name(String::from("pureIO")),
                                                    value,
                                                ),
                                            ),
                                            core::Expr::mk_lam(true, desugared),
                                        );
                                    }
                                }
                            }

                            Ok((
                                desugared,
                                core::Type::mk_app(core::Type::mk_io(self.common_kinds), ret_ty),
                            ))
                        }
                    }
                }
            }
        )
    }

    fn check_expr(
        &mut self,
        expr: &syntax::Spanned<syntax::Expr>,
        ty: &core::Type,
    ) -> Result<core::Expr, TypeError> {
        with_position!(
            self,
            expr.pos,
            match &expr.item {
                syntax::Expr::Lam { args, body } => {
                    self.check_duplicate_args(args)?;

                    let out_ty = self.fresh_typevar(Kind::Type);
                    let mut actual = out_ty.clone();

                    let mut arg_tys = Vec::new();
                    for _ in args.iter().rev() {
                        let arg_ty = self.fresh_typevar(Kind::Type);
                        arg_tys.push(arg_ty.clone());
                        actual = core::Type::mk_arrow(self.common_kinds, arg_ty, actual);
                    }
                    arg_tys.reverse();

                    let context = UnifyTypeContextRefs {
                        expected: ty,
                        actual: &actual,
                    };
                    self.unify_type(&context, ty, &actual)?;

                    let mut args_core = Vec::new();
                    let mut args_binds = Vec::new();
                    for (arg, arg_ty) in args.iter().zip(arg_tys.iter()) {
                        match self.check_pattern(arg, arg_ty) {
                            Err(err) => {
                                return Err(err);
                            }
                            Ok((arg_core, arg_binds)) => {
                                args_core.push(arg_core);
                                args_binds.extend(arg_binds);
                            }
                        }
                    }

                    self.bound_vars.insert(&args_binds);
                    let body_core = self.check_expr(body, &out_ty)?;
                    self.bound_vars.delete(args_binds.len());

                    let mut expr_core = body_core;
                    for arg_core in args_core.iter().rev() {
                        match arg_core {
                            core::Pattern::Wildcard => {
                                expr_core = core::Expr::mk_lam(false, expr_core);
                            }
                            core::Pattern::Name => {
                                expr_core = core::Expr::mk_lam(true, expr_core);
                            }
                            _ => {
                                expr_core = core::Expr::mk_lam(
                                    true,
                                    core::Expr::mk_case(
                                        core::Expr::Var(0),
                                        vec![core::Branch {
                                            pattern: arg_core.clone(),
                                            body: expr_core,
                                        }],
                                    ),
                                );
                            }
                        }
                    }
                    Ok(expr_core)
                }
                _ => {
                    let expected = ty;
                    let (expr, actual) = self.infer_expr(expr)?;
                    let context = UnifyTypeContextRefs {
                        expected,
                        actual: &actual,
                    };
                    self.unify_type(&context, expected, &actual)?;
                    Ok(expr)
                }
            }
        )
    }
}
