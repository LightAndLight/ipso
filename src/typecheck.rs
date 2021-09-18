use crate::{
    builtins, core,
    diagnostic::{self, InputLocation},
    evidence,
    evidence::{solver::solve_placeholder, Constraint, Evidence},
    import::{ModulePath, Modules},
    rope::Rope,
    syntax::{self, ModuleName, Spanned, Type},
};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    path::Path,
    rc::Rc,
    todo,
};

use self::substitution::Substitution;

pub mod substitution;

#[cfg(test)]
mod test;

macro_rules! fresh_kindvar {
    ($val:expr) => {{
        let n = $val.len();
        $val.push(None);
        syntax::Kind::Meta(n)
    }};
}

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
    pub ty_vars: Vec<syntax::Kind>,
    pub antecedents: Vec<syntax::Type<usize>>,
    pub consequent: syntax::Type<usize>,
    pub evidence: core::Expr,
}

impl Implication {
    pub fn instantiate_many(&self, tys: &[Type<usize>]) -> Self {
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
    location: InputLocation,
    kind_solutions: Vec<Option<syntax::Kind>>,
    pub type_solutions: Vec<(syntax::Kind, Option<Type<usize>>)>,
    pub implications: Vec<Implication>,
    pub evidence: Evidence,
    type_context: HashMap<Rc<str>, syntax::Kind>,
    context: HashMap<String, core::TypeSig>,
    pub registered_bindings: HashMap<String, (core::TypeSig, core::Expr)>,
    module_context: HashMap<ModulePath, HashMap<String, core::TypeSig>>,
    module_unmapping: HashMap<ModuleName, ModulePath>,
    class_context: HashMap<Rc<str>, core::ClassDeclaration>,
    bound_vars: BoundVars<Type<usize>>,
    bound_tyvars: BoundVars<syntax::Kind>,
    position: Option<usize>,
    modules: &'modules Modules<'modules>,
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
    ty: Type<A>,
    has_kind: syntax::Kind,
    unifying_types: Option<UnifyTypeContext<A>>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UnifyTypeContext<A> {
    pub expected: Type<A>,
    pub actual: Type<A>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SolveConstraintContext {
    pub pos: usize,
    pub constraint: Type<Rc<str>>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum TypeError {
    DuplicateArgument {
        location: InputLocation,
        pos: usize,
        name: String,
    },
    DuplicateClassArgument {
        location: InputLocation,
        pos: usize,
    },
    NotInScope {
        location: InputLocation,
        pos: usize,
        name: String,
    },
    NotInModule {
        location: InputLocation,
        pos: usize,
        item: String,
    },
    KindMismatch {
        location: InputLocation,
        pos: usize,
        context: UnifyKindContext<Rc<str>>,
        expected: syntax::Kind,
        actual: syntax::Kind,
    },
    TypeMismatch {
        location: InputLocation,
        pos: usize,
        context: UnifyTypeContext<Rc<str>>,
        expected: Type<Rc<str>>,
        actual: Type<Rc<str>>,
    },
    RedundantPattern {
        location: InputLocation,
        pos: usize,
    },
    KindOccurs {
        location: InputLocation,
        pos: usize,
        meta: usize,
        kind: syntax::Kind,
    },
    TypeOccurs {
        location: InputLocation,
        pos: usize,
        meta: usize,
        ty: Type<Rc<str>>,
    },
    NoSuchClass {
        location: InputLocation,
        pos: usize,
    },
    NotAMember {
        location: InputLocation,
        pos: usize,
        cls: Rc<str>,
    },
    CannotDeduce {
        location: InputLocation,
        context: Option<SolveConstraintContext>,
    },
    ShadowedModuleName {
        location: InputLocation,
        pos: usize,
    },
}

impl syntax::Pattern {
    fn get_arg_names(&self) -> Vec<&Spanned<String>> {
        let mut arg_names = Vec::new();
        match self {
            syntax::Pattern::Name(n) => {
                arg_names.push(n);
            }
            syntax::Pattern::Record { names, rest } => {
                for name in names {
                    arg_names.push(name);
                }
                match rest {
                    None => {}
                    Some(n) => {
                        arg_names.push(n);
                    }
                }
            }
            syntax::Pattern::Variant { name: _, arg } => {
                arg_names.push(arg);
            }
            syntax::Pattern::Wildcard => {}
        }
        arg_names
    }
}

impl TypeError {
    pub fn location(&self) -> InputLocation {
        match self {
            TypeError::KindMismatch { location, .. } => location.clone(),
            TypeError::TypeMismatch { location, .. } => location.clone(),
            TypeError::NotInScope { location, .. } => location.clone(),
            TypeError::NotInModule { location, .. } => location.clone(),
            TypeError::DuplicateArgument { location, .. } => location.clone(),
            TypeError::DuplicateClassArgument { location, .. } => location.clone(),
            TypeError::RedundantPattern { location, .. } => location.clone(),
            TypeError::KindOccurs { location, .. } => location.clone(),
            TypeError::TypeOccurs { location, .. } => location.clone(),
            TypeError::NoSuchClass { location, .. } => location.clone(),
            TypeError::NotAMember { location, .. } => location.clone(),
            TypeError::CannotDeduce { location, .. } => location.clone(),
            TypeError::ShadowedModuleName { location, .. } => location.clone(),
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
        }
    }

    pub fn report(&self, diagnostic: &mut diagnostic::Diagnostic) {
        diagnostic.item(diagnostic::Item {
            location: self.location(),
            offset: self.position(),
            message: self.message(),
            addendum: self.addendum(),
        })
    }
}

#[macro_export]
macro_rules! with_tc {
    ($path:expr, $location:expr, $f:expr) => {{
        use crate::import::Modules;
        use typed_arena::Arena;
        let modules_data = Arena::new();
        let modules = Modules::new(&modules_data);
        let tc = Typechecker::new_with_builtins($path, $location, &modules);
        $f(tc)
    }};
}

#[macro_export]
macro_rules! current_dir_with_tc {
    ($f:expr) => {{
        let path = std::env::current_dir().unwrap();
        crate::with_tc!(
            path.as_path(),
            crate::diagnostic::InputLocation::Interactive {
                label: String::from("(typechecker)")
            },
            $f
        )
    }};
}

impl<'modules> Typechecker<'modules> {
    pub fn new(
        working_dir: &'modules Path,
        location: InputLocation,
        modules: &'modules Modules,
    ) -> Self {
        Typechecker {
            location,
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
        location: InputLocation,
        modules: &'modules Modules,
    ) -> Self {
        let mut tc = Self::new(working_dir, location, modules);
        tc.register_from_import(&builtins::builtins(), &syntax::Names::All);
        tc
    }

    fn eq_zonked_type(&self, t1: &Type<usize>, t2: &Type<usize>) -> bool {
        fn zonk_just_enough<'a>(tc: &'a Typechecker, t: &'a Type<usize>) -> &'a Type<usize> {
            match t {
                Type::Meta(n) => match &tc.type_solutions[*n].1 {
                    None => t,
                    Some(sol) => zonk_just_enough(tc, sol),
                },
                t => t,
            }
        }
        let t2: &Type<usize> = zonk_just_enough(self, t2);
        match t1 {
            Type::Name(n) => match t2 {
                Type::Name(n2) => n == n2,
                _ => false,
            },
            Type::Var(v) => match t2 {
                Type::Var(v2) => v == v2,
                _ => false,
            },
            Type::Bool => matches!(t2, Type::Bool),
            Type::Int => matches!(t2, Type::Int),
            Type::Char => matches!(t2, Type::Char),
            Type::String => matches!(t2, Type::String),
            Type::Bytes => matches!(t2, Type::Bytes),
            Type::Arrow => matches!(t2, Type::Arrow),
            Type::FatArrow => matches!(t2, Type::FatArrow),
            Type::Array => matches!(t2, Type::Arrow),
            Type::Record => matches!(t2, Type::Record),
            Type::Variant => matches!(t2, Type::Variant),
            Type::IO => matches!(t2, Type::IO),
            Type::RowNil => matches!(t2, Type::RowNil),
            Type::Unit => matches!(t2, Type::Unit),
            Type::Constraints(cs) => match t2 {
                Type::Constraints(cs2) => {
                    cs.len() == cs2.len()
                        && cs
                            .iter()
                            .zip(cs2.iter())
                            .all(|(a, b)| self.eq_zonked_type(a, b))
                }
                _ => false,
            },
            Type::App(a, b) => match t2 {
                Type::App(a2, b2) => self.eq_zonked_type(a, a2) && self.eq_zonked_type(b, b2),
                _ => false,
            },
            Type::RowCons(a, b, c) => match t2 {
                Type::RowCons(a2, b2, c2) => {
                    a == a2 && self.eq_zonked_type(b, b2) && self.eq_zonked_type(c, c2)
                }
                _ => false,
            },
            Type::HasField(a, b) => match t2 {
                Type::HasField(a2, b2) => a == a2 && self.eq_zonked_type(b, b2),
                _ => false,
            },
            Type::Meta(n) => match &self.type_solutions[*n].1 {
                None => match t2 {
                    Type::Meta(n2) => n == n2,
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

        // generate constraint's kind
        let mut constraint_kind = syntax::Kind::Constraint;
        for (_, kind) in decl.args.iter().rev() {
            constraint_kind = syntax::Kind::mk_arrow(kind.clone(), constraint_kind);
        }
        self.type_context.insert(decl_name.clone(), constraint_kind);

        // generate superclass accessors
        let applied_type = (0..decl.args.len())
            .into_iter()
            .fold(Type::Name(decl_name.clone()), |acc, el| {
                Type::mk_app(acc, Type::Var(el))
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
        let decl_bindings = decl.get_bindings();
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
        ty_vars: &[(Rc<str>, syntax::Kind)],
        superclass_constructors: &[core::Expr],
        assumes: &[Type<usize>],
        head: &Type<usize>,
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

    fn register_from_import(&mut self, module: &core::Module, names: &syntax::Names) {
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
        context: Option<&UnifyTypeContext<usize>>,
        ty: &Type<usize>,
        kind: &syntax::Kind,
    ) -> Result<Type<usize>, TypeError> {
        let expected = kind;
        let (ty, actual) = self.infer_kind(ty)?;
        let context = UnifyKindContext {
            ty: ty.clone(),
            has_kind: expected.clone(),
            unifying_types: context.cloned(),
        };
        self.unify_kind(&context, &expected, &actual)?;
        Ok(ty)
    }

    fn abstract_evidence(
        &mut self,
        expr: core::Expr,
    ) -> Result<(core::Expr, Vec<Type<usize>>), TypeError> {
        let expr = expr.subst_placeholder(&mut |p| {
            let (expr, _solved_constraint) = solve_placeholder(self, *p)?;
            Ok(expr)
        })?;

        let mut unsolved_constraints: Vec<(core::EVar, Type<usize>)> = Vec::new();
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
        ty: Type<usize>,
    ) -> Result<(core::Expr, core::TypeSig), TypeError> {
        let (expr, unsolved_constraints) = self.abstract_evidence(expr)?;

        let mut ty = ty;
        for constraint in unsolved_constraints.into_iter().rev() {
            ty = Type::mk_fatarrow(constraint, ty);
        }

        let ty_vars = self
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
        ty: &Type<Rc<str>>,
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

        let _ = self.context.insert(
            name.to_string(),
            core::TypeSig {
                ty_vars: ty_var_kinds.clone(),
                body: ty.clone(),
            },
        );

        self.bound_tyvars.insert(&ty_var_kinds);

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
            ty.clone(),
        )?;

        let (body, sig) = self.generalise(body, ty.clone())?;
        self.evidence = Evidence::new();

        self.bound_tyvars.delete(ty_var_kinds_len);

        self.context.remove(name);

        Ok(core::Declaration::Definition {
            name: name.to_string(),
            sig,
            body,
        })
    }

    fn check_class_member(
        &mut self,
        class_args_kinds: &[(Rc<str>, syntax::Kind)],
        name: &str,
        type_: &Type<Rc<str>>,
    ) -> Result<core::ClassMember, TypeError> {
        let class_args: Vec<Rc<str>> = class_args_kinds.iter().map(|(x, _)| x.clone()).collect();
        let (type_, ty_vars) = type_.abstract_vars(&class_args);

        let class_args_kinds_map: HashMap<Rc<str>, syntax::Kind> = class_args_kinds
            .iter()
            .map(|(a, b)| (a.clone(), b.clone()))
            .collect();
        let ty_var_kinds: Vec<(Rc<str>, syntax::Kind)> = ty_vars
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
        let checked_type = self.check_kind(None, &type_, &syntax::Kind::Type)?;
        self.bound_tyvars.delete(ty_var_kinds.len());

        let ty_vars = ty_var_kinds
            .iter()
            .map(|(a, b)| (a.clone(), self.zonk_kind(true, b)))
            .collect();
        let sig = core::TypeSig {
            ty_vars,
            body: checked_type,
        };
        Ok(core::ClassMember {
            name: name.to_string(),
            sig,
        })
    }

    fn check_class(
        &mut self,
        supers: &[Spanned<Type<Rc<str>>>],
        name: &Rc<str>,
        args: &[Spanned<Rc<str>>],
        members: &[(String, Type<Rc<str>>)],
    ) -> Result<core::Declaration, TypeError> {
        let args_len = args.len();
        let arg_names: Vec<Rc<str>> = args.iter().map(|x| x.item.clone()).collect();
        let args_kinds = {
            let mut seen: HashSet<Rc<str>> = HashSet::new();
            let mut args_kinds = Vec::with_capacity(args_len);
            for arg in args.iter() {
                if seen.contains(&arg.item) {
                    return Err(TypeError::DuplicateClassArgument {
                        location: self.location(),
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
                match self.check_kind(None, &s_item, &syntax::Kind::Constraint) {
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
                .collect(),
            members: checked_members,
        }))
    }

    fn check_instance(
        &mut self,
        assumes: &[Spanned<Type<Rc<str>>>],
        name: &Spanned<Rc<str>>,
        args: &[Type<Rc<str>>],
        members: &[(Spanned<String>, Vec<syntax::Pattern>, Spanned<syntax::Expr>)],
    ) -> Result<core::Declaration, TypeError> {
        let class_context = &self.class_context;
        let class_decl: core::ClassDeclaration = match class_context.get(&name.item) {
            None => Err(TypeError::NoSuchClass {
                location: self.location(),
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

        let assumes: Vec<Type<usize>> = assumes
            .iter()
            .map(|assume| assume.item.abstract_vars(&ty_vars).0)
            .collect();

        let ty_var_kinds: Vec<(Rc<str>, syntax::Kind)> = {
            let kind_solutions = &mut self.kind_solutions;
            ty_vars
                .into_iter()
                .map(|var| (var, fresh_kindvar!(kind_solutions)))
                .collect()
        };

        let (_, args) = head.unwrap_app();
        let args: Vec<Type<usize>> = args.into_iter().cloned().collect();

        self.bound_tyvars.insert(&ty_var_kinds);

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
                        constraint: self.fill_ty_names(superclass.clone()),
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

        with_position!(
            self,
            name.pos,
            self.check_kind(None, &head, &syntax::Kind::Constraint)
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
                        location: self.location(),
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
                            member_type.sig.body.clone(),
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
            assumes,
            head,
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
                let signatures = self.modules.lookup(&path).unwrap().get_signatures();
                self.module_context.insert(path.clone(), signatures);
                self.module_unmapping
                    .insert(ModuleName(vec![actual_name.item.clone()]), path.clone());
                module_mapping.insert(path, core::ModuleUsage::Named(actual_name.item.clone()));
                Ok(())
            }
            Some(_) => Err(TypeError::ShadowedModuleName {
                location: self.location(),
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

    pub fn location(&self) -> InputLocation {
        self.location.clone()
    }

    fn current_position(&self) -> usize {
        self.position.unwrap_or(0)
    }

    fn lookup_var(&self, name: &str) -> Option<(usize, Type<usize>)> {
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

    pub fn zonk_type(&self, ty: &Type<usize>) -> Type<usize> {
        match ty {
            Type::Name(n) => Type::Name(n.clone()),
            Type::Var(n) => Type::Var(*n),
            Type::Bool => Type::Bool,
            Type::Int => Type::Int,
            Type::Char => Type::Char,
            Type::String => Type::String,
            Type::Bytes => Type::Bytes,
            Type::Arrow => Type::Arrow,
            Type::FatArrow => Type::FatArrow,
            Type::Constraints(cs) => {
                Type::Constraints(cs.iter().map(|c| self.zonk_type(c)).collect())
            }
            Type::Array => Type::Array,
            Type::Record => Type::Record,
            Type::Variant => Type::Variant,
            Type::IO => Type::IO,
            Type::App(a, b) => Type::mk_app(self.zonk_type(a), self.zonk_type(b)),
            Type::RowNil => Type::RowNil,
            Type::Unit => Type::Unit,
            Type::RowCons(field, ty, rest) => {
                Type::mk_rowcons(field.clone(), self.zonk_type(ty), self.zonk_type(rest))
            }
            Type::HasField(field, rest) => Type::mk_hasfield(field.clone(), self.zonk_type(rest)),
            Type::Meta(n) => match &self.type_solutions[*n].1 {
                None => Type::Meta(*n),
                Some(ty) => self.zonk_type(ty),
            },
        }
    }

    pub fn zonk_kind(&self, close_unsolved: bool, kind: &syntax::Kind) -> syntax::Kind {
        match kind {
            syntax::Kind::Type => syntax::Kind::Type,
            syntax::Kind::Row => syntax::Kind::Row,
            syntax::Kind::Constraint => syntax::Kind::Constraint,
            syntax::Kind::Arrow(a, b) => syntax::Kind::mk_arrow(
                self.zonk_kind(close_unsolved, a),
                self.zonk_kind(close_unsolved, b),
            ),
            syntax::Kind::Meta(m) => match &self.kind_solutions[*m] {
                None => {
                    if close_unsolved {
                        syntax::Kind::Type
                    } else {
                        syntax::Kind::Meta(*m)
                    }
                }
                Some(kind) => self.zonk_kind(close_unsolved, kind),
            },
        }
    }

    fn fresh_kindvar(&mut self) -> syntax::Kind {
        fresh_kindvar!(self.kind_solutions)
    }

    pub fn fill_ty_names(&self, ty: Type<usize>) -> Type<Rc<str>> {
        ty.map(&mut |&ix| self.bound_tyvars.lookup_index(ix).unwrap().0.clone())
    }

    fn kind_mismatch<A>(
        &self,
        context: &UnifyKindContext<usize>,
        expected: &syntax::Kind,
        actual: &syntax::Kind,
    ) -> Result<A, TypeError> {
        let context = UnifyKindContext {
            ty: self.fill_ty_names(self.zonk_type(&context.ty)),
            has_kind: self.zonk_kind(false, &context.has_kind),
            unifying_types: context.unifying_types.clone().map(|x| UnifyTypeContext {
                expected: self.fill_ty_names(self.zonk_type(&x.expected)),
                actual: self.fill_ty_names(self.zonk_type(&x.actual)),
            }),
        };
        Err(TypeError::KindMismatch {
            location: self.location(),
            pos: self.current_position(),
            context,
            expected: expected.clone(),
            actual: actual.clone(),
        })
    }

    fn unify_kind(
        &mut self,
        context: &UnifyKindContext<usize>,
        expected: &syntax::Kind,
        actual: &syntax::Kind,
    ) -> Result<(), TypeError> {
        match expected {
            syntax::Kind::Type => match actual {
                syntax::Kind::Type => Ok(()),
                syntax::Kind::Meta(m) => self.solve_kindvar_right(context, expected, *m),
                _ => self.kind_mismatch(context, expected, actual),
            },
            syntax::Kind::Row => match actual {
                syntax::Kind::Row => Ok(()),
                syntax::Kind::Meta(m) => self.solve_kindvar_right(context, expected, *m),
                _ => self.kind_mismatch(context, expected, actual),
            },
            syntax::Kind::Constraint => match actual {
                syntax::Kind::Constraint => Ok(()),
                syntax::Kind::Meta(m) => self.solve_kindvar_right(context, expected, *m),
                _ => self.kind_mismatch(context, expected, actual),
            },
            syntax::Kind::Arrow(expected_a, expected_b) => match actual {
                syntax::Kind::Arrow(actual_a, actual_b) => {
                    self.unify_kind(context, expected_a, actual_a)?;
                    self.unify_kind(context, expected_b, actual_b)
                }
                syntax::Kind::Meta(m) => self.solve_kindvar_right(
                    context,
                    &syntax::Kind::Arrow((*expected_a).clone(), (*expected_b).clone()),
                    *m,
                ),
                _ => self.kind_mismatch(
                    context,
                    &syntax::Kind::Arrow((*expected_a).clone(), (*expected_b).clone()),
                    actual,
                ),
            },
            syntax::Kind::Meta(expected_m) => match actual {
                syntax::Kind::Meta(actual_m) if expected_m == actual_m => Ok(()),
                actual => self.solve_kindvar_left(context, *expected_m, actual),
            },
        }
    }

    fn occurs_kind(&self, meta: usize, kind: &syntax::Kind) -> Result<(), TypeError> {
        match kind.iter_metas().find(|&other| {
            meta == other
                || match &self.kind_solutions[other] {
                    None => false,
                    Some(kind) => self.occurs_kind(meta, kind).is_err(),
                }
        }) {
            None => Ok(()),
            Some(_) => Err(TypeError::KindOccurs {
                location: self.location(),
                pos: self.current_position(),
                meta,
                kind: self.zonk_kind(false, kind),
            }),
        }
    }

    fn solve_kindvar_right(
        &mut self,
        context: &UnifyKindContext<usize>,
        expected: &syntax::Kind,
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
        context: &UnifyKindContext<usize>,
        meta: usize,
        actual: &syntax::Kind,
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
            location: self.location(),
            pos: self.current_position(),
            name: name.to_string(),
        })
    }

    fn type_mismatch<A>(
        &self,
        context: &UnifyTypeContext<usize>,
        expected: Type<usize>,
        actual: Type<usize>,
    ) -> Result<A, TypeError> {
        let context = UnifyTypeContext {
            expected: self.fill_ty_names(self.zonk_type(&context.expected)),
            actual: self.fill_ty_names(self.zonk_type(&context.actual)),
        };
        Err(TypeError::TypeMismatch {
            location: self.location(),
            pos: self.current_position(),
            context,
            expected: self.fill_ty_names(expected),
            actual: self.fill_ty_names(actual),
        })
    }

    fn occurs_type(&self, meta: usize, ty: &Type<usize>) -> Result<(), TypeError> {
        debug_assert!(
            match ty {
                Type::Meta(n) => self.type_solutions[*n].1 == None,
                _ => true,
            },
            "ty is a meta with a solution"
        );

        fn metas_set(tc: &Typechecker, ty: &Type<usize>, set: &mut HashSet<usize>) {
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
            let mut set = HashSet::new();
            metas_set(self, ty, &mut set);
            set
        };

        if set.contains(&meta) {
            Err(TypeError::TypeOccurs {
                location: self.location(),
                pos: self.current_position(),
                meta,
                ty: self.fill_ty_names(self.zonk_type(ty)),
            })
        } else {
            Ok(())
        }
    }

    pub fn solve_typevar_right(
        &mut self,
        context: &UnifyTypeContext<usize>,
        expected: &Type<usize>,
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
        context: &UnifyTypeContext<usize>,
        meta: usize,
        actual: &Type<usize>,
    ) -> Result<(), TypeError> {
        match &self.type_solutions[meta].1.clone() {
            None => {
                self.type_solutions[meta].1 = Some(actual.clone());
                Ok(())
            }
            Some(expected) => self.unify_type(context, expected, actual),
        }
    }

    fn lookup_typevar(&self, n: usize) -> Result<syntax::Kind, TypeError> {
        match self.type_solutions.get(n) {
            None => panic!("missing kind for type var: ?{}", n),
            Some((k, _)) => Ok(k.clone()),
        }
    }

    fn infer_kind(&mut self, ty: &Type<usize>) -> Result<(Type<usize>, syntax::Kind), TypeError> {
        match ty {
            Type::Name(n) => match self.type_context.get(n) {
                None => self.not_in_scope(n),
                Some(kind) => Ok((Type::Name(n.clone()), kind.clone())),
            },
            Type::Var(ix) => match self.bound_tyvars.lookup_index(*ix) {
                None => {
                    panic!("missing tyvar {:?}", ix);
                }
                Some((_, kind)) => Ok((Type::Var(*ix), kind.clone())),
            },
            Type::Bool => Ok((Type::Bool, syntax::Kind::Type)),
            Type::Int => Ok((Type::Int, syntax::Kind::Type)),
            Type::Char => Ok((Type::Char, syntax::Kind::Type)),
            Type::String => Ok((Type::String, syntax::Kind::Type)),
            Type::Bytes => Ok((Type::Bytes, syntax::Kind::Type)),
            Type::Arrow => Ok((
                Type::Arrow,
                syntax::Kind::mk_arrow(
                    syntax::Kind::Type,
                    syntax::Kind::mk_arrow(syntax::Kind::Type, syntax::Kind::Type),
                ),
            )),
            Type::FatArrow => Ok((
                Type::FatArrow,
                syntax::Kind::mk_arrow(
                    syntax::Kind::Constraint,
                    syntax::Kind::mk_arrow(syntax::Kind::Type, syntax::Kind::Type),
                ),
            )),
            Type::Constraints(constraints) => {
                let mut new_constraints = Vec::new();
                for constraint in constraints {
                    match self.check_kind(None, constraint, &syntax::Kind::Constraint) {
                        Err(err) => return Err(err),
                        Ok(new_constraint) => {
                            new_constraints.push(new_constraint);
                        }
                    }
                }
                Ok((Type::Constraints(new_constraints), syntax::Kind::Constraint))
            }
            Type::Array => Ok((
                Type::Array,
                syntax::Kind::mk_arrow(syntax::Kind::Type, syntax::Kind::Type),
            )),
            Type::Record => Ok((
                Type::Record,
                syntax::Kind::mk_arrow(syntax::Kind::Row, syntax::Kind::Type),
            )),
            Type::Variant => Ok((
                Type::Variant,
                syntax::Kind::mk_arrow(syntax::Kind::Row, syntax::Kind::Type),
            )),
            Type::IO => Ok((
                Type::IO,
                syntax::Kind::mk_arrow(syntax::Kind::Type, syntax::Kind::Type),
            )),
            Type::App(a, b) => {
                let in_kind = self.fresh_kindvar();
                let out_kind = self.fresh_kindvar();
                let a = self.check_kind(
                    None,
                    a,
                    &syntax::Kind::mk_arrow(in_kind.clone(), out_kind.clone()),
                )?;
                let b = self.check_kind(None, b, &in_kind)?;
                Ok((Type::mk_app(a, b), out_kind))
            }
            Type::RowNil => Ok((Type::RowNil, syntax::Kind::Row)),
            Type::RowCons(field, ty, rest) => {
                let ty = self.check_kind(None, ty, &syntax::Kind::Type)?;
                let rest = self.check_kind(None, rest, &syntax::Kind::Row)?;
                Ok((Type::mk_rowcons(field.clone(), ty, rest), syntax::Kind::Row))
            }
            Type::HasField(field, rest) => {
                let rest = self.check_kind(None, rest, &syntax::Kind::Row)?;
                Ok((
                    Type::mk_hasfield(field.clone(), rest),
                    syntax::Kind::Constraint,
                ))
            }
            Type::Unit => Ok((Type::Unit, syntax::Kind::Type)),
            Type::Meta(n) => {
                let kind = self.lookup_typevar(*n)?;
                Ok((Type::Meta(*n), kind))
            }
        }
    }

    pub fn fresh_typevar<A>(&mut self, kind: syntax::Kind) -> Type<A> {
        let n = self.type_solutions.len();
        self.type_solutions.push((kind, None));
        Type::Meta(n)
    }

    fn walk(&self, ty: &Type<usize>) -> Type<usize> {
        match ty {
            Type::Meta(n) => match &self.type_solutions[*n].1 {
                None => ty.clone(),
                Some(ty) => self.walk(ty),
            },
            _ => ty.clone(),
        }
    }

    pub fn unify_type_subst(
        &mut self,
        subst: &mut Substitution,
        context: &UnifyTypeContext<usize>,
        expected: &Type<usize>,
        actual: &Type<usize>,
    ) -> Result<(), TypeError> {
        let expected = self.walk(expected);
        let actual = self.walk(actual);

        let (_, expected_kind) = self.infer_kind(&expected)?;
        let _ = self.check_kind(Some(context), &actual, &expected_kind)?;

        match expected {
            Type::App(a1, b1) => match actual {
                Type::App(a2, b2) => {
                    self.unify_type_subst(subst, context, &a1, &a2)?;
                    self.unify_type_subst(subst, context, &b1, &b2)?;
                    Ok(())
                }
                Type::Meta(n) => subst.subst_right(self, context, Type::App(a1, b1), n),
                actual => self.type_mismatch(context, Type::App(a1, b1), actual),
            },
            Type::Name(n) => match actual {
                Type::Name(nn) if n == nn => Ok(()),
                Type::Meta(nn) => subst.subst_right(self, context, Type::Name(n), nn),
                actual => self.type_mismatch(context, Type::Name(n), actual),
            },
            Type::Var(n) => match actual {
                Type::Var(nn) if n == nn => Ok(()),
                Type::Meta(nn) => subst.subst_right(self, context, Type::Var(n), nn),
                actual => self.type_mismatch(context, Type::Var(n), actual),
            },
            Type::Bool => match actual {
                Type::Bool => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::Int => match actual {
                Type::Int => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::Char => match actual {
                Type::Char => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::String => match actual {
                Type::String => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::Bytes => match actual {
                Type::Bytes => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::Array => match actual {
                Type::Array => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::Arrow => match actual {
                Type::Arrow => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::FatArrow => match actual {
                Type::FatArrow => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::Constraints(constraints1) => match actual {
                Type::Constraints(constraints2) => {
                    for (c1, c2) in constraints1.into_iter().zip(constraints2.into_iter()) {
                        if let Err(err) = self.unify_type_subst(subst, context, &c1, &c2) {
                            return Err(err);
                        }
                    }
                    Ok(())
                }
                Type::Meta(n) => {
                    subst.subst_right(self, context, Type::Constraints(constraints1), n)
                }
                actual => self.type_mismatch(context, Type::Constraints(constraints1), actual),
            },
            Type::Record => match actual {
                Type::Record => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::Variant => match actual {
                Type::Variant => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::IO => match actual {
                Type::IO => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::RowNil => match actual {
                Type::RowNil => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::RowCons(field1, ty1, rest1) => match actual {
                Type::RowCons(field2, ty2, rest2) => {
                    let expected = Type::RowCons(field1, ty1, rest1);
                    let actual = Type::RowCons(field2, ty2, rest2);
                    let (rows1, rest1) = expected.unwrap_rows();
                    let (rows2, rest2) = actual.unwrap_rows();

                    let mut rows2_remaining = Rope::from_vec(&rows2);

                    let mut sames: Vec<(&Rc<str>, &Type<usize>, &Type<usize>)> = Vec::new();
                    let mut not_in_rows2: Vec<(Rc<str>, Type<usize>)> = Vec::new();

                    for (field1, ty1) in &rows1 {
                        match rows2_remaining.iter().find(|(field2, _)| field1 == field2) {
                            None => {
                                not_in_rows2.push(((**field1).clone(), (**ty1).clone()));
                            }
                            Some((_, ty2)) => {
                                rows2_remaining =
                                    match rows2_remaining.delete_first(|(f, _)| f == field1) {
                                        Err(new) => new,
                                        Ok(new) => new,
                                    };
                                sames.push((field1, ty1, ty2));
                            }
                        }
                    }

                    // every field in rows1 that has a partner in rows2 has been deleted from rows2
                    // therefore whatever's left in rows2_remaining is necessarily not in rows_1
                    let not_in_rows1: Vec<(Rc<str>, Type<usize>)> = rows2_remaining
                        .iter()
                        .map(|(a, b)| ((**a).clone(), (**b).clone()))
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

                    let rest3 = Some(self.fresh_typevar(syntax::Kind::Row));
                    self.unify_type_subst(
                        subst,
                        context,
                        match rest1 {
                            None => &Type::RowNil,
                            Some(ty) => ty,
                        },
                        &Type::mk_rows(not_in_rows1, rest3.clone()),
                    )?;
                    self.unify_type_subst(
                        subst,
                        context,
                        &Type::mk_rows(not_in_rows2, rest3),
                        match rest2 {
                            None => &Type::RowNil,
                            Some(ty) => ty,
                        },
                    )?;

                    Ok(())
                }
                Type::Meta(n) => {
                    subst.subst_right(self, context, Type::RowCons(field1, ty1, rest1), n)
                }
                actual => self.type_mismatch(context, Type::RowCons(field1, ty1, rest1), actual),
            },
            Type::HasField(field, rest) => match actual {
                Type::HasField(field2, rest2) if field == field2 => {
                    self.unify_type_subst(subst, context, &rest, &rest2)
                }
                Type::Meta(n) => subst.subst_right(self, context, Type::HasField(field, rest), n),
                _ => self.type_mismatch(context, Type::HasField(field, rest), actual),
            },
            Type::Unit => match actual {
                Type::Unit => Ok(()),
                Type::Meta(n) => subst.subst_right(self, context, expected, n),
                _ => self.type_mismatch(context, expected, actual),
            },
            Type::Meta(n) => match actual {
                Type::Meta(nn) if n == nn => Ok(()),
                _ => subst.subst_left(self, context, n, actual),
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
        context: &UnifyTypeContext<usize>,
        expected: &Type<usize>,
        actual: &Type<usize>,
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
                    location: self.location(),
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
    ) -> (core::Pattern, Type<usize>, Vec<(Rc<str>, Type<usize>)>) {
        let arg_ty: Type<usize> = self.fresh_typevar(syntax::Kind::Type);
        let rest_row = self.fresh_typevar(syntax::Kind::Row);
        let ty = Type::mk_variant(vec![(name.clone(), arg_ty.clone())], Some(rest_row.clone()));

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
    ) -> (core::Pattern, Type<usize>, Vec<(Rc<str>, Type<usize>)>) {
        (
            core::Pattern::Wildcard,
            self.fresh_typevar(syntax::Kind::Type),
            Vec::new(),
        )
    }

    fn infer_name_pattern(
        &mut self,
        name: &Spanned<String>,
    ) -> (core::Pattern, Type<usize>, Vec<(Rc<str>, Type<usize>)>) {
        let ty = self.fresh_typevar(syntax::Kind::Type);
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
    ) -> (core::Pattern, Type<usize>, Vec<(Rc<str>, Type<usize>)>) {
        let mut names_tys: Vec<(Spanned<Rc<str>>, Type<usize>)> = names
            .iter()
            .map(|name| {
                let name = Spanned {
                    pos: name.pos,
                    item: Rc::from(name.item.as_ref()),
                };
                (name, self.fresh_typevar(syntax::Kind::Type))
            })
            .collect();
        let rest_row: Option<(Spanned<String>, Type<usize>)> = rest
            .as_ref()
            .map(|name| (name.clone(), self.fresh_typevar(syntax::Kind::Row)));
        let ty = Type::mk_record(
            names_tys
                .iter()
                .map(|(name, ty)| (name.item.clone(), ty.clone()))
                .collect(),
            rest_row.clone().map(|x| x.1),
        );

        let mut extending_row = match &rest_row {
            Some((_, row)) => row.clone(),
            None => Type::RowNil,
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
            extending_row = Type::mk_rowcons(name.item.clone(), ty.clone(), extending_row);
        }
        names_placeholders.reverse();

        rest_row.iter().for_each(|(rest_name, rest_row)| {
            names_tys.push((
                Spanned {
                    pos: rest_name.pos,
                    item: Rc::from(rest_name.item.as_ref()),
                },
                Type::mk_record(Vec::new(), Some(rest_row.clone())),
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
    ) -> (core::Pattern, Type<usize>, Vec<(Rc<str>, Type<usize>)>) {
        match arg {
            syntax::Pattern::Wildcard => self.infer_wildcard_pattern(),
            syntax::Pattern::Name(n) => self.infer_name_pattern(n),
            syntax::Pattern::Record { names, rest } => self.infer_record_pattern(names, rest),
            syntax::Pattern::Variant { name, arg } => {
                self.infer_variant_pattern(&Rc::from(name.as_ref()), arg)
            }
        }
    }

    fn check_pattern(
        &mut self,
        arg: &syntax::Pattern,
        expected: &Type<usize>,
    ) -> Result<(core::Pattern, Vec<(Rc<str>, Type<usize>)>), TypeError> {
        let (pat, actual, binds) = self.infer_pattern(arg);
        let context = UnifyTypeContext {
            expected: expected.clone(),
            actual: actual.clone(),
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
                let e_core = self.check_expr(e, Type::String)?;
                Ok(core::StringPart::Expr(e_core))
            }
        }
    }

    fn instantiate(&mut self, expr: core::Expr, sig: core::TypeSig) -> (core::Expr, Type<usize>) {
        let metas: Vec<Type<usize>> = sig
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
    ) -> Result<(core::Expr, Type<usize>), TypeError> {
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
                                location: self.location(),
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
                    let in_ty = self.fresh_typevar(syntax::Kind::Type);
                    let out_ty = self.fresh_typevar(syntax::Kind::Type);
                    let expected = Type::mk_arrow(in_ty.clone(), out_ty.clone());
                    let actual = f_ty;
                    let context = UnifyTypeContext {
                        expected: expected.clone(),
                        actual: actual.clone(),
                    };
                    self.unify_type(&context, &expected, &actual)?;
                    let x_core = self.check_expr(x, in_ty)?;
                    Ok((core::Expr::mk_app(f_core, x_core), out_ty))
                }
                syntax::Expr::Lam { args, body } => {
                    let in_ty = self.fresh_typevar(syntax::Kind::Type);
                    let out_ty = self.fresh_typevar(syntax::Kind::Type);
                    let ty = Type::mk_arrow(in_ty, out_ty);
                    let expr_core = self.check_expr(
                        &Spanned {
                            pos: expr.pos,
                            item: syntax::Expr::Lam {
                                args: args.clone(),
                                body: body.clone(),
                            },
                        },
                        ty.clone(),
                    )?;
                    Ok((expr_core, ty))
                }
                syntax::Expr::True => Ok((core::Expr::True, Type::Bool)),
                syntax::Expr::False => Ok((core::Expr::False, Type::Bool)),
                syntax::Expr::IfThenElse(cond, then_, else_) => {
                    let cond_core = self.check_expr(cond, Type::Bool)?;
                    let (then_core, then_ty) = self.infer_expr(then_)?;
                    let else_core = self.check_expr(else_, then_ty.clone())?;
                    Ok((
                        core::Expr::mk_ifthenelse(cond_core, then_core, else_core),
                        then_ty,
                    ))
                }
                syntax::Expr::Unit => Ok((core::Expr::Unit, Type::Unit)),
                syntax::Expr::Int(n) => Ok((core::Expr::Int(*n), Type::Int)),
                syntax::Expr::Char(c) => Ok((core::Expr::Char(*c), Type::Char)),
                syntax::Expr::String(parts) => {
                    let mut parts_core = Vec::new();
                    for part in parts {
                        match self.check_string_part(part) {
                            Err(err) => return Err(err),
                            Ok(part_core) => parts_core.push(part_core),
                        }
                    }
                    Ok((core::Expr::String(parts_core), Type::String))
                }
                syntax::Expr::Array(items) => {
                    let mut items_iter = items.iter();
                    match items_iter.next() {
                        Some(first) => {
                            let (first_core, first_ty) = self.infer_expr(first)?;
                            let mut items_core = vec![first_core];
                            for item in items_iter {
                                let item_core = self.check_expr(item, first_ty.clone())?;
                                items_core.push(item_core);
                            }
                            Ok((
                                core::Expr::Array(items_core),
                                Type::mk_app(Type::Array, first_ty),
                            ))
                        }
                        None => Ok((
                            core::Expr::Array(Vec::new()),
                            Type::mk_app(Type::Array, self.fresh_typevar(syntax::Kind::Type)),
                        )),
                    }
                }
                syntax::Expr::Record { fields, rest } => {
                    let mut fields_result: Vec<(Rc<str>, core::Expr, Type<usize>)> = Vec::new();
                    let mut fields_rows: Vec<(Rc<str>, Type<usize>)> = Vec::new();
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

                    let rest_row_var = self.fresh_typevar(syntax::Kind::Row);
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
                        extending_row = Type::mk_rowcons(field, expr_ty, extending_row);
                    }
                    // we did a right fold to build extending_row, but we want fields_core too look like we did a left fold
                    fields_core.reverse();

                    let mut rest_core = None;
                    let mut rest_row = None;
                    let _ = match rest {
                        None => {
                            let expected = Type::mk_record(fields_rows.clone(), None);
                            let actual =
                                Type::mk_record(fields_rows.clone(), Some(rest_row_var.clone()));
                            let context = UnifyTypeContext { expected, actual };
                            self.unify_type(&context, &Type::RowNil, &rest_row_var)
                        }
                        Some(expr) => {
                            let expr_core = self.check_expr(
                                expr,
                                Type::mk_app(Type::Record, rest_row_var.clone()),
                            )?;
                            rest_core = Some(expr_core);
                            rest_row = Some(rest_row_var);
                            Ok(())
                        }
                    }?;

                    Ok((
                        core::Expr::mk_record(fields_core, rest_core),
                        Type::mk_record(fields_rows, rest_row),
                    ))
                }
                syntax::Expr::Project(expr, field) => {
                    let field: Rc<str> = Rc::from(field.as_ref());
                    let out_ty = self.fresh_typevar(syntax::Kind::Type);
                    let rest = self.fresh_typevar(syntax::Kind::Row);
                    let rows = Type::mk_rows(vec![(field.clone(), out_ty.clone())], Some(rest));
                    let expr_core =
                        self.check_expr(expr, Type::mk_app(Type::Record, rows.clone()))?;
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

                    let arg_ty = self.fresh_typevar(syntax::Kind::Type);
                    let rest = self.fresh_typevar(syntax::Kind::Row);
                    let tag = self.evidence.placeholder(
                        None,
                        evidence::Constraint::HasField {
                            field: ctor.clone(),
                            rest: Type::mk_rows(vec![], Some(rest.clone())),
                        },
                    );
                    Ok((
                        core::Expr::mk_variant(core::Expr::Placeholder(tag)),
                        Type::mk_arrow(
                            arg_ty.clone(),
                            Type::mk_variant(vec![(ctor.clone(), arg_ty)], Some(rest)),
                        ),
                    ))
                }
                syntax::Expr::Embed(ctor, rest) => {
                    let ctor: Rc<str> = Rc::from(ctor.as_ref());

                    let arg_ty = self.fresh_typevar(syntax::Kind::Type);
                    let rest_rows = self.fresh_typevar(syntax::Kind::Row);
                    let rest_core =
                        self.check_expr(rest, Type::mk_app(Type::Variant, rest_rows.clone()))?;
                    let tag = core::Expr::Placeholder(self.evidence.placeholder(
                        None,
                        evidence::Constraint::HasField {
                            field: ctor.clone(),
                            rest: rest_rows.clone(),
                        },
                    ));
                    Ok((
                        core::Expr::mk_embed(tag, rest_core),
                        Type::mk_app(
                            Type::Variant,
                            Type::mk_rowcons(ctor.clone(), arg_ty, rest_rows),
                        ),
                    ))
                }
                syntax::Expr::Binop(op, left, right) => {
                    match op {
                        syntax::Binop::Add => {
                            let left_core = self.check_expr(left, Type::Int)?;
                            let right_core = self.check_expr(right, Type::Int)?;
                            Ok((core::Expr::mk_binop(*op, left_core, right_core), Type::Int))
                        }
                        syntax::Binop::Multiply => {
                            let left_core = self.check_expr(left, Type::Int)?;
                            let right_core = self.check_expr(right, Type::Int)?;
                            Ok((core::Expr::mk_binop(*op, left_core, right_core), Type::Int))
                        }
                        syntax::Binop::Subtract => {
                            let left_core = self.check_expr(left, Type::Int)?;
                            let right_core = self.check_expr(right, Type::Int)?;
                            Ok((core::Expr::mk_binop(*op, left_core, right_core), Type::Int))
                        }
                        syntax::Binop::Divide => {
                            let left_core = self.check_expr(left, Type::Int)?;
                            let right_core = self.check_expr(right, Type::Int)?;
                            Ok((core::Expr::mk_binop(*op, left_core, right_core), Type::Int))
                        }

                        syntax::Binop::Append => {
                            let item_ty = self.fresh_typevar(syntax::Kind::Type);
                            let expected = Type::mk_app(Type::Array, item_ty);
                            let left_core = self.check_expr(left, expected.clone())?;
                            let right_core = self.check_expr(right, expected.clone())?;
                            Ok((core::Expr::mk_binop(*op, left_core, right_core), expected))
                        }

                        syntax::Binop::Or => {
                            let left_core = self.check_expr(left, Type::Bool)?;
                            let right_core = self.check_expr(right, Type::Bool)?;
                            Ok((core::Expr::mk_binop(*op, left_core, right_core), Type::Bool))
                        }
                        syntax::Binop::And => {
                            let left_core = self.check_expr(left, Type::Bool)?;
                            let right_core = self.check_expr(right, Type::Bool)?;
                            Ok((core::Expr::mk_binop(*op, left_core, right_core), Type::Bool))
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

                    let expected_body_ty = self.fresh_typevar(syntax::Kind::Type);
                    let mut expected_pattern_ty = expr_ty.clone();
                    let mut seen_fallthrough = false;
                    let mut matching_variant = false;
                    let mut seen_ctors: HashSet<Rc<str>> = HashSet::new();
                    let mut expr_rows: Option<Type<usize>> = None;
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
                                location: self.location(),
                                pos: branch.pattern.pos,
                            });
                        }
                        let (pattern_core, _pattern_ty, pattern_binds) =
                            with_position!(self, branch.pattern.pos, {
                                let ((pattern_core, pattern_ty, pattern_binds), saw_variant) =
                                    match &branch.pattern.item {
                                        syntax::Pattern::Wildcard => {
                                            (self.infer_wildcard_pattern(), false)
                                        }
                                        syntax::Pattern::Name(n) => {
                                            (self.infer_name_pattern(n), false)
                                        }
                                        syntax::Pattern::Record { names, rest } => {
                                            (self.infer_record_pattern(names, rest), false)
                                        }
                                        syntax::Pattern::Variant { name, arg } => {
                                            let name: Rc<str> = Rc::from(name.as_ref());

                                            let arg_ty = self.fresh_typevar(syntax::Kind::Type);
                                            let rest_row = self.fresh_typevar(syntax::Kind::Row);
                                            let pattern_rows = syntax::Type::mk_rows(
                                                vec![(name.clone(), arg_ty.clone())],
                                                Some(rest_row.clone()),
                                            );
                                            let expr_rows: Type<usize> = match &expr_rows {
                                                None => {
                                                    expr_rows = Some(pattern_rows.clone());
                                                    pattern_rows.clone()
                                                }
                                                Some(rows) => rows.clone(),
                                            };

                                            let tag =
                                                core::Expr::Placeholder(self.evidence.placeholder(
                                                    None,
                                                    evidence::Constraint::HasField {
                                                        field: name.clone(),
                                                        rest: expr_rows,
                                                    },
                                                ));

                                            let pattern_ty =
                                                Type::mk_app(Type::Variant, pattern_rows);
                                            let context: UnifyTypeContext<usize> =
                                                UnifyTypeContext {
                                                    expected: expected_pattern_ty.clone(),
                                                    actual: pattern_ty.clone(),
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
                                            expected_pattern_ty =
                                                Type::mk_variant(Vec::new(), Some(rest_row));

                                            seen_ctors.insert(name);

                                            let pattern_core = core::Pattern::mk_variant(tag);
                                            let pattern_binds: Vec<(Rc<str>, Type<usize>)> =
                                                vec![(Rc::from(arg.item.as_ref()), arg_ty)];
                                            ((pattern_core, pattern_ty, pattern_binds), true)
                                        }
                                    };
                                if !saw_variant {
                                    let context: UnifyTypeContext<usize> = UnifyTypeContext {
                                        expected: expected_pattern_ty.clone(),
                                        actual: pattern_ty.clone(),
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
                        let body_core = self.check_expr(&branch.body, expected_body_ty.clone())?;
                        self.bound_vars.delete(pattern_binds.len());
                        branches_core.push(core::Branch {
                            pattern: pattern_core,
                            body: body_core,
                        });
                    }

                    if matching_variant && !seen_fallthrough {
                        let expr_ty = self.zonk_type(&expr_ty);
                        let (ctors, rest) = expr_ty.unwrap_variant().unwrap();
                        let ctors: Vec<(Rc<str>, syntax::Type<usize>)> = ctors
                            .iter()
                            .map(|(x, y)| ((*x).clone(), (*y).clone()))
                            .collect();
                        let rest: Option<Type<usize>> = rest.cloned();
                        let context = UnifyTypeContext {
                            expected: syntax::Type::mk_variant(ctors.clone(), None),
                            actual: syntax::Type::mk_variant(ctors, rest),
                        };
                        let _ = self.unify_type(
                            &context,
                            &expected_pattern_ty,
                            &Type::mk_variant(Vec::new(), None),
                        )?;
                    }

                    Ok((
                        core::Expr::mk_case(expr_core, branches_core),
                        expected_body_ty,
                    ))
                }
            }
        )
    }

    fn check_expr(
        &mut self,
        expr: &syntax::Spanned<syntax::Expr>,
        ty: Type<usize>,
    ) -> Result<core::Expr, TypeError> {
        with_position!(
            self,
            expr.pos,
            match &expr.item {
                syntax::Expr::Lam { args, body } => {
                    self.check_duplicate_args(args)?;

                    let out_ty = self.fresh_typevar(syntax::Kind::Type);
                    let mut actual = out_ty.clone();

                    let mut arg_tys = Vec::new();
                    for _ in args.iter().rev() {
                        let arg_ty = self.fresh_typevar(syntax::Kind::Type);
                        arg_tys.push(arg_ty.clone());
                        actual = Type::mk_arrow(arg_ty, actual);
                    }
                    arg_tys.reverse();

                    let context = UnifyTypeContext {
                        expected: ty.clone(),
                        actual: actual.clone(),
                    };
                    self.unify_type(&context, &ty, &actual)?;

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
                    let body_core = self.check_expr(body, out_ty)?;
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
                    let context = UnifyTypeContext {
                        expected: expected.clone(),
                        actual: actual.clone(),
                    };
                    self.unify_type(&context, &expected, &actual)?;
                    Ok(expr)
                }
            }
        )
    }
}
