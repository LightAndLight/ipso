use ipso_core::Name;
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Binding<'a> {
    Expr(Rc<ipso_core::Expr>),
    Module(&'a Bindings),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingInternal {
    CoreExpr(Rc<ipso_core::Expr>),
    Module(Bindings),
}

impl From<ipso_core::Binding> for BindingInternal {
    fn from(binding: ipso_core::Binding) -> Self {
        match binding {
            ipso_core::Binding::Expr(expr) => Self::CoreExpr(expr),
            ipso_core::Binding::Module(bindings) => Self::Module(Bindings::from(bindings)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bindings(HashMap<Name, BindingInternal>);

impl Bindings {
    pub fn get(&self, name: &Name) -> Option<Binding> {
        self.0.get(name).map(|binding| match binding {
            BindingInternal::CoreExpr(core_expr) => Binding::Expr(core_expr.clone()),
            BindingInternal::Module(bindings) => Binding::Module(bindings),
        })
    }
}

impl From<HashMap<Name, ipso_core::Binding>> for Bindings {
    fn from(bindings: HashMap<Name, ipso_core::Binding>) -> Self {
        Self(
            bindings
                .into_iter()
                .map(|(name, binding)| (name, BindingInternal::from(binding)))
                .collect(),
        )
    }
}
