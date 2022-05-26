use crate::closure_conversion;
use ipso_core::Name;
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Eq)]
pub enum Binding<'a> {
    Expr(Rc<closure_conversion::Expr>),
    Module(&'a mut Bindings),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Core(Rc<ipso_core::Expr>),
    Closure(Rc<closure_conversion::Expr>),
}

fn get_closure_converted(expr: &mut Expr) -> Rc<closure_conversion::Expr> {
    match expr {
        Expr::Core(core) => {
            let converted = Rc::new(closure_conversion::convert(core.as_ref()));
            *expr = Expr::Closure(converted.clone());
            converted
        }
        Expr::Closure(expr) => expr.clone(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum BindingInternal {
    Expr(Expr),
    Module(Bindings),
}

impl From<ipso_core::Binding> for BindingInternal {
    fn from(binding: ipso_core::Binding) -> Self {
        match binding {
            ipso_core::Binding::Expr(expr) => Self::Expr(Expr::Core(expr)),
            ipso_core::Binding::Module(bindings) => Self::Module(Bindings::from(bindings)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bindings(HashMap<Name, BindingInternal>);

impl Bindings {
    pub fn get(&mut self, name: &Name) -> Option<Binding> {
        self.0.get_mut(name).map(|binding| match binding {
            BindingInternal::Expr(expr) => Binding::Expr(get_closure_converted(expr)),
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
