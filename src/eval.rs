use crate::core::{Expr, Pattern};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Value {
    Closure {
        env: Vec<Value>,
        arg: Pattern,
        body: Expr,
    },
    True,
    False,
}

struct Interpreter {
    context: HashMap<String, Expr>,
    bound_vars: Vec<Value>,
}

impl Interpreter {
    fn eval(&self, expr: &Expr) -> Value {
        match expr {
            Expr::Var(ix) => self.bound_vars[self.bound_vars.len() - 1 - ix].clone(),
            Expr::Name(name) => self.eval(self.context.get(name).unwrap()),

            Expr::App(a, b) => {
                let a = self.eval(a);
                let b = self.eval(b);
                match a {
                    Value::Closure { env, arg, body } => todo!(),
                    a => panic!("expected closure, got {:?}", a),
                }
            }
            Expr::Lam { arg, body } => todo!(),

            Expr::True => todo!(),
            Expr::False => todo!(),
            Expr::IfThenElse(a, b, c) => todo!(),

            Expr::Int(u32) => todo!(),

            Expr::Binop(op, a, b) => todo!(),

            Expr::Char(char) => todo!(),

            Expr::String(parts) => todo!(),

            Expr::Array(items) => todo!(),

            Expr::Append(a, b) => todo!(),
            Expr::Record { fields, rest } => todo!(),
            Expr::Project(expr, index) => todo!(),

            Expr::Variant(tag, value) => todo!(),
            Expr::Case(expr, branches) => todo!(),
            Expr::Unit => todo!(),
        }
    }
}
