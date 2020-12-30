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
    fn eval(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Var(ix) => self.bound_vars[self.bound_vars.len() - 1 - ix].clone(),
            Expr::Name(name) => {
                let next: Expr = self.context.get(name).unwrap().clone();
                self.eval(&next)
            }

            Expr::App(a, b) => {
                let a = self.eval(a);
                let b = self.eval(b);
                match a {
                    Value::Closure { env, arg, body } => match arg {
                        Pattern::Name => todo!(),
                        Pattern::Record { names, rest } => {
                            if rest {
                                todo!()
                            } else {
                                // we can assume we're destructuring the whole record
                                // but we need to know the order in which to bind fields
                                todo!()
                            }
                        }
                        Pattern::Variant { name } => todo!("pattern matching for variant"),
                        Pattern::Wildcard => {
                            self.bound_vars = env;
                            self.eval(&body)
                        }
                    },
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
