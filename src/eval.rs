use crate::syntax::Binop;
use crate::{
    builtins,
    core::{Builtin, Declaration, EVar, Expr, StringPart},
};
use std::collections::HashMap;
use std::{fmt::Debug, io::Write};
use typed_arena::Arena;

mod test;

type ValueRef<'heap> = &'heap Value<'heap>;

#[derive(Clone)]
struct StaticClosureBody<'heap>(
    fn(&mut Interpreter<'_, 'heap>, Vec<ValueRef<'heap>>, ValueRef<'heap>) -> ValueRef<'heap>,
);

impl<'heap> Debug for StaticClosureBody<'heap> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("StaticClosure()")
    }
}

#[derive(Debug, Clone)]
pub enum Value<'heap> {
    Closure {
        env: Vec<ValueRef<'heap>>,
        arg: bool,
        body: Expr,
    },
    StaticClosure {
        env: Vec<ValueRef<'heap>>,
        body: StaticClosureBody<'heap>,
    },
    True,
    False,
    Int(u32),
    Char(char),
    String(String),
    Array(Vec<ValueRef<'heap>>),
    Record(Vec<ValueRef<'heap>>),
    Variant(usize, ValueRef<'heap>),
    Unit,
}

impl<'heap> Value<'heap> {
    pub fn render(&self) -> String {
        match self {
            Value::Closure { env, arg, body } => String::from("<closure>"),
            Value::StaticClosure { env, body } => String::from("<static builtin>"),
            Value::True => String::from("true"),
            Value::False => String::from("false"),
            Value::Int(n) => String::from(format!("{:?}", n)),
            Value::Char(c) => String::from(format!("{:?}", c)),
            Value::String(s) => String::from(format!("{:?}", s)),
            Value::Array(items) => {
                let mut s = String::new();
                s.push_str("[ ");
                let mut items_iter = items.iter();
                match items_iter.next() {
                    None => {}
                    Some(item) => {
                        s.push_str(item.render().as_str());
                        for item in items_iter {
                            s.push_str(", ");
                            s.push_str(item.render().as_str());
                        }
                    }
                }
                s.push_str(" ]");
                s
            }
            Value::Record(items) => {
                let mut s = String::new();
                s.push_str("{ ");
                let mut items_iter = items.iter();
                match items_iter.next() {
                    None => {}
                    Some(item) => {
                        s.push_str(item.render().as_str());
                        for item in items_iter {
                            s.push_str(", ");
                            s.push_str(item.render().as_str());
                        }
                    }
                }
                s.push_str(" }");
                s
            }
            Value::Variant(tag, value) => {
                let mut s = String::new();
                s.push_str("ctor(");
                s.push_str(format!("{:?}", tag).as_str());
                s.push_str(", ");
                s.push_str(value.render().as_str());
                s.push(')');
                s
            }
            Value::Unit => String::from("()"),
        }
    }
}

impl<'heap> PartialEq for Value<'heap> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::Closure { env, arg, body } => match other {
                Value::Closure {
                    env: env2,
                    arg: arg2,
                    body: body2,
                } => env == env2 && arg == arg2 && body == body2,
                _ => false,
            },
            Value::StaticClosure { env, body } => match other {
                Value::StaticClosure {
                    env: env2,
                    body: body2,
                } => env == env2 && std::ptr::eq(body, body2),
                _ => false,
            },
            Value::True => match other {
                Value::True => true,
                _ => false,
            },
            Value::False => match other {
                Value::False => true,
                _ => false,
            },
            Value::Int(n) => match other {
                Value::Int(n2) => n == n2,
                _ => false,
            },
            Value::Char(c) => match other {
                Value::Char(c2) => c == c2,
                _ => false,
            },
            Value::String(s) => match other {
                Value::String(s2) => s == s2,
                _ => false,
            },
            Value::Array(items) => match other {
                Value::Array(items2) => items == items2,
                _ => false,
            },
            Value::Record(items) => match other {
                Value::Record(items2) => items == items2,
                _ => false,
            },
            Value::Variant(tag, value) => match other {
                Value::Variant(tag2, value2) => tag == tag2 && value == value2,
                _ => false,
            },
            Value::Unit => match other {
                Value::Unit => true,
                _ => false,
            },
        }
    }
}

pub struct Interpreter<'stdout, 'heap> {
    stdout: &'stdout mut dyn Write,
    heap: &'heap Arena<Value<'heap>>,
    context: HashMap<String, Expr>,
    bound_vars: Vec<ValueRef<'heap>>,
    evidence: Vec<ValueRef<'heap>>,
}

impl<'stdout, 'heap> Interpreter<'stdout, 'heap> {
    pub fn new(
        stdout: &'stdout mut dyn Write,
        context: HashMap<String, Expr>,
        heap: &'heap Arena<Value<'heap>>,
    ) -> Self {
        let evidence = Vec::new();
        Interpreter {
            stdout,
            context,
            heap,
            bound_vars: Vec::new(),
            evidence,
        }
    }

    pub fn new_with_builtins(
        stdout: &'stdout mut dyn Write,
        heap: &'heap Arena<Value<'heap>>,
    ) -> Self {
        let context = builtins::BUILTINS
            .decls
            .iter()
            .filter_map(|decl| match decl {
                Declaration::Definition { name, sig, body } => Some((name.clone(), body.clone())),
                _ => None,
            })
            .collect();
        Self::new(stdout, context, heap)
    }

    pub fn alloc_value(&self, val: Value<'heap>) -> ValueRef<'heap> {
        self.heap.alloc(val)
    }

    pub fn eval_builtin(&self, name: &Builtin) -> ValueRef<'heap> {
        match name {
            Builtin::MapIO => todo!(),
            Builtin::PureIO => todo!(),
            Builtin::Trace => {
                fn code_outer<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    _: Vec<ValueRef<'heap>>,
                    arg: ValueRef<'heap>,
                ) -> ValueRef<'heap> {
                    fn code_inner<'stdout, 'heap>(
                        interpreter: &mut Interpreter<'stdout, 'heap>,
                        env: Vec<ValueRef<'heap>>,
                        arg: ValueRef<'heap>,
                    ) -> ValueRef<'heap> {
                        let _ = writeln!(interpreter.stdout, "trace: {}", env[0].render()).unwrap();
                        arg
                    }
                    let closure = Value::StaticClosure {
                        env: vec![arg],
                        body: StaticClosureBody(code_inner),
                    };
                    interpreter.alloc_value(closure)
                }
                let closure = self.alloc_value(Value::StaticClosure {
                    env: vec![],
                    body: StaticClosureBody(code_outer),
                });
                closure
            }
        }
    }

    pub fn eval(&mut self, expr: Expr) -> ValueRef<'heap> {
        match expr {
            Expr::Var(ix) => self.bound_vars[self.bound_vars.len() - 1 - ix],
            Expr::Name(name) => self.eval(self.context.get(&name).unwrap().clone()),
            Expr::Builtin(name) => self.eval_builtin(&name),

            Expr::App(a, b) => {
                let a = self.eval(*a);
                let b = self.eval(*b);
                match a {
                    Value::Closure { env, arg, body } => {
                        self.bound_vars = env.clone();
                        if *arg {
                            self.bound_vars.push(b)
                        }
                        self.eval(body.clone())
                    }
                    Value::StaticClosure { env, body } => body.0(self, env.clone(), b),
                    a => panic!("expected closure, got {:?}", a),
                }
            }
            Expr::Lam { arg, body } => self.alloc_value(Value::Closure {
                env: self.bound_vars.clone(),
                arg,
                body: *body,
            }),

            Expr::True => self.alloc_value(Value::True),
            Expr::False => self.alloc_value(Value::False),
            Expr::IfThenElse(cond, t, e) => {
                let cond = self.eval(*cond);
                match cond {
                    Value::True => self.eval(*t),
                    Value::False => self.eval(*e),
                    cond => panic!("expected bool, got {:?}", cond),
                }
            }

            Expr::Int(n) => self.alloc_value(Value::Int(n)),

            Expr::Binop(op, a, b) => {
                let a = self.eval(*a);
                let b = self.eval(*b);
                match op {
                    Binop::Add => todo!(),
                    Binop::Multiply => todo!(),
                    Binop::Subtract => todo!(),
                    Binop::Divide => todo!(),
                    Binop::Append => todo!(),
                    Binop::Or => todo!(),
                    Binop::And => todo!(),
                    Binop::Eq => todo!(),
                    Binop::Neq => todo!(),
                    Binop::Gt => todo!(),
                    Binop::Gte => todo!(),
                    Binop::Lt => todo!(),
                    Binop::Lte => todo!(),
                }
            }

            Expr::Char(c) => self.alloc_value(Value::Char(c)),

            Expr::String(parts) => {
                let mut value = String::new();
                for part in parts {
                    match part {
                        StringPart::Expr(expr) => {
                            let expr = self.eval(expr);
                            match expr {
                                Value::String(s) => value.push_str(s.as_str()),
                                expr => panic!("expected string, got {:?}", expr),
                            }
                        }
                        StringPart::String(s) => value.push_str(s.as_str()),
                    }
                }
                self.alloc_value(Value::String(value))
            }

            Expr::Array(items) => {
                let items = items.into_iter().map(|item| self.eval(item)).collect();
                self.alloc_value(Value::Array(items))
            }

            Expr::Extend(ev, value, rest) => {
                let value = self.eval(*value);
                let rest = self.eval(*rest);
                match rest {
                    Value::Record(fields) => match &self.evidence[ev.0] {
                        Value::Int(ix) => {
                            // assume: all stacks in fields are non-empty
                            let ix = *ix as usize;
                            let mut record = Vec::with_capacity(fields.len() + 1);
                            record.extend_from_slice(&fields[0..ix]);
                            record.push(value);
                            record.extend_from_slice(&fields[ix..]);

                            debug_assert!(record.len() == fields.len() + 1);

                            self.alloc_value(Value::Record(record))
                        }
                        evidence => panic!("expected int, got {:?}", evidence),
                    },
                    rest => panic!("expected record, got {:?}", rest),
                }
            }
            Expr::Record(fields) => {
                let mut record: Vec<ValueRef<'heap>> = Vec::with_capacity(fields.len());
                let fields: Vec<(EVar, ValueRef<'heap>)> = fields
                    .into_iter()
                    .map(|(ev, field)| (ev, self.eval(field)))
                    .collect();
                for (ev, field) in fields.into_iter().rev() {
                    let index = match &self.evidence[ev.0] {
                        Value::Int(ix) => *ix as usize,
                        evidence => panic!("expected int, got {:?}", evidence),
                    };
                    record.insert(index, field);
                }
                self.alloc_value(Value::Record(record))
            }
            Expr::Project(expr, index) => {
                let expr = self.eval(*expr);
                match expr {
                    Value::Record(fields) => match &self.evidence[index.0] {
                        Value::Int(ix) => fields[*ix as usize],
                        evidence => panic!("expected int, got {:?}", evidence),
                    },
                    expr => panic!("expected record, got {:?}", expr),
                }
            }

            Expr::Variant(tag, value) => {
                let tag = match &self.evidence[tag.0] {
                    Value::Int(tag) => *tag as usize,
                    evidence => panic!("expected int, got {:?}", evidence),
                };
                let value: ValueRef<'heap> = self.eval(*value);
                self.alloc_value(Value::Variant(tag, value))
            }
            Expr::Case(expr, branches) => todo!("eval case"),
            Expr::Unit => self.alloc_value(Value::Unit),
        }
    }
}
