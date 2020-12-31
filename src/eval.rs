use crate::core::{Builtin, EVar, Expr, StringPart};
use crate::syntax::Binop;
use std::fmt::Debug;
use std::{collections::HashMap, rc::Rc};
use typed_arena::Arena;

type ValueRef<'heap> = &'heap Value<'heap>;

#[derive(Clone)]
struct Closure2Body<'heap>(
    fn(&mut Interpreter<'heap>, Vec<ValueRef<'heap>>, ValueRef<'heap>) -> ValueRef<'heap>,
);

impl<'heap> Debug for Closure2Body<'heap> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Closure2()")
    }
}

#[derive(Debug, Clone)]
enum Value<'heap> {
    Closure {
        env: Vec<ValueRef<'heap>>,
        arg: bool,
        body: Expr,
    },
    Closure2 {
        env: Vec<ValueRef<'heap>>,
        body: Closure2Body<'heap>,
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

struct Interpreter<'heap> {
    heap: &'heap Arena<Value<'heap>>,
    context: HashMap<String, fn() -> ValueRef<'heap>>,
    bound_vars: Vec<ValueRef<'heap>>,
    evidence: Vec<ValueRef<'heap>>,
}

impl<'heap> Interpreter<'heap> {
    pub fn new(heap: &'heap Arena<Value<'heap>>) -> Self {
        let context = HashMap::new();
        let evidence = Vec::new();
        Interpreter {
            context,
            heap,
            bound_vars: Vec::new(),
            evidence,
        }
    }

    pub fn alloc_value(&self, val: Value<'heap>) -> ValueRef<'heap> {
        self.heap.alloc(val)
    }

    pub fn eval_builtin(&self, name: &Builtin) -> ValueRef<'heap> {
        match name {
            Builtin::MapIO => todo!(),
            Builtin::PureIO => todo!(),
            Builtin::Trace => {
                fn code<'heap>(
                    interpreter: &mut Interpreter<'heap>,
                    env: Vec<ValueRef<'heap>>,
                    arg: ValueRef<'heap>,
                ) -> ValueRef<'heap> {
                    fn code<'heap>(
                        _: &mut Interpreter<'heap>,
                        env: Vec<ValueRef<'heap>>,
                        arg: ValueRef<'heap>,
                    ) -> ValueRef<'heap> {
                        println!("trace: {:?}", env[0]);
                        arg
                    }
                    let closure = Value::Closure2 {
                        env: vec![arg],
                        body: Closure2Body(code),
                    };
                    interpreter.alloc_value(closure)
                }
                let closure = self.alloc_value(Value::Closure2 {
                    env: vec![],
                    body: Closure2Body(code),
                });
                closure
            }
        }
    }

    pub fn eval(&mut self, expr: Expr) -> ValueRef<'heap> {
        match expr {
            Expr::Var(ix) => self.bound_vars[self.bound_vars.len() - 1 - ix],
            Expr::Name(name) => self.context.get(&name).unwrap().clone()(),
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
                    Value::Closure2 { env, body } => body.0(self, env.clone(), b),
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
