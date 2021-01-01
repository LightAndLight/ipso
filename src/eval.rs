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
pub struct StaticClosureBody<'heap>(
    fn(
        &mut Interpreter<'_, 'heap>,
        &'heap Vec<ValueRef<'heap>>,
        ValueRef<'heap>,
    ) -> ValueRef<'heap>,
);

impl<'heap> Debug for StaticClosureBody<'heap> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("StaticClosure()")
    }
}

#[derive(Clone)]
pub struct IOBody<'heap>(
    fn(&mut Interpreter<'_, 'heap>, &'heap Vec<ValueRef<'heap>>) -> ValueRef<'heap>,
);

impl<'heap> Debug for IOBody<'heap> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("IO()")
    }
}

#[derive(Debug, Clone)]
pub enum Value<'heap> {
    Closure {
        env: &'heap Vec<ValueRef<'heap>>,
        arg: bool,
        body: Expr,
    },
    StaticClosure {
        env: &'heap Vec<ValueRef<'heap>>,
        body: StaticClosureBody<'heap>,
    },
    IO {
        env: &'heap Vec<ValueRef<'heap>>,
        body: IOBody<'heap>,
    },
    True,
    False,
    Int(u32),
    Char(char),
    String(String),
    Bytes(&'heap [u8]),
    Array(Vec<ValueRef<'heap>>),
    Record(Vec<ValueRef<'heap>>),
    Variant(usize, ValueRef<'heap>),
    Unit,

    Stdout,
}

impl<'heap> Value<'heap> {
    pub fn perform_io<'stdout>(
        &self,
        interpreter: &mut Interpreter<'stdout, 'heap>,
    ) -> ValueRef<'heap> {
        match self {
            Value::IO { env, body } => body.0(interpreter, env),
            val => panic!("expected io, got {:?}", val),
        }
    }

    pub fn unpack_string<'stdout>(&'heap self) -> &'heap String {
        match self {
            Value::String(str) => str,
            val => panic!("expected string, got {:?}", val),
        }
    }

    pub fn unpack_bytes<'stdout>(&'heap self) -> &'heap [u8] {
        match self {
            Value::Bytes(bs) => bs,
            val => panic!("expected bytes, got {:?}", val),
        }
    }

    pub fn unpack_stdout<'stdout>(&'heap self) -> () {
        match self {
            Value::Stdout => (),
            val => panic!("expected stdout, got {:?}", val),
        }
    }

    pub fn apply<'stdout>(
        &self,
        interpreter: &mut Interpreter<'stdout, 'heap>,
        arg: ValueRef<'heap>,
    ) -> ValueRef<'heap> {
        match self {
            Value::Closure {
                env,
                arg: use_arg,
                body,
            } => {
                let env = {
                    let mut env: Vec<&Value> = (*env).clone();
                    if *use_arg {
                        env.push(arg);
                    }
                    interpreter.alloc_env(env)
                };
                interpreter.eval(env, body.clone())
            }
            Value::StaticClosure { env, body } => body.0(interpreter, env, arg),
            a => panic!("expected closure, got {:?}", a),
        }
    }

    pub fn render(&self) -> String {
        match self {
            Value::Closure {
                env: _,
                arg: _,
                body: _,
            } => String::from("<closure>"),
            Value::StaticClosure { env: _, body: _ } => String::from("<static builtin>"),
            Value::IO { env: _, body: _ } => String::from("<io>"),
            Value::True => String::from("true"),
            Value::False => String::from("false"),
            Value::Int(n) => String::from(format!("{:?}", n)),
            Value::Char(c) => String::from(format!("{:?}", c)),
            Value::String(s) => String::from(format!("{:?}", s)),
            Value::Bytes(bs) => String::from(format!("{:?}", bs)),
            Value::Stdout => String::from("Stdout"),
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
                } => env == env2 && (body.0 as usize) == (body2.0 as usize),
                _ => false,
            },
            Value::IO { env, body } => match other {
                Value::IO {
                    env: env2,
                    body: body2,
                } => env == env2 && (body.0 as usize) == (body2.0 as usize),
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
            Value::Bytes(bs) => match other {
                Value::Bytes(bs2) => bs == bs2,
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
            Value::Stdout => match other {
                Value::Stdout => true,
                _ => false,
            },
        }
    }
}

pub enum Object<'heap> {
    Value(Value<'heap>),
    Env(Vec<ValueRef<'heap>>),
}

pub struct Interpreter<'stdout, 'heap> {
    stdout: &'stdout mut dyn Write,
    heap: &'heap Arena<Object<'heap>>,
    context: HashMap<String, Expr>,
    evidence: Vec<ValueRef<'heap>>,
}

impl<'stdout, 'heap> Interpreter<'stdout, 'heap> {
    pub fn new(
        stdout: &'stdout mut dyn Write,
        context: HashMap<String, Expr>,
        heap: &'heap Arena<Object<'heap>>,
    ) -> Self {
        let evidence = Vec::new();
        Interpreter {
            stdout,
            context,
            heap,
            evidence,
        }
    }

    pub fn new_with_builtins(
        stdout: &'stdout mut dyn Write,
        heap: &'heap Arena<Object<'heap>>,
    ) -> Self {
        let context = builtins::BUILTINS
            .decls
            .iter()
            .filter_map(|decl| match decl {
                Declaration::Definition { name, sig: _, body } => {
                    Some((name.clone(), body.clone()))
                }
                _ => None,
            })
            .collect();
        Self::new(stdout, context, heap)
    }

    pub fn alloc_value(&self, val: Value<'heap>) -> ValueRef<'heap> {
        match self.heap.alloc(Object::Value(val)) {
            Object::Value(val) => val,
            _ => panic!("impossible"),
        }
    }

    pub fn alloc_env(&self, env: Vec<ValueRef<'heap>>) -> &'heap Vec<ValueRef<'heap>> {
        match self.heap.alloc(Object::Env(env)) {
            Object::Env(env) => env,
            _ => panic!("impossible"),
        }
    }

    pub fn eval_builtin(&self, name: &Builtin) -> ValueRef<'heap> {
        match name {
            Builtin::PureIO => {
                fn pure_io_0<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap Vec<ValueRef<'heap>>,
                    arg: ValueRef<'heap>,
                ) -> ValueRef<'heap> {
                    fn pure_io_1<'stdout, 'heap>(
                        _: &mut Interpreter<'stdout, 'heap>,
                        env: &'heap Vec<ValueRef<'heap>>,
                    ) -> ValueRef<'heap> {
                        env[0]
                    }
                    let env: &Vec<&Value> = interpreter.alloc_env({
                        let mut env = env.clone();
                        env.push(arg);
                        env
                    });
                    let closure = Value::IO {
                        env,
                        body: IOBody(pure_io_1),
                    };
                    interpreter.alloc_value(closure)
                }
                let env = self.alloc_env(Vec::new());
                let closure = Value::StaticClosure {
                    env,
                    body: StaticClosureBody(pure_io_0),
                };
                self.alloc_value(closure)
            }
            Builtin::MapIO => {
                fn map_io_0<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap Vec<ValueRef<'heap>>,
                    arg: ValueRef<'heap>, // a -> b
                ) -> ValueRef<'heap> {
                    fn map_io_1<'stdout, 'heap>(
                        interpreter: &mut Interpreter<'stdout, 'heap>,
                        env: &'heap Vec<ValueRef<'heap>>,
                        arg: ValueRef<'heap>, // IO a
                    ) -> ValueRef<'heap> {
                        fn map_io_2<'stdout, 'heap>(
                            interpreter: &mut Interpreter<'stdout, 'heap>,
                            env: &'heap Vec<ValueRef<'heap>>,
                        ) -> ValueRef<'heap> {
                            // env[0] : a -> b
                            // env[1] : IO a
                            let b = env[1].perform_io(interpreter); // type: a
                            env[0].apply(interpreter, b) // type: b
                        }
                        let env = interpreter.alloc_env({
                            let mut env = env.clone();
                            env.push(arg);
                            env
                        });
                        let closure = Value::IO {
                            env,
                            body: IOBody(map_io_2),
                        };
                        interpreter.alloc_value(closure)
                    }
                    let env = interpreter.alloc_env({
                        let mut env = env.clone();
                        env.push(arg);
                        env
                    });
                    let closure = Value::StaticClosure {
                        env,
                        body: StaticClosureBody(map_io_1),
                    };
                    interpreter.alloc_value(closure)
                }
                let env = self.alloc_env(Vec::new());
                let closure = Value::StaticClosure {
                    env,
                    body: StaticClosureBody(map_io_0),
                };
                self.alloc_value(closure)
            }
            Builtin::BindIO => {
                fn bind_io_0<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap Vec<ValueRef<'heap>>,
                    arg: ValueRef<'heap>, // IO a
                ) -> ValueRef<'heap> {
                    fn bind_io_1<'stdout, 'heap>(
                        interpreter: &mut Interpreter<'stdout, 'heap>,
                        env: &'heap Vec<ValueRef<'heap>>,
                        arg: ValueRef<'heap>, // a -> IO b
                    ) -> ValueRef<'heap> {
                        fn bind_io_2<'stdout, 'heap>(
                            interpreter: &mut Interpreter<'stdout, 'heap>,
                            env: &'heap Vec<ValueRef<'heap>>,
                        ) -> ValueRef<'heap> {
                            // env[0] : IO a
                            // env[1] : a -> IO b
                            let a = env[0].perform_io(interpreter); // type: a
                            let b = env[1].apply(interpreter, a); // type: IO b
                            b.perform_io(interpreter) // type: b
                        }
                        let env = interpreter.alloc_env({
                            let mut env = env.clone();
                            env.push(arg);
                            env
                        });
                        let closure = Value::IO {
                            env,
                            body: IOBody(bind_io_2),
                        };
                        interpreter.alloc_value(closure)
                    }
                    let env = interpreter.alloc_env({
                        let mut env = env.clone();
                        env.push(arg);
                        env
                    });
                    let closure = Value::StaticClosure {
                        env,
                        body: StaticClosureBody(bind_io_1),
                    };
                    interpreter.alloc_value(closure)
                }
                let env = self.alloc_env(Vec::new());
                let closure = Value::StaticClosure {
                    env,
                    body: StaticClosureBody(bind_io_0),
                };
                self.alloc_value(closure)
            }
            Builtin::Trace => {
                fn code_outer<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap Vec<ValueRef<'heap>>,
                    arg: ValueRef<'heap>,
                ) -> ValueRef<'heap> {
                    fn code_inner<'stdout, 'heap>(
                        interpreter: &mut Interpreter<'stdout, 'heap>,
                        env: &'heap Vec<ValueRef<'heap>>,
                        arg: ValueRef<'heap>,
                    ) -> ValueRef<'heap> {
                        let _ = writeln!(interpreter.stdout, "trace: {}", env[0].render()).unwrap();
                        arg
                    }
                    let env = interpreter.alloc_env({
                        let mut env = env.clone();
                        env.push(arg);
                        env
                    });
                    let closure = Value::StaticClosure {
                        env,
                        body: StaticClosureBody(code_inner),
                    };
                    interpreter.alloc_value(closure)
                }
                let env = self.alloc_env(Vec::new());
                let closure = self.alloc_value(Value::StaticClosure {
                    env,
                    body: StaticClosureBody(code_outer),
                });
                closure
            }
            Builtin::ToUtf8 => {
                fn to_utf8_0<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    _env: &'heap Vec<ValueRef<'heap>>,
                    arg: ValueRef<'heap>,
                ) -> ValueRef<'heap> {
                    let a = arg.unpack_string();
                    interpreter.alloc_value(Value::Bytes(a.as_bytes()))
                }
                let env = self.alloc_env(Vec::new());
                let closure = self.alloc_value(Value::StaticClosure {
                    env,
                    body: StaticClosureBody(to_utf8_0),
                });
                closure
            }
            Builtin::Stdout => self.alloc_value(Value::Stdout),
            Builtin::WriteStdout => {
                fn write_stdout_0<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap Vec<ValueRef<'heap>>,
                    arg: ValueRef<'heap>, // Stdout
                ) -> ValueRef<'heap> {
                    fn write_stdout_1<'stdout, 'heap>(
                        interpreter: &mut Interpreter<'stdout, 'heap>,
                        env: &'heap Vec<ValueRef<'heap>>,
                        arg: ValueRef<'heap>, // Bytes
                    ) -> ValueRef<'heap> {
                        fn write_stdout_2<'stdout, 'heap>(
                            interpreter: &mut Interpreter<'stdout, 'heap>,
                            env: &'heap Vec<ValueRef<'heap>>,
                        ) -> ValueRef<'heap> {
                            // env[0] : Stdout
                            // env[1] : Bytes
                            let () = env[0].unpack_stdout();
                            let bs = env[1].unpack_bytes();
                            let _ = std::io::stdout().write_all(bs).unwrap();
                            interpreter.alloc_value(Value::Unit)
                        }

                        let env = interpreter.alloc_env({
                            let mut env = env.clone();
                            env.push(arg);
                            env
                        });
                        interpreter.alloc_value(Value::IO {
                            env,
                            body: IOBody(write_stdout_2),
                        })
                    }
                    let env = interpreter.alloc_env({
                        let mut env = env.clone();
                        env.push(arg);
                        env
                    });
                    let closure = interpreter.alloc_value(Value::StaticClosure {
                        env,
                        body: StaticClosureBody(write_stdout_1),
                    });
                    closure
                }
                let env = self.alloc_env(Vec::new());
                let closure = self.alloc_value(Value::StaticClosure {
                    env,
                    body: StaticClosureBody(write_stdout_0),
                });
                closure
            }
        }
    }

    pub fn eval(&mut self, env: &'heap Vec<ValueRef<'heap>>, expr: Expr) -> ValueRef<'heap> {
        match expr {
            Expr::Var(ix) => env[env.len() - 1 - ix],
            Expr::Name(name) => self.eval(env, self.context.get(&name).unwrap().clone()),
            Expr::Builtin(name) => self.eval_builtin(&name),

            Expr::App(a, b) => {
                let a = self.eval(env, *a);
                let b = self.eval(env, *b);
                a.apply(self, b)
            }
            Expr::Lam { arg, body } => self.alloc_value(Value::Closure {
                env,
                arg,
                body: *body,
            }),

            Expr::True => self.alloc_value(Value::True),
            Expr::False => self.alloc_value(Value::False),
            Expr::IfThenElse(cond, t, e) => {
                let cond = self.eval(env, *cond);
                match cond {
                    Value::True => self.eval(env, *t),
                    Value::False => self.eval(env, *e),
                    cond => panic!("expected bool, got {:?}", cond),
                }
            }

            Expr::Int(n) => self.alloc_value(Value::Int(n)),

            Expr::Binop(op, a, b) => {
                let a = self.eval(env, *a);
                let b = self.eval(env, *b);
                match op {
                    Binop::Add => todo!("eval add {:?} {:?}", a, b),
                    Binop::Multiply => todo!("eval multiply {:?} {:?}", a, b),
                    Binop::Subtract => todo!("eval subtract {:?} {:?}", a, b),
                    Binop::Divide => todo!("eval divide {:?} {:?}", a, b),
                    Binop::Append => todo!("eval append {:?} {:?}", a, b),
                    Binop::Or => todo!("eval or {:?} {:?}", a, b),
                    Binop::And => todo!("eval and {:?} {:?}", a, b),
                    Binop::Eq => todo!("eval eq {:?} {:?}", a, b),
                    Binop::Neq => todo!("eval neq {:?} {:?}", a, b),
                    Binop::Gt => todo!("eval gt {:?} {:?}", a, b),
                    Binop::Gte => todo!("eval gte {:?} {:?}", a, b),
                    Binop::Lt => todo!("eval lt {:?} {:?}", a, b),
                    Binop::Lte => todo!("eval lte {:?} {:?}", a, b),
                }
            }

            Expr::Char(c) => self.alloc_value(Value::Char(c)),

            Expr::String(parts) => {
                let mut value = String::new();
                for part in parts {
                    match part {
                        StringPart::Expr(expr) => {
                            let expr = self.eval(env, expr);
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
                let items = items.into_iter().map(|item| self.eval(env, item)).collect();
                self.alloc_value(Value::Array(items))
            }

            Expr::Extend(ev, value, rest) => {
                let value = self.eval(env, *value);
                let rest = self.eval(env, *rest);
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
                    .map(|(ev, field)| (ev, self.eval(env, field)))
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
                let expr = self.eval(env, *expr);
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
                let value: ValueRef<'heap> = self.eval(env, *value);
                self.alloc_value(Value::Variant(tag, value))
            }
            Expr::Case(expr, branches) => todo!("eval case {:?} {:?}", expr, branches),
            Expr::Unit => self.alloc_value(Value::Unit),
        }
    }
}
