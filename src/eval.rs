use crate::{
    builtins,
    core::{Builtin, Declaration, Expr, Pattern, StringPart},
    rope::Rope,
    syntax::Binop,
};
use std::collections::HashMap;
use std::{fmt::Debug, io::Write};
use typed_arena::Arena;

mod test;

macro_rules! function1 {
    ($self:expr, $body:expr) => {{
        fn code_0<'heap>(
            eval: &mut Interpreter<'_, 'heap>,
            env: &'heap Vec<ValueRef<'heap>>,
            arg: ValueRef<'heap>,
        ) -> ValueRef<'heap> {
            $body(eval, env, arg)
        };

        let env = $self.alloc_env(Vec::new());
        let closure = $self.alloc_value(Value::StaticClosure {
            env,
            body: StaticClosureBody(code_0),
        });
        closure
    }};
}

macro_rules! function2 {
    ($self:expr, $body:expr) => {{
        function1!($self, |eval: &mut Interpreter<'_, 'heap>,
                           env: &'heap Vec<ValueRef<'heap>>,
                           arg: ValueRef<'heap>| {
            fn code_1<'heap>(
                eval: &mut Interpreter<'_, 'heap>,
                env: &'heap Vec<ValueRef<'heap>>,
                arg: ValueRef<'heap>,
            ) -> ValueRef<'heap> {
                $body(eval, env, arg)
            }
            let env = {
                let mut env = env.clone();
                env.push(arg);
                eval.alloc_env(vec![arg])
            };
            let closure = eval.alloc_value(Value::StaticClosure {
                env,
                body: StaticClosureBody(code_1),
            });
            closure
        })
    }};
}

macro_rules! function3 {
    ($self:expr, $body:expr) => {{
        function2!($self, |eval: &mut Interpreter<'_, 'heap>,
                           env: &'heap Vec<ValueRef<'heap>>,
                           arg| {
            fn code_2<'heap>(
                eval: &mut Interpreter<'_, 'heap>,
                env: &'heap Vec<ValueRef<'heap>>,
                arg: ValueRef<'heap>,
            ) -> ValueRef<'heap> {
                $body(eval, env, arg)
            }
            let env = {
                let mut env = env.clone();
                env.push(arg);
                eval.alloc_env(env)
            };
            let closure = eval.alloc_value(Value::StaticClosure {
                env,
                body: StaticClosureBody(code_2),
            });
            closure
        })
    }};
}

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
    Stdin,
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

    pub fn unpack_array<'stdout>(&'heap self) -> Vec<ValueRef<'heap>> {
        match self {
            Value::Array(vals) => vals.clone(),
            val => panic!("expected array, got {:?}", val),
        }
    }

    pub fn unpack_bool<'stdout>(&'heap self) -> bool {
        match self {
            Value::False => false,
            Value::True => true,
            val => panic!("expected bool, got {:?}", val),
        }
    }

    pub fn unpack_int<'stdout>(&'heap self) -> u32 {
        match self {
            Value::Int(n) => *n,
            val => panic!("expected int, got {:?}", val),
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

    pub fn unpack_stdin<'stdout>(&'heap self) -> () {
        match self {
            Value::Stdin => (),
            val => panic!("expected stdin, got {:?}", val),
        }
    }

    pub fn unpack_variant(&'heap self) -> (usize, ValueRef<'heap>) {
        match self {
            Value::Variant(tag, rest) => (*tag, *rest),
            val => panic!("expected variant, got {:?}", val),
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
            Value::Stdout => String::from("Stdout"),
            Value::Stdin => String::from("Stdin"),
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
            Value::Stdin => match other {
                Value::Stdin => true,
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
}

impl<'stdout, 'heap> Interpreter<'stdout, 'heap> {
    pub fn new(
        stdout: &'stdout mut dyn Write,
        context: HashMap<String, Expr>,
        heap: &'heap Arena<Object<'heap>>,
    ) -> Self {
        Interpreter {
            stdout,
            context,
            heap,
        }
    }

    pub fn new_with_builtins(
        stdout: &'stdout mut dyn Write,
        additional_context: HashMap<String, Expr>,
        heap: &'heap Arena<Object<'heap>>,
    ) -> Self {
        let mut context: HashMap<String, Expr> = builtins::BUILTINS
            .decls
            .iter()
            .filter_map(|decl| match decl {
                Declaration::Definition { name, sig: _, body } => {
                    Some((name.clone(), body.clone()))
                }
                _ => None,
            })
            .collect();
        context.extend(additional_context);
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
                            let f = env[0];
                            let io_a = env[1];
                            let a = io_a.perform_io(interpreter); // type: a
                            f.apply(interpreter, a) // type: b
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
                            let io_a = env[0];
                            let f = env[1];
                            let a = io_a.perform_io(interpreter); // type: a
                            let io_b = f.apply(interpreter, a); // type: IO b
                            io_b.perform_io(interpreter) // type: b
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
            Builtin::Stdin => self.alloc_value(Value::Stdin),
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
            Builtin::ReadLineStdin => {
                fn read_line_stdin_0<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap Vec<ValueRef<'heap>>,
                    arg: ValueRef<'heap>, // Stdout
                ) -> ValueRef<'heap> {
                    fn read_line_stdin_1<'stdout, 'heap>(
                        interpreter: &mut Interpreter<'stdout, 'heap>,
                        env: &'heap Vec<ValueRef<'heap>>,
                    ) -> ValueRef<'heap> {
                        // env[0] : Stdin
                        let () = env[0].unpack_stdin();
                        let mut str = String::new();
                        let _ = std::io::stdin().read_line(&mut str).unwrap();
                        interpreter.alloc_value(Value::String(str))
                    }
                    let env = interpreter.alloc_env({
                        let mut env = env.clone();
                        env.push(arg);
                        env
                    });
                    let closure = interpreter.alloc_value(Value::IO {
                        env,
                        body: IOBody(read_line_stdin_1),
                    });
                    closure
                }
                let env = self.alloc_env(Vec::new());
                let closure = self.alloc_value(Value::StaticClosure {
                    env,
                    body: StaticClosureBody(read_line_stdin_0),
                });
                closure
            }
            Builtin::EqInt => {
                fn eq_int_0<'heap>(
                    eval: &mut Interpreter<'_, 'heap>,
                    _env: &'heap Vec<ValueRef<'heap>>,
                    arg: ValueRef<'heap>,
                ) -> ValueRef<'heap> {
                    fn eq_int_1<'heap>(
                        eval: &mut Interpreter<'_, 'heap>,
                        env: &'heap Vec<ValueRef<'heap>>,
                        arg: ValueRef<'heap>,
                    ) -> ValueRef<'heap> {
                        let a = env[0].unpack_int();
                        let b = arg.unpack_int();
                        if a == b {
                            eval.alloc_value(Value::True)
                        } else {
                            eval.alloc_value(Value::False)
                        }
                    }
                    let env = eval.alloc_env(vec![arg]);
                    let closure = eval.alloc_value(Value::StaticClosure {
                        env,
                        body: StaticClosureBody(eq_int_1),
                    });
                    closure
                };

                let env = self.alloc_env(Vec::new());
                let closure = self.alloc_value(Value::StaticClosure {
                    env,
                    body: StaticClosureBody(eq_int_0),
                });
                closure
            }
            Builtin::LtInt => {
                function2!(
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap Vec<ValueRef<'heap>>,
                     arg: ValueRef<'heap>| {
                        let a = env[0].unpack_int();
                        let b = arg.unpack_int();
                        if a < b {
                            eval.alloc_value(Value::True)
                        } else {
                            eval.alloc_value(Value::False)
                        }
                    }
                )
            }
            Builtin::ShowInt => {
                function1!(
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     _env: &'heap Vec<ValueRef<'heap>>,
                     arg: ValueRef<'heap>| {
                        let a = arg.unpack_int();
                        eval.alloc_value(Value::String(format!("{}", a)))
                    }
                )
            }
            Builtin::EqArray => {
                fn eq_int_0<'heap>(
                    eval: &mut Interpreter<'_, 'heap>,
                    _env: &'heap Vec<ValueRef<'heap>>,
                    arg: ValueRef<'heap>,
                ) -> ValueRef<'heap> {
                    fn eq_int_1<'heap>(
                        eval: &mut Interpreter<'_, 'heap>,
                        env: &'heap Vec<ValueRef<'heap>>,
                        arg: ValueRef<'heap>,
                    ) -> ValueRef<'heap> {
                        fn eq_int_2<'heap>(
                            eval: &mut Interpreter<'_, 'heap>,
                            env: &'heap Vec<ValueRef<'heap>>,
                            arg: ValueRef<'heap>,
                        ) -> ValueRef<'heap> {
                            let f = env[0];
                            let a = env[1].unpack_array();
                            let b = arg.unpack_array();

                            let mut acc = Value::True;
                            if a.len() == b.len() {
                                for (a, b) in a.iter().zip(b.iter()) {
                                    let res = f.apply(eval, a).apply(eval, b).unpack_bool();
                                    if !res {
                                        acc = Value::False;
                                        break;
                                    }
                                }
                            } else {
                                acc = Value::False;
                            }
                            return eval.alloc_value(acc);
                        }
                        let env = eval.alloc_env(vec![env[0], arg]);
                        let closure = eval.alloc_value(Value::StaticClosure {
                            env,
                            body: StaticClosureBody(eq_int_2),
                        });
                        closure
                    }
                    let env = eval.alloc_env(vec![arg]);
                    let closure = eval.alloc_value(Value::StaticClosure {
                        env,
                        body: StaticClosureBody(eq_int_1),
                    });
                    closure
                };

                let env = self.alloc_env(Vec::new());
                let closure = self.alloc_value(Value::StaticClosure {
                    env,
                    body: StaticClosureBody(eq_int_0),
                });
                closure
            }
            Builtin::LtArray => {
                function3!(
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap Vec<ValueRef<'heap>>,
                     arg: ValueRef<'heap>| {
                        let lt = env[0];
                        let a = env[1].unpack_array();
                        let b = arg.unpack_array();

                        let mut ix = 0;
                        let a_len = a.len();
                        let b_len = b.len();
                        loop {
                            // the prefix of a matches the prefix of b
                            if ix < a_len {
                                if ix < b_len {
                                    let a_val = a[ix];
                                    let b_val = b[ix];
                                    if lt.apply(eval, a_val).apply(eval, b_val).unpack_bool() {
                                        ix += 1;
                                    } else {
                                        return eval.alloc_value(Value::False);
                                    }
                                } else {
                                    // a is longer than b
                                    return eval.alloc_value(Value::False);
                                }
                            } else {
                                if ix < b_len {
                                    // a is shorter than b
                                    return eval.alloc_value(Value::True);
                                } else {
                                    // a is the same length as b
                                    return eval.alloc_value(Value::False);
                                }
                            }
                        }
                    }
                )
            }
            Builtin::FoldlArray => {
                function3!(
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap Vec<ValueRef<'heap>>,
                     arg: ValueRef<'heap>| {
                        let f = env[0];
                        let z = env[1];
                        let arr = arg.unpack_array();

                        let mut acc = z;
                        for el in arr {
                            acc = f.apply(eval, acc).apply(eval, el);
                        }
                        acc
                    }
                )
            }
        }
    }

    pub fn eval(&mut self, env: &'heap Vec<ValueRef<'heap>>, expr: Expr) -> ValueRef<'heap> {
        let out = match expr {
            Expr::Var(ix) => env[env.len() - 1 - ix],
            Expr::EVar(n) => panic!("found EVar({:?})", n),
            Expr::Placeholder(n) => panic!("found Placeholder({:?})", n),
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
                            let s = self.eval(env, expr).unpack_string();
                            value.push_str(s.as_str());
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
                let ix = self.eval(env, *ev).unpack_int();
                let value = self.eval(env, *value);
                let rest = self.eval(env, *rest);
                match rest {
                    Value::Record(fields) => {
                        // assume: all stacks in fields are non-empty
                        let ix = ix as usize;
                        let mut record = Vec::with_capacity(fields.len() + 1);
                        record.extend_from_slice(&fields[0..ix]);
                        record.push(value);
                        record.extend_from_slice(&fields[ix..]);

                        debug_assert!(record.len() == fields.len() + 1);

                        self.alloc_value(Value::Record(record))
                    }
                    rest => panic!("expected record, got {:?}", rest),
                }
            }
            Expr::Record(fields) => {
                let mut record: Vec<ValueRef<'heap>> = Vec::with_capacity(fields.len());
                let mut fields: Vec<(u32, ValueRef<'heap>)> = fields
                    .into_iter()
                    .map(|(ev, field)| (self.eval(env, ev).unpack_int(), self.eval(env, field)))
                    .collect();
                fields.sort_by_key(|x| x.0);
                for (_index, field) in fields.into_iter() {
                    record.push(field);
                }
                self.alloc_value(Value::Record(record))
            }
            Expr::Project(expr, index) => {
                let index = self.eval(env, *index).unpack_int();
                let expr = self.eval(env, *expr);
                match expr {
                    Value::Record(fields) => fields[index as usize],
                    expr => panic!("expected record, got {:?}", expr),
                }
            }

            Expr::Variant(tag) => {
                let tag = self.eval(env, *tag);
                let env = self.alloc_env(vec![tag]);
                fn code<'heap>(
                    interpreter: &mut Interpreter<'_, 'heap>,
                    env: &'heap Vec<ValueRef<'heap>>,
                    arg: ValueRef<'heap>,
                ) -> ValueRef<'heap> {
                    let tag = env[0].unpack_int() as usize;
                    interpreter.alloc_value(Value::Variant(tag, arg))
                }
                let closure = Value::StaticClosure {
                    env,
                    body: StaticClosureBody(code),
                };
                self.alloc_value(closure)
            }
            Expr::Embed(tag, rest) => {
                let tag = self.eval(env, *tag).unpack_int() as usize;
                let rest = self.eval(env, *rest);
                let (old_tag, arg) = rest.unpack_variant();
                self.alloc_value(Value::Variant(
                    if tag <= old_tag { old_tag + 1 } else { old_tag },
                    arg,
                ))
            }
            Expr::Case(expr, branches) => {
                let expr = self.eval(env, *expr);
                match expr {
                    Value::Record(fields) => {
                        // expect a record pattern
                        debug_assert!(branches.len() == 1);
                        let branch = &branches[0];
                        match &branch.pattern {
                            Pattern::Record { names, rest } => {
                                let mut new_env = env.clone();
                                let mut extracted = Vec::new();
                                for name in names {
                                    let ix = self.eval(env, name.clone()).unpack_int() as usize;
                                    new_env.push(fields[ix]);
                                    extracted.push(ix);
                                }
                                if *rest {
                                    let mut leftover_fields = Rope::from_vec(fields);
                                    extracted.sort();
                                    for ix in extracted.iter().rev() {
                                        leftover_fields = leftover_fields.delete(*ix).unwrap();
                                    }
                                    let leftover_fields: Vec<&Value> =
                                        leftover_fields.iter().map(|x| *x).collect();
                                    let leftover_record =
                                        self.alloc_value(Value::Record(leftover_fields));
                                    new_env.push(leftover_record);
                                }

                                let new_env = self.alloc_env(new_env);
                                self.eval(new_env, branch.body.clone())
                            }
                            pattern => panic!("expected record pattern, got {:?}", pattern),
                        }
                    }
                    Value::Variant(tag, value) => {
                        // expect variant patterns
                        let mut target: Option<(ValueRef, Expr)> = None;
                        for branch in branches {
                            match branch.pattern {
                                Pattern::Record { .. } => {
                                    panic!("expected variant pattern, got {:?}", branch.pattern)
                                }
                                Pattern::Variant { tag: branch_tag } => {
                                    let branch_tag =
                                        self.eval(env, *branch_tag).unpack_int() as usize;
                                    if *tag == branch_tag {
                                        target = Some((value, branch.body));
                                        break;
                                    }
                                }
                                Pattern::Name | Pattern::Wildcard => {
                                    target = Some((expr, branch.body));
                                    break;
                                }
                            }
                        }
                        match target {
                            None => panic!("pattern match failure"),
                            Some((value, body)) => {
                                let env = self.alloc_env({
                                    let mut env = env.clone();
                                    env.push(value);
                                    env
                                });
                                self.eval(env, body)
                            }
                        }
                    }
                    _ => {
                        // expect a name or wildcard pattern
                        debug_assert!(branches.len() == 1);
                        todo!("case of name/wildcard")
                    }
                }
            }
            Expr::Unit => self.alloc_value(Value::Unit),
        };
        out
    }
}
