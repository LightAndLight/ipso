use crate::{
    core::{self, Builtin, Declaration, Expr, ModuleUsage, Pattern, StringPart},
    import::ModulePath,
    rope::Rope,
    syntax::{Binop, ModuleName},
};
use paste::paste;
use std::{
    collections::HashMap,
    fmt::Debug,
    io::{BufRead, Write},
    rc::Rc,
};
use typed_arena::Arena;

mod test;

macro_rules! function1 {
    ($name:ident, $self:expr, $body:expr) => {{
        paste! {
            fn [<$name _code_0>]<'heap>(
                eval: &mut Interpreter<'_, 'heap>,
                env: &'heap [Value<'heap>],
                arg: Value<'heap>,
            ) -> Value<'heap> {
                #[allow(clippy::redundant_closure_call)]
                $body(eval, env, arg)
            }

            let closure = $self.alloc(Object::StaticClosure {
                env: &[],
                body: StaticClosureBody([<$name _code_0>]),
            });
            closure
        }
    }};
}

macro_rules! function2 {
    ($name:ident, $self:expr, $body:expr) => {{
        function1!(
            $name,
            $self,
            (|eval: &mut Interpreter<'_, 'heap>, env: &'heap [Value<'heap>], arg: Value<'heap>| {
                paste! {
                    fn [<$name _code_1>]<'heap>(
                        eval: &mut Interpreter<'_, 'heap>,
                        env: &'heap [Value<'heap>],
                        arg: Value<'heap>,
                    ) -> Value<'heap> {
                        $body(eval, env, arg)
                    }
                    let env = eval.alloc_values({
                        let mut env = Vec::from(env);
                        env.push(arg);
                        env
                    });
                    eval.alloc(Object::StaticClosure {
                        env,
                        body: StaticClosureBody([<$name _code_1>]),
                    })
                }
            })
        )
    }};
}

macro_rules! function3 {
    ($name:ident, $self:expr, $body:expr) => {{
        function2!(
            $name,
            $self,
            (|eval: &mut Interpreter<'_, 'heap>, env: &'heap [Value<'heap>], arg| {
                paste! {
                    fn [<$name _code_2>]<'heap>(
                        eval: &mut Interpreter<'_, 'heap>,
                        env: &'heap [Value<'heap>],
                        arg: Value<'heap>,
                    ) -> Value<'heap> {
                        $body(eval, env, arg)
                    }
                    let env = eval.alloc_values({
                        let mut env = Vec::from(env);
                        env.push(arg);
                        env
                    });
                    eval.alloc(Object::StaticClosure {
                        env,
                        body: StaticClosureBody([<$name _code_2>]),
                    })
                }
            })
        )
    }};
}

#[derive(Clone)]
pub struct StaticClosureBody<'heap>(
    fn(&mut Interpreter<'_, 'heap>, &'heap [Value<'heap>], Value<'heap>) -> Value<'heap>,
);

impl<'heap> Debug for StaticClosureBody<'heap> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("StaticClosure()")
    }
}

#[derive(Clone)]
pub struct IOBody<'heap>(fn(&mut Interpreter<'_, 'heap>, &'heap [Value<'heap>]) -> Value<'heap>);

impl<'heap> Debug for IOBody<'heap> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("IO()")
    }
}

#[derive(Debug)]
pub enum Object<'heap> {
    String(&'heap str),
    Bytes(&'heap [u8]),
    Variant(usize, Value<'heap>),
    Array(&'heap [Value<'heap>]),
    Record(&'heap [Value<'heap>]),
    Closure {
        env: &'heap [Value<'heap>],
        arg: bool,
        body: Rc<Expr>,
    },
    StaticClosure {
        env: &'heap [Value<'heap>],
        body: StaticClosureBody<'heap>,
    },
    IO {
        env: &'heap [Value<'heap>],
        body: IOBody<'heap>,
    },
}

impl<'heap> Object<'heap> {
    pub fn unpack_array(&'heap self) -> &'heap [Value<'heap>] {
        match self {
            Object::Array(vals) => vals,
            val => panic!("expected array, got {:?}", val),
        }
    }

    pub fn perform_io<'stdout>(
        &'heap self,
        interpreter: &mut Interpreter<'stdout, 'heap>,
    ) -> Value<'heap> {
        match self {
            Object::IO { env, body } => body.0(interpreter, env),
            val => panic!("expected io, got {:?}", val),
        }
    }

    pub fn unpack_string(&'heap self) -> &'heap str {
        match self {
            Object::String(str) => str,
            val => panic!("expected string, got {:?}", val),
        }
    }

    pub fn unpack_bytes(&'heap self) -> &'heap [u8] {
        match self {
            Object::Bytes(bs) => bs,
            val => panic!("expected bytes, got {:?}", val),
        }
    }

    pub fn unpack_variant(&'heap self) -> (&'heap usize, &'heap Value<'heap>) {
        match self {
            Object::Variant(tag, rest) => (tag, rest),
            val => panic!("expected variant, got {:?}", val),
        }
    }

    pub fn apply<'stdout>(
        &'heap self,
        interpreter: &mut Interpreter<'stdout, 'heap>,
        arg: Value<'heap>,
    ) -> Value<'heap> {
        match self {
            Object::Closure {
                env,
                arg: use_arg,
                body,
            } => {
                let new_env: &[Value];
                if *use_arg {
                    let mut buffer = Vec::with_capacity(env.len() + 1);
                    buffer.extend_from_slice(env);
                    buffer.push(arg);
                    new_env = interpreter.alloc_values(buffer);
                } else {
                    new_env = env;
                }
                interpreter.eval(new_env, body.clone())
            }
            Object::StaticClosure { env, body } => body.0(interpreter, env, arg),
            a => panic!("expected closure, got {:?}", a),
        }
    }

    pub fn render(&self) -> String {
        match self {
            Object::Closure {
                env: _,
                arg: _,
                body: _,
            } => String::from("<closure>"),
            Object::StaticClosure { env: _, body: _ } => String::from("<static builtin>"),
            Object::IO { env: _, body: _ } => String::from("<io>"),
            Object::String(s) => format!("{:?}", s),
            Object::Bytes(bs) => format!("{:?}", bs),
            Object::Array(items) => {
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
            Object::Record(items) => {
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
            Object::Variant(tag, value) => {
                let mut s = String::new();
                s.push_str("ctor(");
                s.push_str(format!("{:?}", tag).as_str());
                s.push_str(", ");
                s.push_str(value.render().as_str());
                s.push(')');
                s
            }
        }
    }
}

impl<'heap> PartialEq for Object<'heap> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Object::Closure { env, arg, body } => match other {
                Object::Closure {
                    env: env2,
                    arg: arg2,
                    body: body2,
                } => env == env2 && arg == arg2 && body == body2,
                _ => false,
            },
            Object::StaticClosure { env, body } => match other {
                Object::StaticClosure {
                    env: env2,
                    body: body2,
                } => env == env2 && (body.0 as usize) == (body2.0 as usize),
                _ => false,
            },
            Object::IO { env, body } => match other {
                Object::IO {
                    env: env2,
                    body: body2,
                } => env == env2 && (body.0 as usize) == (body2.0 as usize),
                _ => false,
            },
            Object::String(s) => match other {
                Object::String(s2) => s == s2,
                _ => false,
            },
            Object::Bytes(bs) => match other {
                Object::Bytes(bs2) => bs == bs2,
                _ => false,
            },
            Object::Array(items) => match other {
                Object::Array(items2) => items == items2,
                _ => false,
            },
            Object::Record(items) => match other {
                Object::Record(items2) => items == items2,
                _ => false,
            },
            Object::Variant(tag, value) => match other {
                Object::Variant(tag2, value2) => tag == tag2 && value == value2,
                _ => false,
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Value<'heap> {
    True,
    False,
    Int(u32),
    Char(char),
    Unit,

    Stdout,
    Stdin,

    Object(&'heap Object<'heap>),
}

impl<'heap> Value<'heap> {
    pub fn apply<'stdout>(
        &self,
        interpreter: &mut Interpreter<'stdout, 'heap>,
        arg: Value<'heap>,
    ) -> Value<'heap> {
        self.unpack_object().apply(interpreter, arg)
    }

    pub fn perform_io<'stdout>(
        &self,
        interpreter: &mut Interpreter<'stdout, 'heap>,
    ) -> Value<'heap> {
        self.unpack_object().perform_io(interpreter)
    }

    pub fn unpack_string(&self) -> &'heap str {
        self.unpack_object().unpack_string()
    }

    pub fn unpack_bytes(&self) -> &'heap [u8] {
        self.unpack_object().unpack_bytes()
    }

    pub fn unpack_array(&self) -> &'heap [Value<'heap>] {
        self.unpack_object().unpack_array()
    }

    pub fn unpack_variant(&self) -> (&'heap usize, &'heap Value<'heap>) {
        self.unpack_object().unpack_variant()
    }

    pub fn unpack_object(&self) -> &'heap Object<'heap> {
        match self {
            Value::Object(obj) => obj,
            val => panic!("expected object, got {:?}", val),
        }
    }

    pub fn unpack_bool(&self) -> bool {
        match self {
            Value::False => false,
            Value::True => true,
            val => panic!("expected bool, got {:?}", val),
        }
    }

    pub fn unpack_int(&self) -> u32 {
        match self {
            Value::Int(n) => *n,
            val => panic!("expected int, got {:?}", val),
        }
    }

    pub fn unpack_char(&self) -> char {
        match self {
            Value::Char(c) => *c,
            val => panic!("expected char, got {:?}", val),
        }
    }

    pub fn unpack_stdout(&self) {
        match self {
            Value::Stdout => (),
            val => panic!("expected stdout, got {:?}", val),
        }
    }

    pub fn unpack_stdin(&self) {
        match self {
            Value::Stdin => (),
            val => panic!("expected stdin, got {:?}", val),
        }
    }

    pub fn render(&self) -> String {
        match self {
            Value::True => String::from("true"),
            Value::False => String::from("false"),
            Value::Int(n) => format!("{:?}", n),
            Value::Char(c) => format!("{:?}", c),
            Value::Unit => String::from("()"),
            Value::Stdout => String::from("Stdout"),
            Value::Stdin => String::from("Stdin"),
            Value::Object(o) => o.render(),
        }
    }
}

impl<'heap> PartialEq for Value<'heap> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::True => matches!(other, Value::True),
            Value::False => matches!(other, Value::False),
            Value::Int(n) => match other {
                Value::Int(n2) => n == n2,
                _ => false,
            },
            Value::Char(c) => match other {
                Value::Char(c2) => c == c2,
                _ => false,
            },
            Value::Unit => matches!(other, Value::Unit),
            Value::Stdout => matches!(other, Value::Stdout),
            Value::Stdin => matches!(other, Value::Stdin),
            Value::Object(o1) => match other {
                Value::Object(o2) => o1.eq(o2),
                _ => false,
            },
        }
    }
}

pub struct Module {
    pub module_mapping: HashMap<ModulePath, ModuleUsage>,
    pub bindings: HashMap<String, Expr>,
}

pub struct Interpreter<'stdout, 'heap> {
    stdin: &'stdout mut dyn BufRead,
    stdout: &'stdout mut dyn Write,
    bytes: &'heap Arena<u8>,
    values: &'heap Arena<Value<'heap>>,
    objects: &'heap Arena<Object<'heap>>,
    context: HashMap<String, Rc<Expr>>,
    module_context: HashMap<ModulePath, Module>,
    module_unmapping: Vec<HashMap<ModuleName, ModulePath>>,
}

impl<'stdout, 'heap> Interpreter<'stdout, 'heap> {
    pub fn new(
        stdin: &'stdout mut dyn BufRead,
        stdout: &'stdout mut dyn Write,
        context: HashMap<String, Expr>,
        module_context: HashMap<ModulePath, Module>,
        bytes: &'heap Arena<u8>,
        values: &'heap Arena<Value<'heap>>,
        objects: &'heap Arena<Object<'heap>>,
    ) -> Self {
        Interpreter {
            stdin,
            stdout,
            context: context
                .into_iter()
                .map(|(name, expr)| (name, Rc::new(expr)))
                .collect(),
            module_context,
            module_unmapping: Vec::with_capacity(1),
            bytes,
            values,
            objects,
        }
    }

    pub fn register_module(&mut self, module: &core::Module) {
        let module_context = module.decls.iter().filter_map(|x| match x {
            Declaration::Definition { name, sig: _, body } => {
                Some((name.clone(), Rc::new(body.clone())))
            }
            _ => None,
        });
        self.context.extend(module_context);
    }

    pub fn alloc(&self, obj: Object<'heap>) -> Value<'heap> {
        Value::Object(self.objects.alloc(obj))
    }

    pub fn alloc_str(&self, s: &str) -> &'heap str {
        self.bytes.alloc_str(s)
    }

    pub fn alloc_bytes<I: IntoIterator<Item = u8>>(&self, s: I) -> &'heap [u8] {
        self.bytes.alloc_extend(s)
    }

    pub fn alloc_values<I: IntoIterator<Item = Value<'heap>>>(
        &self,
        vals: I,
    ) -> &'heap [Value<'heap>]
where {
        self.values.alloc_extend(vals)
    }

    pub fn eval_builtin(&self, name: &Builtin) -> Value<'heap> {
        match name {
            Builtin::PureIO => {
                fn pure_io_0<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap [Value<'heap>],
                    arg: Value<'heap>,
                ) -> Value<'heap> {
                    fn pure_io_1<'stdout, 'heap>(
                        _: &mut Interpreter<'stdout, 'heap>,
                        env: &'heap [Value<'heap>],
                    ) -> Value<'heap> {
                        env[0]
                    }
                    let env = interpreter.alloc_values({
                        let mut env = Vec::from(env);
                        env.push(arg);
                        env
                    });
                    let closure = Object::IO {
                        env,
                        body: IOBody(pure_io_1),
                    };
                    interpreter.alloc(closure)
                }
                let closure = Object::StaticClosure {
                    env: &[],
                    body: StaticClosureBody(pure_io_0),
                };
                self.alloc(closure)
            }
            Builtin::MapIO => {
                fn map_io_0<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap [Value<'heap>],
                    arg: Value<'heap>, // a -> b
                ) -> Value<'heap> {
                    fn map_io_1<'stdout, 'heap>(
                        interpreter: &mut Interpreter<'stdout, 'heap>,
                        env: &'heap [Value<'heap>],
                        arg: Value<'heap>, // IO a
                    ) -> Value<'heap> {
                        fn map_io_2<'stdout, 'heap>(
                            interpreter: &mut Interpreter<'stdout, 'heap>,
                            env: &'heap [Value<'heap>],
                        ) -> Value<'heap> {
                            let f = env[0];
                            let io_a = env[1];
                            let a = io_a.perform_io(interpreter); // type: a
                            f.apply(interpreter, a) // type: b
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env);
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(map_io_2),
                        };
                        interpreter.alloc(closure)
                    }
                    let env = interpreter.alloc_values({
                        let mut env = Vec::from(env);
                        env.push(arg);
                        env
                    });
                    let closure = Object::StaticClosure {
                        env,
                        body: StaticClosureBody(map_io_1),
                    };
                    interpreter.alloc(closure)
                }
                let closure = Object::StaticClosure {
                    env: &[],
                    body: StaticClosureBody(map_io_0),
                };
                self.alloc(closure)
            }
            Builtin::BindIO => {
                fn bind_io_0<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap [Value<'heap>],
                    arg: Value<'heap>, // IO a
                ) -> Value<'heap> {
                    fn bind_io_1<'stdout, 'heap>(
                        interpreter: &mut Interpreter<'stdout, 'heap>,
                        env: &'heap [Value<'heap>],
                        arg: Value<'heap>, // a -> IO b
                    ) -> Value<'heap> {
                        fn bind_io_2<'stdout, 'heap>(
                            interpreter: &mut Interpreter<'stdout, 'heap>,
                            env: &'heap [Value<'heap>],
                        ) -> Value<'heap> {
                            let io_a = env[0];
                            let f = env[1];
                            let a = io_a.perform_io(interpreter); // type: a
                            let io_b = f.apply(interpreter, a); // type: IO b
                            io_b.perform_io(interpreter) // type: b
                        }
                        let env = interpreter.alloc_values({
                            let mut new_env = Vec::with_capacity(env.len() + 1);
                            new_env.extend_from_slice(env);
                            new_env.push(arg);
                            new_env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(bind_io_2),
                        };
                        interpreter.alloc(closure)
                    }
                    let env = interpreter.alloc_values({
                        let mut new_env = Vec::with_capacity(env.len() + 1);
                        new_env.extend_from_slice(env);
                        new_env.push(arg);
                        new_env
                    });
                    let closure = Object::StaticClosure {
                        env,
                        body: StaticClosureBody(bind_io_1),
                    };
                    interpreter.alloc(closure)
                }
                let closure = Object::StaticClosure {
                    env: &[],
                    body: StaticClosureBody(bind_io_0),
                };
                self.alloc(closure)
            }
            Builtin::Trace => {
                fn code_outer<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap [Value<'heap>],
                    arg: Value<'heap>,
                ) -> Value<'heap> {
                    fn code_inner<'stdout, 'heap>(
                        interpreter: &mut Interpreter<'stdout, 'heap>,
                        env: &'heap [Value<'heap>],
                        arg: Value<'heap>,
                    ) -> Value<'heap> {
                        let _ = writeln!(interpreter.stdout, "trace: {}", env[0].render()).unwrap();
                        arg
                    }
                    let env = interpreter.alloc_values({
                        let mut env = Vec::from(env);
                        env.push(arg);
                        env
                    });
                    let closure = Object::StaticClosure {
                        env,
                        body: StaticClosureBody(code_inner),
                    };
                    interpreter.alloc(closure)
                }
                let closure = self.alloc(Object::StaticClosure {
                    env: &[],
                    body: StaticClosureBody(code_outer),
                });
                closure
            }
            Builtin::ToUtf8 => {
                fn to_utf8_0<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    _env: &'heap [Value<'heap>],
                    arg: Value<'heap>,
                ) -> Value<'heap> {
                    let a = arg.unpack_string();
                    interpreter.alloc(Object::Bytes(a.as_bytes()))
                }
                let closure = self.alloc(Object::StaticClosure {
                    env: &[],
                    body: StaticClosureBody(to_utf8_0),
                });
                closure
            }
            Builtin::Stdout => Value::Stdout,
            Builtin::Stdin => Value::Stdin,
            Builtin::WriteStdout => {
                fn write_stdout_0<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap [Value<'heap>],
                    arg: Value<'heap>, // Stdout
                ) -> Value<'heap> {
                    fn write_stdout_1<'stdout, 'heap>(
                        interpreter: &mut Interpreter<'stdout, 'heap>,
                        env: &'heap [Value<'heap>],
                        arg: Value<'heap>, // Bytes
                    ) -> Value<'heap> {
                        fn write_stdout_2<'stdout, 'heap>(
                            interpreter: &mut Interpreter<'stdout, 'heap>,
                            env: &'heap [Value<'heap>],
                        ) -> Value<'heap> {
                            // env[0] : Stdout
                            // env[1] : Bytes
                            let () = env[0].unpack_stdout();
                            let bs = env[1].unpack_bytes();
                            let _ = interpreter.stdout.write_all(bs).unwrap();
                            Value::Unit
                        }

                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env);
                            env.push(arg);
                            env
                        });
                        interpreter.alloc(Object::IO {
                            env,
                            body: IOBody(write_stdout_2),
                        })
                    }
                    let env = interpreter.alloc_values({
                        let mut env = Vec::from(env);
                        env.push(arg);
                        env
                    });
                    let closure = interpreter.alloc(Object::StaticClosure {
                        env,
                        body: StaticClosureBody(write_stdout_1),
                    });
                    closure
                }
                let closure = self.alloc(Object::StaticClosure {
                    env: &[],
                    body: StaticClosureBody(write_stdout_0),
                });
                closure
            }
            Builtin::FlushStdout => {
                fn flush_stdout<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap [Value<'heap>],
                ) -> Value<'heap> {
                    // env[0] : Stdout
                    env[0].unpack_stdout();
                    interpreter.stdout.flush().unwrap();
                    Value::Unit
                }
                function1!(
                    flush_stdout,
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let env = eval.alloc_values({
                            let mut env = Vec::from(env);
                            env.push(arg);
                            env
                        });
                        eval.alloc(Object::IO {
                            env,
                            body: IOBody(flush_stdout),
                        })
                    }
                )
            }
            Builtin::ReadLineStdin => {
                fn read_line_stdin_0<'stdout, 'heap>(
                    interpreter: &mut Interpreter<'stdout, 'heap>,
                    env: &'heap [Value<'heap>],
                    arg: Value<'heap>, // Stdout
                ) -> Value<'heap> {
                    fn read_line_stdin_1<'stdout, 'heap>(
                        interpreter: &mut Interpreter<'stdout, 'heap>,
                        env: &'heap [Value<'heap>],
                    ) -> Value<'heap> {
                        // env[0] : Stdin
                        let () = env[0].unpack_stdin();
                        let mut str = String::new();
                        let _ = interpreter.stdin.read_line(&mut str).unwrap();
                        let str = interpreter.alloc_str(&str);
                        interpreter.alloc(Object::String(str))
                    }
                    let env = interpreter.alloc_values({
                        let mut env = Vec::from(env);
                        env.push(arg);
                        env
                    });
                    let closure = interpreter.alloc(Object::IO {
                        env,
                        body: IOBody(read_line_stdin_1),
                    });
                    closure
                }
                let closure = self.alloc(Object::StaticClosure {
                    env: &[],
                    body: StaticClosureBody(read_line_stdin_0),
                });
                closure
            }
            Builtin::EqString => {
                function2!(
                    eq_string,
                    self,
                    |_: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let a = env[0].unpack_string();
                        let b = arg.unpack_string();
                        if a == b {
                            Value::True
                        } else {
                            Value::False
                        }
                    }
                )
            }
            Builtin::EqInt => {
                fn eq_int_0<'heap>(
                    eval: &mut Interpreter<'_, 'heap>,
                    _env: &'heap [Value<'heap>],
                    arg: Value<'heap>,
                ) -> Value<'heap> {
                    fn eq_int_1<'heap>(
                        _: &mut Interpreter<'_, 'heap>,
                        env: &'heap [Value<'heap>],
                        arg: Value<'heap>,
                    ) -> Value<'heap> {
                        let a = env[0].unpack_int();
                        let b = arg.unpack_int();
                        if a == b {
                            Value::True
                        } else {
                            Value::False
                        }
                    }
                    let env = eval.alloc_values(vec![arg]);
                    let closure = eval.alloc(Object::StaticClosure {
                        env,
                        body: StaticClosureBody(eq_int_1),
                    });
                    closure
                }

                let closure = self.alloc(Object::StaticClosure {
                    env: &[],
                    body: StaticClosureBody(eq_int_0),
                });
                closure
            }
            Builtin::LtInt => {
                function2!(
                    lt_int,
                    self,
                    |_: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let a = env[0].unpack_int();
                        let b = arg.unpack_int();
                        if a < b {
                            Value::True
                        } else {
                            Value::False
                        }
                    }
                )
            }
            Builtin::ShowInt => {
                function1!(
                    show_int,
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     _env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let a = arg.unpack_int();
                        let str = eval.alloc_str(&format!("{}", a));
                        eval.alloc(Object::String(str))
                    }
                )
            }
            Builtin::Subtract => {
                function2!(
                    subtract,
                    self,
                    |_: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let a = env[0].unpack_int();
                        let b = arg.unpack_int();
                        Value::Int(a - b)
                    }
                )
            }
            Builtin::Add => {
                function2!(
                    add,
                    self,
                    |_: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let a = env[0].unpack_int();
                        let b = arg.unpack_int();
                        Value::Int(a + b)
                    }
                )
            }
            Builtin::Multiply => {
                function2!(
                    multiply,
                    self,
                    |_: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let a = env[0].unpack_int();
                        let b = arg.unpack_int();
                        Value::Int(a * b)
                    }
                )
            }
            Builtin::EqArray => {
                fn eq_int_0<'heap>(
                    eval: &mut Interpreter<'_, 'heap>,
                    _env: &'heap [Value<'heap>],
                    arg: Value<'heap>,
                ) -> Value<'heap> {
                    fn eq_int_1<'heap>(
                        eval: &mut Interpreter<'_, 'heap>,
                        env: &'heap [Value<'heap>],
                        arg: Value<'heap>,
                    ) -> Value<'heap> {
                        fn eq_int_2<'heap>(
                            eval: &mut Interpreter<'_, 'heap>,
                            env: &'heap [Value<'heap>],
                            arg: Value<'heap>,
                        ) -> Value<'heap> {
                            let f = env[0];
                            let a = env[1].unpack_array();
                            let b = arg.unpack_array();

                            let mut acc = Value::True;
                            if a.len() == b.len() {
                                for (a, b) in a.iter().zip(b.iter()) {
                                    let res = f.apply(eval, *a).apply(eval, *b).unpack_bool();
                                    if !res {
                                        acc = Value::False;
                                        break;
                                    }
                                }
                            } else {
                                acc = Value::False;
                            }
                            acc
                        }
                        let env = eval.alloc_values(vec![env[0], arg]);
                        let closure = eval.alloc(Object::StaticClosure {
                            env,
                            body: StaticClosureBody(eq_int_2),
                        });
                        closure
                    }
                    let env = eval.alloc_values(vec![arg]);
                    let closure = eval.alloc(Object::StaticClosure {
                        env,
                        body: StaticClosureBody(eq_int_1),
                    });
                    closure
                }

                let closure = self.alloc(Object::StaticClosure {
                    env: &[],
                    body: StaticClosureBody(eq_int_0),
                });
                closure
            }
            Builtin::LtArray => {
                function3!(
                    lt_array,
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
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
                                        return Value::False;
                                    }
                                } else {
                                    // a is longer than b
                                    return Value::False;
                                }
                            } else if ix < b_len {
                                // a is shorter than b
                                return Value::True;
                            } else {
                                // a is the same length as b
                                return Value::False;
                            }
                        }
                    }
                )
            }
            Builtin::FoldlArray => {
                function3!(
                    foldl_array,
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let f = env[0];
                        let z = env[1];
                        let arr = arg.unpack_array();

                        let mut acc = z;
                        for el in arr {
                            acc = f.apply(eval, acc).apply(eval, *el);
                        }
                        acc
                    }
                )
            }
            Builtin::GenerateArray => {
                function2!(
                    generate_array,
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let len = env[0].unpack_int();
                        let f = arg;

                        let mut array = Vec::with_capacity(len as usize);
                        for ix in 0..len {
                            let ix = Value::Int(ix);
                            array.push(f.apply(eval, ix));
                        }

                        let array = eval.alloc_values(array);
                        eval.alloc(Object::Array(array))
                    }
                )
            }
            Builtin::LengthArray => {
                function1!(
                    length_array,
                    self,
                    |_: &mut Interpreter<'_, 'heap>,
                     _env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let arr = arg.unpack_array();

                        Value::Int(arr.len() as u32)
                    }
                )
            }
            Builtin::IndexArray => {
                function2!(
                    index_array,
                    self,
                    |_eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let ix = env[0].unpack_int() as usize;
                        let arr = arg.unpack_array();

                        arr[ix]
                    }
                )
            }
            Builtin::SliceArray => {
                function3!(
                    slice_array,
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let start = env[0].unpack_int() as usize;
                        let len = env[1].unpack_int() as usize;
                        let arr = arg.unpack_array();

                        eval.alloc(Object::Array(&arr[start..start + len]))
                    }
                )
            }
            Builtin::FilterString => {
                function2!(
                    filter_string,
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let predicate = env[0];
                        let string = arg.unpack_string();
                        let new_string: String = string
                            .chars()
                            .filter(|c| {
                                let c_val = Value::Char(*c);
                                predicate.apply(eval, c_val).unpack_bool()
                            })
                            .collect();
                        let str = eval.alloc_str(&new_string);
                        eval.alloc(Object::String(str))
                    }
                )
            }
            Builtin::EqChar => {
                function2!(
                    eq_char,
                    self,
                    |_: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let c1 = env[0].unpack_char();
                        let c2 = arg.unpack_char();
                        if c1 == c2 {
                            Value::True
                        } else {
                            Value::False
                        }
                    }
                )
            }
            Builtin::SplitString => {
                function2!(
                    split_string,
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let c = env[0].unpack_char();
                        let s = arg.unpack_string();
                        let a =
                            eval.alloc_values(s.split(c).map(|s| eval.alloc(Object::String(s))));
                        eval.alloc(Object::Array(a))
                    }
                )
            }
            Builtin::FoldlString => {
                function3!(
                    foldl_string,
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let f = env[0];
                        let mut acc = env[1];
                        let s = arg.unpack_string();
                        for c in s.chars() {
                            let c_value = Value::Char(c);
                            acc = f.apply(eval, acc).apply(eval, c_value);
                        }
                        acc
                    }
                )
            }
            Builtin::SnocArray => {
                function2!(
                    snoc_array,
                    self,
                    |eval: &mut Interpreter<'_, 'heap>,
                     env: &'heap [Value<'heap>],
                     arg: Value<'heap>| {
                        let array = env[0].unpack_array();
                        let new_array = eval.alloc_values({
                            let mut new_array = Vec::from(array);
                            new_array.push(arg);
                            new_array
                        });
                        eval.alloc(Object::Array(new_array))
                    }
                )
            }
        }
    }

    pub fn eval_from_module(
        &mut self,
        env: &'heap [Value<'heap>],
        path: &ModulePath,
        binding: &str,
    ) -> Value<'heap> {
        let (expr, next_module_mapping) = match self.module_context.get(path) {
            None => panic!("no module found at {:?}", path),
            Some(module) => match module.bindings.get(binding) {
                None => panic!("{:?} not found in {:?}", binding, path),
                Some(expr) => (expr.clone(), module.module_mapping.clone()),
            },
        };
        self.module_unmapping.push(
            next_module_mapping
                .into_iter()
                .filter_map(|(module_path, module_usage)| {
                    let m_module_name = match module_usage {
                        ModuleUsage::All => module_path.get_module_name().cloned(),
                        ModuleUsage::Items(_) => module_path.get_module_name().cloned(),
                        ModuleUsage::Named(name) => Some(ModuleName(vec![name])),
                    };
                    m_module_name.map(|module_name| (module_name, module_path))
                })
                .collect(),
        );
        let res = self.eval(env, Rc::new(expr));
        self.module_unmapping.pop();
        res
    }

    pub fn eval(&mut self, env: &'heap [Value<'heap>], expr: Rc<Expr>) -> Value<'heap> {
        let out = match expr.as_ref() {
            Expr::Var(ix) => env[env.len() - 1 - ix],
            Expr::EVar(n) => panic!("found EVar({:?})", n),
            Expr::Placeholder(n) => panic!("found Placeholder({:?})", n),
            Expr::Name(name) => match self.context.get(name) {
                None => panic!("{:?} not in scope", name),
                Some(body) => {
                    let body: Rc<Expr> = body.clone();
                    self.eval(env, body)
                }
            },
            Expr::Module(name, item) => self.eval_from_module(
                env,
                &self
                    .module_unmapping
                    .last()
                    .unwrap()
                    .get(name)
                    .unwrap()
                    .clone(),
                item,
            ),
            Expr::Builtin(name) => self.eval_builtin(name),

            Expr::App(a, b) => {
                let a = self.eval(env, a.clone());
                let b = self.eval(env, b.clone());
                a.apply(self, b)
            }
            Expr::Lam { arg, body } => self.alloc(Object::Closure {
                env,
                arg: *arg,
                body: body.clone(),
            }),

            Expr::True => Value::True,
            Expr::False => Value::False,
            Expr::IfThenElse(cond, t, e) => {
                let cond = self.eval(env, cond.clone());
                match cond {
                    Value::True => self.eval(env, t.clone()),
                    Value::False => self.eval(env, e.clone()),
                    cond => panic!("expected bool, got {:?}", cond),
                }
            }

            Expr::Int(n) => Value::Int(*n),

            Expr::Binop(op, a, b) => {
                let a = self.eval(env, a.clone());
                let b = self.eval(env, b.clone());
                match op {
                    Binop::Add => {
                        let a = a.unpack_int();
                        let b = b.unpack_int();
                        Value::Int(a + b)
                    }
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

            Expr::Char(c) => Value::Char(*c),

            Expr::String(parts) => {
                let mut value = String::new();

                for part in parts {
                    match part {
                        StringPart::Expr(expr) => {
                            let s = self.eval(env, Rc::new(expr.clone())).unpack_string();
                            value.push_str(s);
                        }
                        StringPart::String(s) => value.push_str(s.as_str()),
                    }
                }
                let str = self.alloc_str(&value);
                self.alloc(Object::String(str))
            }

            Expr::Array(items) => {
                let items: Vec<Value> = items
                    .iter()
                    .map(|item| self.eval(env, Rc::new(item.clone())))
                    .collect();
                let items = self.alloc_values(items);
                self.alloc(Object::Array(items))
            }

            Expr::Extend(ev, value, rest) => {
                let ix = self.eval(env, ev.clone()).unpack_int();
                let value = self.eval(env, value.clone());
                let rest = self.eval(env, rest.clone());
                match rest.unpack_object() {
                    Object::Record(fields) => {
                        // assume: all stacks in fields are non-empty
                        let ix = ix as usize;
                        let mut record = Vec::with_capacity(fields.len() + 1);
                        record.extend_from_slice(&fields[0..ix]);
                        record.push(value);
                        record.extend_from_slice(&fields[ix..]);

                        debug_assert!(record.len() == fields.len() + 1);

                        let record = self.alloc_values(record);
                        self.alloc(Object::Record(record))
                    }
                    rest => panic!("expected record, got {:?}", rest),
                }
            }
            Expr::Record(fields) => {
                let mut record: Vec<Value<'heap>> = Vec::with_capacity(fields.len());
                let mut fields: Vec<(u32, Value<'heap>)> = fields
                    .iter()
                    .map(|(ev, field)| {
                        (
                            self.eval(env, Rc::new(ev.clone())).unpack_int(),
                            self.eval(env, Rc::new(field.clone())),
                        )
                    })
                    .collect();
                fields.sort_by_key(|x| x.0);
                for (_index, field) in fields.into_iter() {
                    record.push(field);
                }

                let record = self.alloc_values(record);
                self.alloc(Object::Record(record))
            }
            Expr::Project(expr, index) => {
                let index = self.eval(env, index.clone()).unpack_int();
                let expr = self.eval(env, expr.clone());
                match expr.unpack_object() {
                    Object::Record(fields) => fields[index as usize],
                    expr => panic!("expected record, got {:?}", expr),
                }
            }

            Expr::Variant(tag) => {
                let tag = self.eval(env, tag.clone());
                let env = self.alloc_values(vec![tag]);
                fn code<'heap>(
                    interpreter: &mut Interpreter<'_, 'heap>,
                    env: &'heap [Value<'heap>],
                    arg: Value<'heap>,
                ) -> Value<'heap> {
                    let tag = env[0].unpack_int() as usize;
                    interpreter.alloc(Object::Variant(tag, arg))
                }
                let closure = Object::StaticClosure {
                    env,
                    body: StaticClosureBody(code),
                };
                self.alloc(closure)
            }
            Expr::Embed(tag, rest) => {
                let tag = self.eval(env, tag.clone()).unpack_int() as usize;
                let rest = self.eval(env, rest.clone());
                let (&old_tag, arg) = rest.unpack_variant();
                self.alloc(Object::Variant(
                    if tag <= old_tag { old_tag + 1 } else { old_tag },
                    *arg,
                ))
            }
            Expr::Case(expr, branches) => {
                let expr = self.eval(env, expr.clone());
                match expr.unpack_object() {
                    Object::Record(fields) => {
                        // expect a record pattern
                        debug_assert!(branches.len() == 1);
                        let branch = &branches[0];
                        match &branch.pattern {
                            Pattern::Record { names, rest } => {
                                let mut new_env = Vec::from(env);
                                let mut extracted = Vec::new();
                                for name in names {
                                    let ix =
                                        self.eval(env, Rc::new(name.clone())).unpack_int() as usize;
                                    new_env.push(fields[ix]);
                                    extracted.push(ix);
                                }
                                if *rest {
                                    let mut leftover_fields = Rope::from_vec(fields);
                                    extracted.sort_unstable();
                                    for ix in extracted.iter().rev() {
                                        leftover_fields = leftover_fields.delete(*ix).unwrap();
                                    }
                                    let leftover_fields =
                                        self.alloc_values(leftover_fields.iter().copied());
                                    let leftover_record =
                                        self.alloc(Object::Record(leftover_fields));
                                    new_env.push(leftover_record);
                                }

                                let new_env = self.alloc_values(new_env);
                                self.eval(new_env, Rc::new(branch.body.clone()))
                            }
                            Pattern::Name {} | Pattern::Wildcard {} => {
                                todo!("allow name and wildcard patterns")
                            }
                            Pattern::Variant { .. } => {
                                panic!("expected record pattern, but got {:?}", branch.pattern)
                            }
                        }
                    }
                    Object::Variant(tag, value) => {
                        // expect variant patterns
                        let mut target: Option<Expr> = None;
                        let mut new_env = Vec::from(env);
                        for branch in branches {
                            match &branch.pattern {
                                Pattern::Record { .. } => {
                                    panic!("expected variant pattern, got {:?}", branch.pattern)
                                }
                                Pattern::Variant { tag: branch_tag } => {
                                    let branch_tag =
                                        self.eval(env, branch_tag.clone()).unpack_int() as usize;
                                    if *tag == branch_tag {
                                        new_env.push(*value);
                                        target = Some(branch.body.clone());
                                        break;
                                    }
                                }
                                Pattern::Name => {
                                    new_env.push(expr);
                                    target = Some(branch.body.clone());
                                    break;
                                }
                                Pattern::Wildcard => {
                                    target = Some(branch.body.clone());
                                    break;
                                }
                            }
                        }
                        match target {
                            None => panic!("pattern match failure"),
                            Some(body) => {
                                let env = self.alloc_values(new_env);
                                self.eval(env, Rc::new(body))
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
            Expr::Unit => Value::Unit,
        };
        out
    }
}
