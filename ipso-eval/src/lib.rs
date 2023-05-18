#![deny(unused_crate_dependencies)]

#[cfg(test)]
mod test;

pub mod bindings;
pub mod closure_conversion;
pub mod reflect;

use bindings::{Binding, Bindings};
use closure_conversion::Expr;
use ipso_core::{self as core, Binop, Builtin, CommonKinds, Name, Pattern, StringPart};
use ipso_rope::Rope;
use ipso_syntax::{ModuleId, ModuleRef, Modules};
use paste::paste;
use reflect::{Reflect, ReflectRef};
use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt::Debug,
    io::Write,
    io::{self, BufRead, BufReader},
    ops::Index,
    path::Path,
    process::{self, Command, ExitStatus, Stdio},
    rc::Rc,
};

fn check_exit_status(cmd: &str, status: &ExitStatus) {
    match status.code() {
        Some(code) => {
            if code == 0 {
            } else {
                println!("process {:?} exited with code {:?}", cmd, code);
                process::exit(1);
            }
        }
        None => {
            println!("process {:?} terminated unexpectedly", cmd);
            process::exit(1);
        }
    }
}

macro_rules! function1 {
    ($name:ident, $self:expr, $body:expr) => {{
        paste! {
            #[allow(non_snake_case)]
            fn [<$name _code_0>](
                eval: &mut Interpreter<'_>,
                env: Rc<[Value]>,
                arg: Value,
            ) -> Value {
                // This clippy lint is `allow`ed because requiring `$body` to be a function
                // seems to be the only way to make sure it's scope-checked.
                #[allow(clippy::redundant_closure_call)]
                $body(eval, env, arg)
            }

            let closure = $self.alloc(Object::StaticClosure {
                env: Rc::new([]),
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
            (|eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                paste! {
                    fn [<$name _code_1>](
                        eval: &mut Interpreter<'_>,
                        env: Rc<[Value]>,
                        arg: Value,
                    ) -> Value {
                        $body(eval, env, arg)
                    }
                    let env = eval.alloc_values({
                        let mut env = Vec::from(env.as_ref());
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
            (|eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg| {
                paste! {
                    fn [<$name _code_2>](
                        eval: &mut Interpreter<'_>,
                        env: Rc<[Value]>,
                        arg: Value,
                    ) -> Value {
                        $body(eval, env, arg)
                    }
                    let env = eval.alloc_values({
                        let mut env = Vec::from(env.as_ref());
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
pub struct StaticClosureBody(fn(&mut Interpreter<'_>, Rc<[Value]>, Value) -> Value);

impl Debug for StaticClosureBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("StaticClosure()")
    }
}

#[derive(Clone)]
pub struct IOBody(fn(&mut Interpreter<'_>, Rc<[Value]>) -> Value);

impl IOBody {
    pub fn run(&self, interpreter: &mut Interpreter<'_>, env: Rc<[Value]>) -> Value {
        self.0(interpreter, env)
    }
}

impl Debug for IOBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("IO()")
    }
}

#[derive(Debug)]
pub enum Env {
    Empty,
    Borrowed(Rc<[Value]>),
    Owned(Vec<Value>),
}

impl Default for Env {
    fn default() -> Self {
        Env::Empty
    }
}

impl Env {
    fn push(&mut self, value: Value) {
        match self {
            Env::Empty => {
                *self = Env::Owned(vec![value]);
            }
            Env::Borrowed(vs) => {
                let mut new_vs = Vec::with_capacity(vs.len() + 1);
                new_vs.extend_from_slice(vs);
                new_vs.push(value);
                *self = Env::Owned(new_vs);
            }
            Env::Owned(vs) => vs.push(value),
        }
    }

    fn pop(&mut self) -> Option<Value> {
        match self {
            Env::Empty => panic!("env is empty"),
            Env::Borrowed(values) => {
                let mut values = Vec::from(values.as_ref());
                let result = values.pop();
                *self = Env::Owned(values);
                result
            }
            Env::Owned(values) => values.pop(),
        }
    }

    fn len(&self) -> usize {
        match self {
            Env::Empty => 0,
            Env::Borrowed(vs) => vs.len(),
            Env::Owned(vs) => vs.len(),
        }
    }

    pub fn new() -> Self {
        Env::Owned(Vec::new())
    }
}

impl From<Rc<[Value]>> for Env {
    fn from(value: Rc<[Value]>) -> Self {
        Env::Borrowed(value)
    }
}

impl Index<usize> for Env {
    type Output = Value;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            Env::Empty => panic!("index {:?} out of bounds", index),
            Env::Borrowed(vs) => &vs[index],
            Env::Owned(vs) => &vs[index],
        }
    }
}

#[derive(Debug)]
pub enum Object {
    String(Rc<str>),
    Bytes(Rc<[u8]>),
    Variant(usize, Value),
    Array(Rc<[Value]>),
    Record(Rc<[Value]>),
    Closure {
        env: Rc<[Value]>,
        arg: bool,
        /**
        The module from which the closure originated.

        Any [`Expr::Name`]s in the closure's body reference definitions in this module.
        */
        module: Option<ModulePath>,
        body: Rc<Expr>,
    },
    StaticClosure {
        env: Rc<[Value]>,
        body: StaticClosureBody,
    },
    IO {
        env: Rc<[Value]>,
        body: IOBody,
    },
    Cmd(Vec<Rc<str>>),
}

impl Object {
    pub fn unpack_array(&self) -> Rc<[Value]> {
        match self {
            Object::Array(vals) => vals.clone(),
            val => panic!("expected array, got {:?}", val),
        }
    }

    pub fn perform_io<'io>(&self, interpreter: &mut Interpreter<'io>) -> Value {
        match self {
            Object::IO { env, body } => body.0(interpreter, env.clone()),
            val => panic!("expected io, got {:?}", val),
        }
    }

    pub fn unpack_string(&self) -> Rc<str> {
        match self {
            Object::String(str) => str.clone(),
            val => panic!("expected string, got {:?}", val),
        }
    }

    pub fn unpack_bytes(&self) -> &[u8] {
        match self {
            Object::Bytes(bs) => bs,
            val => panic!("expected bytes, got {:?}", val),
        }
    }

    pub fn unpack_variant(&self) -> (&usize, &Value) {
        match self {
            Object::Variant(tag, rest) => (tag, rest),
            val => panic!("expected variant, got {:?}", val),
        }
    }

    fn unpack_record(&self) -> Rc<[Value]> {
        match self {
            Object::Record(fields) => fields.clone(),
            val => panic!("expected record,got {:?}", val),
        }
    }

    pub fn unpack_cmd(&self) -> &[Rc<str>] {
        match self {
            Object::Cmd(values) => values,
            val => panic!("expected command, got {:?}", val),
        }
    }

    pub fn apply<'io>(&self, interpreter: &mut Interpreter<'io>, arg: Value) -> Value {
        match self {
            Object::Closure {
                env,
                arg: use_arg,
                module,
                body,
            } => {
                let mut env = Env::from(env.clone());
                if *use_arg {
                    env.push(arg);
                }

                if let Some(module) = module {
                    interpreter.context.modules.push(module.clone());
                }
                let result = interpreter.eval(&mut env, body);
                if module.is_some() {
                    interpreter.context.modules.pop();
                }
                result
            }
            Object::StaticClosure { env, body } => body.0(interpreter, env.clone(), arg),
            a => panic!("expected closure, got {:?}", a),
        }
    }

    pub fn render(&self) -> String {
        match self {
            Object::Closure { .. } => String::from("<closure>"),
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
            Object::Cmd(parts) => format!("Cmd({:?})", parts),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Object::Closure {
                env,
                arg,
                module,
                body,
            } => match other {
                Object::Closure {
                    env: env2,
                    arg: arg2,
                    module: module2,
                    body: body2,
                } => env == env2 && arg == arg2 && module == module2 && body == body2,
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
            Object::Cmd(parts) => match other {
                Object::Cmd(parts2) => parts == parts2,
                _ => false,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    True,
    False,
    Int(i32),
    Char(char),
    Unit,

    Object(Rc<Object>),
}

impl Value {
    pub fn apply<'io>(&self, interpreter: &mut Interpreter<'io>, arg: Value) -> Value {
        self.unpack_object().apply(interpreter, arg)
    }

    pub fn perform_io<'io>(&self, interpreter: &mut Interpreter<'io>) -> Value {
        self.unpack_object().perform_io(interpreter)
    }

    pub fn unpack_string(&self) -> Rc<str> {
        self.unpack_object().unpack_string()
    }

    pub fn unpack_bytes(&self) -> &[u8] {
        self.unpack_object().unpack_bytes()
    }

    pub fn unpack_array(&self) -> Rc<[Value]> {
        self.unpack_object().unpack_array()
    }

    pub fn unpack_variant(&self) -> (&usize, &Value) {
        self.unpack_object().unpack_variant()
    }

    pub fn unpack_cmd(&self) -> &[Rc<str>] {
        self.unpack_object().unpack_cmd()
    }

    pub fn unpack_object(&self) -> &Object {
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

    pub fn unpack_int(&self) -> i32 {
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

    pub fn unpack_unit(&self) {
        match self {
            Value::Unit => (),
            val => panic!("expected char, got {:?}", val),
        }
    }

    pub fn unpack_record(&self) -> Rc<[Value]> {
        self.unpack_object().unpack_record()
    }

    pub fn render(&self) -> String {
        match self {
            Value::True => String::from("true"),
            Value::False => String::from("false"),
            Value::Int(n) => format!("{:?}", n),
            Value::Char(c) => format!("{:?}", c),
            Value::Unit => String::from("()"),
            Value::Object(o) => o.render(),
        }
    }
}

impl PartialEq for Value {
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
            Value::Object(o1) => match other {
                Value::Object(o2) => o1.eq(o2),
                _ => false,
            },
        }
    }
}

pub struct Module {
    pub bindings: Bindings,
}

/**
A module is either a top-level module or a submodule.

A top-level module is identified by a [`ModuleId`], and a submodule
is identified by a path through a top-level module.
*/
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ModulePath {
    pub id: ModuleId,
    pub path: Vec<String>,
}

struct Context {
    /**
    A stack of module scopes.

    The top of the stack is the current module in which evaluation is taking place.
    */
    modules: Vec<ModulePath>,
    base: Bindings,
}

pub struct Interpreter<'io> {
    program: Rc<str>,
    args: &'io [Rc<str>],
    stdin: &'io mut dyn BufRead,
    stdout: &'io mut dyn io::Write,
    stderr: &'io mut dyn io::Write,
    context: Context,
    modules: HashMap<ModuleId, Module>,
}

fn lookup_path<'a>(bindings: &'a mut Bindings, path: &[String]) -> &'a mut Bindings {
    if path.is_empty() {
        bindings
    } else {
        let name = Name::definition(path[0].as_str());

        // This implementation isn't intuitive to me.
        if bindings.contains(&name) {
            let binding = bindings.get(&name).unwrap();
            /*
            I tried to write it like this:

            ```rust
            match bindings.get(&Name::definition(path[0].as_str())) {
                None => panic!("submodule {:?} not found in {:?}", path[0], bindings),
                Some(binding) => match binding {
                    Binding::Expr(expr) => panic!("unexpected expr {:?}", expr),
                    Binding::Module(bindings) => lookup_path(bindings, &path[1..]),
                },
            }
            ```

            But the compiler, as of 1.63.0, thinks that `get`'s exclusive borrow of `bindings` is still
            active in the `None` case and prevents `panic!` from casting `bindings` to a shared reference.

            I think the code I *want* to write requires the "polonius" borrow checker:
            https://smallcultfollowing.com/babysteps/blog/2018/06/15/mir-based-borrow-check-nll-status-update/#polonius
            */

            match binding {
                Binding::Expr(expr) => panic!("unexpected expr {:?}", expr),
                Binding::Module(bindings) => lookup_path(bindings, &path[1..]),
            }
        } else {
            panic!("submodule {:?} not found in {:?}", path[0], bindings)
        }
    }
}

pub struct IO<'io> {
    pub stdin: &'io mut dyn BufRead,
    pub stdout: &'io mut dyn io::Write,
    pub stderr: &'io mut dyn io::Write,
}

impl<'io> Interpreter<'io> {
    pub fn new(
        program: Rc<str>,
        args: &'io [Rc<str>],
        io: IO<'io>,
        common_kinds: &CommonKinds,
        modules: &Modules<core::Module>,
        context: &HashMap<Name, ipso_core::Binding>,
    ) -> Self {
        let modules = modules
            .iter_ids()
            .map(|(module_id, module)| {
                (
                    module_id,
                    Module {
                        bindings: Bindings::from(module.get_bindings(common_kinds)),
                    },
                )
            })
            .collect();

        Interpreter {
            program,
            args,
            stdin: io.stdin,
            stdout: io.stdout,
            stderr: io.stderr,
            context: Context {
                modules: Vec::new(),
                base: Bindings::from(context.clone()),
            },
            modules,
        }
    }

    pub fn alloc(&self, obj: Object) -> Value {
        Value::Object(Rc::new(obj))
    }

    pub fn alloc_str(&self, s: &str) -> Rc<str> {
        Rc::from(s)
    }

    pub fn alloc_bytes<I: IntoIterator<Item = u8>>(&self, s: I) -> Rc<[u8]> {
        let bytes: Vec<u8> = s.into_iter().collect();
        Rc::from(bytes)
    }

    pub fn alloc_values<I: IntoIterator<Item = Value>>(&self, vals: I) -> Rc<[Value]>
where {
        let values: Vec<Value> = vals.into_iter().collect();
        Rc::from(values)
    }

    #[inline(always)]
    pub fn reflect<T: Reflect>(&mut self, value: T) -> Value {
        value.reflect(self)
    }

    #[inline(always)]
    pub fn reflect_ref<T: ReflectRef + ?Sized>(&mut self, value: &T) -> Value {
        value.reflect_ref(self)
    }

    pub fn eval_builtin(&self, name: &Builtin) -> Value {
        match name {
            Builtin::Pure => {
                function1!(
                    pure,
                    self,
                    |interpreter: &mut Interpreter, env: Rc<[Value]>, arg: Value| {
                        fn pure_io_1(_: &mut Interpreter, env: Rc<[Value]>) -> Value {
                            env[0].clone()
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(pure_io_1),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::MapIO => {
                function2!(
                    map_io,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        fn map_io_2(interpreter: &mut Interpreter, env: Rc<[Value]>) -> Value {
                            let f = env[0].clone();
                            let io_a = env[1].clone();
                            let a = io_a.perform_io(interpreter); // type: a
                            f.apply(interpreter, a) // type: b
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(map_io_2),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::BindIO => {
                function2!(
                    bind_io,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        fn bind_io_2(interpreter: &mut Interpreter, env: Rc<[Value]>) -> Value {
                            let io_a = env[0].clone();
                            let f = env[1].clone();
                            let a = io_a.perform_io(interpreter); // type: a
                            let io_b = f.apply(interpreter, a); // type: IO b
                            io_b.perform_io(interpreter) // type: b
                        }
                        let env = interpreter.alloc_values({
                            let mut new_env = Vec::with_capacity(env.len() + 1);
                            new_env.extend_from_slice(env.as_ref());
                            new_env.push(arg);
                            new_env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(bind_io_2),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::Trace => {
                function2!(
                    trace,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        writeln!(interpreter.stdout, "trace: {}", env[0].render()).unwrap();
                        arg
                    }
                )
            }
            Builtin::ToUtf8 => {
                function1!(
                    to_utf8,
                    self,
                    |interpreter: &mut Interpreter<'_>, _: Rc<[Value]>, arg: Value| {
                        let a = arg.unpack_string();
                        let bytes: Rc<[u8]> = Rc::from(a.as_bytes());
                        interpreter.reflect(bytes)
                    }
                )
            }
            Builtin::Println => {
                function1!(
                    println,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        fn println(interpreter: &mut Interpreter, env: Rc<[Value]>) -> Value {
                            // env[0] : String
                            let value = env[0].unpack_string();
                            writeln!(interpreter.stdout, "{}", value)
                                .expect("writing to stdout failed");
                            Value::Unit
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });

                        interpreter.alloc(Object::IO {
                            env,
                            body: IOBody(println),
                        })
                    }
                )
            }
            Builtin::Print => {
                function1!(
                    print,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        fn print(interpreter: &mut Interpreter, env: Rc<[Value]>) -> Value {
                            // env[0] : String
                            let value = env[0].unpack_string();
                            {
                                write!(interpreter.stdout, "{}", value)
                                    .expect("writing to stdout failed");
                                interpreter.stdout.flush().expect("flushing stdout failed");
                            }
                            Value::Unit
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });

                        interpreter.alloc(Object::IO {
                            env,
                            body: IOBody(print),
                        })
                    }
                )
            }
            Builtin::Readln => {
                fn readln(interpreter: &mut Interpreter, _: Rc<[Value]>) -> Value {
                    // env[0] : Stdin
                    let mut str = String::new();
                    let _ = interpreter.stdin.read_line(&mut str).unwrap();
                    let str: Rc<str> = interpreter.alloc_str(&str);
                    interpreter.reflect(str)
                }

                self.alloc(Object::IO {
                    env: Rc::from([]),
                    body: IOBody(readln),
                })
            }
            Builtin::EqString => {
                function2!(
                    eq_string,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let a = env[0].unpack_string();
                        let b = arg.unpack_string();
                        interpreter.reflect(a == b)
                    }
                )
            }
            Builtin::CompareString => {
                function2!(
                    compare_string,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let a = env[0].unpack_string();
                        let b = arg.unpack_string();
                        interpreter.reflect(a.as_ref().cmp(b.as_ref()))
                    }
                )
            }
            Builtin::EqInt => {
                function2!(
                    eq_int,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let a = env[0].unpack_int();
                        let b = arg.unpack_int();
                        interpreter.reflect(a == b)
                    }
                )
            }
            Builtin::CompareInt => {
                function2!(
                    compare_int,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let a = env[0].unpack_int();
                        let b = arg.unpack_int();
                        interpreter.reflect(a.cmp(&b))
                    }
                )
            }
            Builtin::IntToString => {
                function1!(
                    int_to_string,
                    self,
                    |eval: &mut Interpreter<'_>, _env: Rc<[Value]>, arg: Value| {
                        let a = arg.unpack_int();
                        let str: Rc<str> = eval.alloc_str(&format!("{}", a));
                        eval.reflect(str)
                    }
                )
            }
            Builtin::EqArray => {
                function3!(
                    eq_array,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let f = env[0].clone();
                        let a = env[1].unpack_array();
                        let b = arg.unpack_array();

                        let mut acc = Value::True;
                        if a.len() == b.len() {
                            for (a, b) in a.iter().zip(b.iter()) {
                                let res = f
                                    .apply(eval, a.clone())
                                    .apply(eval, b.clone())
                                    .unpack_bool();
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
                )
            }
            Builtin::CompareArray => {
                function3!(
                    compare_array,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let f = env[0].clone();
                        let a = env[1].unpack_array();
                        let b = arg.unpack_array();

                        let a_len = a.len();
                        let b_len = b.len();
                        let mut index = 0;
                        let mut ordering = Ordering::Equal;

                        loop {
                            fn unpack_ordering(value: &Value) -> Ordering {
                                let (tag, _) = value.unpack_variant();
                                match tag {
                                    // Equal () : (| Equal : (), Greater : (), Less : () |)
                                    0 => Ordering::Equal,
                                    // Greater () : (| Equal : (), Greater : (), Less : () |)
                                    1 => Ordering::Greater,
                                    // Less () : (| Equal : (), Greater : (), Less : () |)
                                    2 => Ordering::Less,
                                    tag => panic!("unexpected tag {}", tag),
                                }
                            }

                            if index < a_len {
                                if index < b_len {
                                    // precondition: a[0..index] == b[0..index]
                                    match unpack_ordering(
                                        &f.apply(interpreter, a[index].clone())
                                            .apply(interpreter, b[index].clone()),
                                    ) {
                                        Ordering::Less => {
                                            ordering = Ordering::Less;
                                            break;
                                        }
                                        Ordering::Equal => {
                                            index += 1;
                                            continue;
                                        }
                                        Ordering::Greater => {
                                            ordering = Ordering::Greater;
                                            break;
                                        }
                                    }
                                } else {
                                    // `a` contains more elements than `b` and we have reached the
                                    // end of `b`
                                    ordering = Ordering::Greater;
                                    break;
                                }
                            } else {
                                // we have reached the end of `a`
                                if index < b_len {
                                    // and `b` still has more elements
                                    ordering = Ordering::Less;
                                } else {
                                    // and we have reached the end of `b`
                                }
                                break;
                            }
                        }
                        interpreter.reflect(ordering)
                    }
                )
            }
            Builtin::FoldlArray => {
                function3!(
                    foldl_array,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let f = env[0].clone();
                        let z = env[1].clone();
                        let arr = arg.unpack_array();

                        let mut acc = z;
                        for el in arr.as_ref() {
                            acc = f.apply(eval, acc).apply(eval, el.clone());
                        }
                        acc
                    }
                )
            }
            Builtin::GenerateArray => {
                function2!(
                    generate_array,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let len = env[0].unpack_int();
                        let f = arg;

                        let mut array = Vec::with_capacity(len as usize);
                        for ix in 0..len {
                            let ix = Value::Int(ix);
                            array.push(f.apply(eval, ix));
                        }

                        let array: Rc<[Value]> = eval.alloc_values(array);
                        eval.reflect(array)
                    }
                )
            }
            Builtin::LengthArray => {
                function1!(
                    length_array,
                    self,
                    |interpreter: &mut Interpreter<'_>, _env: Rc<[Value]>, arg: Value| {
                        let arr = arg.unpack_array();
                        interpreter.reflect(arr.len() as i32)
                    }
                )
            }
            Builtin::IndexArray => {
                function2!(
                    index_array,
                    self,
                    |_eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let ix = env[0].unpack_int() as usize;
                        let arr = arg.unpack_array();
                        arr[ix].clone()
                    }
                )
            }
            Builtin::SliceArray => {
                function3!(
                    slice_array,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let start = env[0].unpack_int() as usize;
                        let len = env[1].unpack_int() as usize;
                        let array = arg.unpack_array();
                        eval.reflect_ref(&array[start..start + len])
                    }
                )
            }
            Builtin::FilterString => {
                function2!(
                    filter_string,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let predicate = env[0].clone();
                        let string = arg.unpack_string();
                        let new_string: String = string
                            .chars()
                            .filter(|c| {
                                let c_val = eval.reflect(c);
                                predicate.apply(eval, c_val).unpack_bool()
                            })
                            .collect();
                        eval.reflect(new_string)
                    }
                )
            }
            Builtin::EqChar => {
                function2!(
                    eq_char,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let c1 = env[0].unpack_char();
                        let c2 = arg.unpack_char();
                        interpreter.reflect(c1 == c2)
                    }
                )
            }
            Builtin::CompareChar => {
                function2!(
                    compare_char,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let c1 = env[0].unpack_char();
                        let c2 = arg.unpack_char();
                        interpreter.reflect(c1.cmp(&c2))
                    }
                )
            }
            Builtin::StringSplit => {
                function2!(
                    string_split,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let sep = env[0].unpack_string();
                        let s = arg.unpack_string();
                        eval.reflect(s.split(sep.as_ref()).collect::<Vec<_>>())
                    }
                )
            }
            Builtin::StringSplitc => {
                function2!(
                    string_splitc,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let c = env[0].unpack_char();
                        let s = arg.unpack_string();
                        eval.reflect(s.split(c).collect::<Vec<_>>())
                    }
                )
            }
            Builtin::JoinString => {
                function2!(
                    join_string,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let sep = env[0].unpack_string();
                        let strings = arg.unpack_array();
                        if strings.is_empty() {
                            eval.reflect("")
                        } else {
                            let mut joined = String::from(strings[0].unpack_string().as_ref());
                            for string in &strings[1..] {
                                joined.push_str(sep.as_ref());
                                joined.push_str(string.unpack_string().as_ref());
                            }
                            eval.reflect(joined)
                        }
                    }
                )
            }
            Builtin::StringParts => {
                struct Parts<'a> {
                    delimiter: &'a str,
                    string: &'a str,
                }

                impl<'a> Iterator for Parts<'a> {
                    type Item = &'a str;

                    fn next(&mut self) -> Option<&'a str> {
                        let trimmed = self.string.trim_start_matches(self.delimiter);

                        if trimmed.is_empty() {
                            None
                        } else {
                            match trimmed.split_once(self.delimiter) {
                                Some((prefix, suffix)) => {
                                    self.string = suffix;
                                    Some(prefix)
                                }
                                None => {
                                    self.string = "";
                                    Some(trimmed)
                                }
                            }
                        }
                    }
                }

                function2!(
                    string_parts,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let sep = env[0].unpack_string();
                        let s = arg.unpack_string();

                        if sep.is_empty() {
                            eval.stderr
                                .write_all(b"error: string.parts called with empty delimiter\n")
                                .unwrap();
                            std::process::exit(1)
                        } else {
                            eval.reflect(
                                Parts {
                                    delimiter: sep.as_ref(),
                                    string: s.as_ref(),
                                }
                                .collect::<Vec<_>>(),
                            )
                        }
                    }
                )
            }
            Builtin::StringPartsc => {
                struct Partsc<'a> {
                    delimiter: char,
                    string: &'a str,
                }

                impl<'a> Iterator for Partsc<'a> {
                    type Item = &'a str;

                    fn next(&mut self) -> Option<&'a str> {
                        let trimmed = self.string.trim_start_matches(self.delimiter);

                        if trimmed.is_empty() {
                            None
                        } else {
                            match trimmed.split_once(self.delimiter) {
                                Some((prefix, suffix)) => {
                                    self.string = suffix;
                                    Some(prefix)
                                }
                                None => {
                                    self.string = "";
                                    Some(trimmed)
                                }
                            }
                        }
                    }
                }
                function2!(
                    string_partsc,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let c = env[0].unpack_char();
                        let s = arg.unpack_string();
                        eval.reflect(
                            Partsc {
                                delimiter: c,
                                string: s.as_ref(),
                            }
                            .collect::<Vec<_>>(),
                        )
                    }
                )
            }
            Builtin::StringTrimp => {
                function2!(
                    string_trimp,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let predicate = &env[0];
                        let input_string = arg.unpack_string();
                        let output_string: &str = input_string.trim_matches(|c: char| {
                            let c = eval.reflect(c);
                            predicate.apply(eval, c).unpack_bool()
                        });
                        eval.reflect(output_string)
                    }
                )
            }
            Builtin::StringTrimc => {
                function2!(
                    string_trimc,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let c: char = env[0].unpack_char();
                        let input_string = arg.unpack_string();
                        let output_string: &str = input_string.trim_matches(c);
                        eval.reflect(output_string)
                    }
                )
            }
            Builtin::StringTrim => {
                function1!(
                    string_trim,
                    self,
                    |eval: &mut Interpreter<'_>, _env: Rc<[Value]>, arg: Value| {
                        let input_string = arg.unpack_string();
                        let output_string: &str = input_string.trim();
                        eval.reflect(output_string)
                    }
                )
            }
            Builtin::FoldlString => {
                function3!(
                    foldl_string,
                    self,
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let f = env[0].clone();
                        let mut acc = env[1].clone();
                        let s = arg.unpack_string();
                        for c in s.chars() {
                            let c_value = eval.reflect(c);
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
                    |eval: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let array = env[0].unpack_array();
                        let new_array: Rc<[Value]> = eval.alloc_values({
                            let mut new_array = Vec::from(array.as_ref());
                            new_array.push(arg);
                            new_array
                        });
                        eval.reflect(new_array)
                    }
                )
            }
            Builtin::Run => {
                function1!(
                    run,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        fn run_1(_: &mut Interpreter, env: Rc<[Value]>) -> Value {
                            let cmd = env[0].unpack_cmd();
                            if cmd.is_empty() {
                                Value::Unit
                            } else {
                                let status = process::Command::new(cmd[0].as_ref())
                                    .args(
                                        cmd[1..].iter().map(|arg| arg.as_ref()).collect::<Vec<_>>(),
                                    )
                                    .status()
                                    .unwrap_or_else(|err| {
                                        println!("failed to start process {:?}: {}", cmd[0], err);
                                        process::exit(1);
                                    });

                                check_exit_status(&cmd[0], &status);

                                Value::Unit
                            }
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(run_1),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::EqBool => function2!(
                eq_bool,
                self,
                |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                    let a = env[0].unpack_bool();
                    let b = arg.unpack_bool();
                    interpreter.reflect(a == b)
                }
            ),
            Builtin::Lines => function1!(
                lines,
                self,
                |interpreter: &mut Interpreter<'_>, _: Rc<[Value]>, arg: Value| {
                    fn lines_io_body(interpreter: &mut Interpreter<'_>, env: Rc<[Value]>) -> Value {
                        let cmd: &[Rc<str>] = env[0].unpack_cmd();
                        if cmd.is_empty() {
                            Value::Unit
                        } else {
                            let output = process::Command::new(cmd[0].as_ref())
                                .args(cmd[1..].iter().map(|arg| arg.as_ref()))
                                .stdin(Stdio::inherit())
                                .stdout(Stdio::piped())
                                .stderr(Stdio::inherit())
                                .output()
                                .unwrap_or_else(|err| {
                                    panic!("failed to start process {:?}: {}", cmd[0], err)
                                });

                            check_exit_status(&cmd[0], &output.status);

                            interpreter.reflect(
                                output
                                    .stdout
                                    .lines()
                                    .map(|line: Result<String, _>| {
                                        line.unwrap_or_else(|err| {
                                            panic!("failed to decode line: {}", err)
                                        })
                                    })
                                    .collect::<Vec<_>>(),
                            )
                        }
                    }

                    let env = interpreter.alloc_values([arg]);
                    interpreter.alloc(Object::IO {
                        env,
                        body: IOBody(lines_io_body),
                    })
                }
            ),
            Builtin::CmdRead => function1!(
                cmd_read,
                self,
                |interpreter: &mut Interpreter<'_>, _: Rc<[Value]>, arg: Value| {
                    fn cmd_read_io(interpreter: &mut Interpreter<'_>, env: Rc<[Value]>) -> Value {
                        let cmd: &[Rc<str>] = env[0].unpack_cmd();
                        if cmd.is_empty() {
                            interpreter.reflect("")
                        } else {
                            let output = process::Command::new(cmd[0].as_ref())
                                .args(cmd[1..].iter().map(|arg| arg.as_ref()))
                                .stdin(Stdio::inherit())
                                .stdout(Stdio::piped())
                                .stderr(Stdio::inherit())
                                .output()
                                .unwrap_or_else(|err| {
                                    panic!("failed to start process {:?}: {}", cmd[0], err)
                                });

                            check_exit_status(&cmd[0], &output.status);

                            let line: &str = std::str::from_utf8(&output.stdout)
                                .unwrap_or_else(|err| panic!("{:?}", err));

                            interpreter.reflect(line)
                        }
                    }

                    let env = interpreter.alloc_values([arg]);
                    interpreter.alloc(Object::IO {
                        env,
                        body: IOBody(cmd_read_io),
                    })
                }
            ),
            Builtin::ShowCmd => function1!(
                show_cmd,
                self,
                |interpreter: &mut Interpreter<'_>, _: Rc<[Value]>, arg: Value| {
                    let cmd = arg
                        .unpack_cmd()
                        .iter()
                        .map(|value| {
                            if value.contains(' ') {
                                let mut string = String::with_capacity(value.len() + 2);

                                string.push('"');
                                value.chars().for_each(|c| {
                                    if c == '"' {
                                        string.push('\\');
                                    }
                                    string.push(c);
                                });
                                string.push('"');

                                string
                            } else {
                                String::from(value.as_ref())
                            }
                        })
                        .collect::<Vec<String>>()
                        .join(" ");
                    interpreter.reflect(cmd.as_str())
                }
            ),
            Builtin::DebugCmd => function1!(
                debug_cmd,
                self,
                |interpreter: &mut Interpreter<'_>, _: Rc<[Value]>, arg: Value| {
                    fn add_part(buffer: &mut String, value: &str) {
                        if value.contains(' ') {
                            buffer.reserve(2 + value.len());

                            buffer.push('"');
                            value.chars().for_each(|c| {
                                if c == '"' {
                                    buffer.push('\\');
                                }
                                buffer.push(c);
                            });
                            buffer.push('"');
                        } else {
                            buffer.reserve(value.len());

                            value.chars().for_each(|c| {
                                if c == '`' {
                                    buffer.push('\\');
                                }
                                buffer.push(c);
                            });
                        }
                    }

                    let mut buffer = String::from("`");

                    let mut cmd_parts = arg.unpack_cmd().iter();
                    if let Some(part) = cmd_parts.next() {
                        add_part(&mut buffer, part)
                    }
                    cmd_parts.for_each(|value| {
                        buffer.push(' ');
                        add_part(&mut buffer, value);
                    });

                    buffer.push('`');

                    interpreter.reflect(buffer)
                }
            ),
            Builtin::CmdTry => function1!(
                cmd_try,
                self,
                |interpreter: &mut Interpreter<'_>, _: Rc<[Value]>, arg: Value| {
                    fn cmd_try_io(interpreter: &mut Interpreter<'_>, env: Rc<[Value]>) -> Value {
                        /*
                        Return type: (| Success : (), Failure : Int |)

                        Tags are alphabetically ordered:
                        * Failure tag is 0
                        * Success tag is 1
                        */

                        fn mk_failure(interpreter: &mut Interpreter<'_>, arg: Value) -> Value {
                            interpreter.alloc(Object::Variant(0, arg))
                        }

                        fn mk_success(interpreter: &mut Interpreter<'_>) -> Value {
                            interpreter.alloc(Object::Variant(1, Value::Unit))
                        }

                        let cmd: &[Rc<str>] = env[0].unpack_cmd();
                        if cmd.is_empty() {
                            mk_success(interpreter)
                        } else {
                            let output = process::Command::new(cmd[0].as_ref())
                                .args(cmd[1..].iter().map(|arg| arg.as_ref()))
                                .stdin(Stdio::inherit())
                                .stdout(Stdio::inherit())
                                .stderr(Stdio::inherit())
                                .output()
                                .unwrap_or_else(|err| {
                                    panic!("failed to start process {:?}: {}", cmd[0], err)
                                });

                            if output.status.success() {
                                mk_success(interpreter)
                            } else {
                                match output.status.code() {
                                    Some(code) => mk_failure(interpreter, Value::Int(code)),
                                    None => {
                                        eprintln!(
                                            "process {:?} exited unexpectedly: {}",
                                            cmd[0], output.status,
                                        );
                                        std::process::exit(1)
                                    }
                                }
                            }
                        }
                    }

                    let env = interpreter.alloc_values([arg]);
                    interpreter.alloc(Object::IO {
                        env,
                        body: IOBody(cmd_try_io),
                    })
                }
            ),
            Builtin::FlatMap => function2!(
                flat_map,
                self,
                |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                    let f = env[0].clone();
                    let xs = arg.unpack_array();

                    let mut result: Vec<Value> = Vec::new();
                    for x in xs.as_ref() {
                        result.extend(
                            f.apply(interpreter, x.clone())
                                .unpack_array()
                                .as_ref()
                                .iter()
                                .cloned(),
                        )
                    }

                    interpreter.reflect(result)
                }
            ),
            Builtin::MapArray => function2!(
                map_array,
                self,
                |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                    let f = env[0].clone();
                    let xs = arg.unpack_array();

                    let result: Vec<Value> = xs
                        .iter()
                        .map(|x| f.apply(interpreter, x.clone()))
                        .collect::<Vec<_>>();

                    interpreter.reflect(result)
                }
            ),
            Builtin::CharToString => function1!(
                char_to_string,
                self,
                |interpreter: &mut Interpreter<'_>, _: Rc<[Value]>, arg: Value| {
                    let char = arg.unpack_char();
                    let string: String = format!("{:?}", char);
                    interpreter.reflect(string)
                }
            ),
            Builtin::DebugString => function1!(
                debug_string,
                self,
                |interpreter: &mut Interpreter<'_>, _: Rc<[Value]>, arg: Value| {
                    let string = arg.unpack_string();
                    let new_string: String = format!("{:?}", string);
                    interpreter.reflect(new_string)
                }
            ),
            Builtin::ArrayUnfoldr => {
                function2!(
                    array_unfoldr,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let mut s = env[0].clone();
                        let f = arg;

                        let mut array: Vec<Value> = Vec::new();
                        loop {
                            // (| Step : ..., Skip : ..., Done : ... |)
                            let value = f.apply(interpreter, s);
                            let (tag, value) = value.unpack_variant();

                            match tag {
                                // Done
                                0 => {
                                    debug_assert!(*value == Value::Unit);
                                    break;
                                }

                                // Skip
                                1 => {
                                    // value : { next : s }
                                    let value = value.unpack_record();
                                    s = value[0].clone();
                                }

                                // Step
                                2 => {
                                    // value : { value : a, next : s }
                                    let value = value.unpack_record();
                                    s = value[0].clone();
                                    array.push(value[1].clone())
                                }

                                _ => unreachable!(),
                            }
                        }

                        interpreter.reflect(array)
                    }
                )
            }
            Builtin::IntMod => {
                function2!(
                    int_mod,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let a = env[0].unpack_int();
                        let b = arg.unpack_int();
                        interpreter.reflect(a % b)
                    }
                )
            }
            Builtin::PathExists => {
                function1!(
                    path_exists,
                    self,
                    |interpreter: &mut Interpreter, env: Rc<[Value]>, arg: Value| {
                        fn path_exists_io_1(
                            interpreter: &mut Interpreter,
                            env: Rc<[Value]>,
                        ) -> Value {
                            let path = env[0].unpack_string();
                            interpreter.reflect(Path::new(path.as_ref()).exists())
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(path_exists_io_1),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::EnvProgram => {
                fn env_program_io(interpreter: &mut Interpreter, _: Rc<[Value]>) -> Value {
                    interpreter.reflect(interpreter.program.clone())
                }
                let closure = Object::IO {
                    env: Rc::from([]),
                    body: IOBody(env_program_io),
                };
                self.alloc(closure)
            }
            Builtin::EnvArgs => {
                fn env_args_io(interpreter: &mut Interpreter, _: Rc<[Value]>) -> Value {
                    interpreter.reflect(interpreter.args)
                }
                let closure = Object::IO {
                    env: Rc::from([]),
                    body: IOBody(env_args_io),
                };
                self.alloc(closure)
            }
            Builtin::EnvGetvar => {
                function1!(
                    env_getvar,
                    self,
                    |interpreter: &mut Interpreter, env: Rc<[Value]>, arg: Value| {
                        fn env_getvar_io(interpreter: &mut Interpreter, env: Rc<[Value]>) -> Value {
                            let var = env[0].unpack_string();
                            match std::env::var(var.as_ref()) {
                                Err(err) => match err {
                                    std::env::VarError::NotPresent => {
                                        // None () : (| None : (), Some : String |)
                                        interpreter.reflect(None::<String>)
                                    }
                                    err => {
                                        panic!("getvar: {}", err)
                                    }
                                },
                                Ok(value) => {
                                    // Some value : (| None : (), Some : String |)
                                    interpreter.reflect(Some(value))
                                }
                            }
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(env_getvar_io),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::EnvGetvarBang => {
                function1!(
                    env_getvarbang,
                    self,
                    |interpreter: &mut Interpreter, env: Rc<[Value]>, arg: Value| {
                        fn env_getvarbang_io(
                            interpreter: &mut Interpreter,
                            env: Rc<[Value]>,
                        ) -> Value {
                            let var = env[0].unpack_string();
                            match std::env::var(var.as_ref()) {
                                Err(err) => match err {
                                    std::env::VarError::NotPresent => {
                                        write!(
                                            interpreter.stderr,
                                            "getvar!: missing environment variable {}",
                                            var
                                        )
                                        .unwrap();
                                        std::process::exit(1)
                                    }
                                    err => {
                                        panic!("getvar!: {}", err)
                                    }
                                },
                                Ok(value) => interpreter.reflect(value),
                            }
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(env_getvarbang_io),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::EnvSetvar => {
                function2!(
                    env_setvar,
                    self,
                    |interpreter: &mut Interpreter, env: Rc<[Value]>, arg: Value| {
                        fn env_setvar_io(_: &mut Interpreter, env: Rc<[Value]>) -> Value {
                            let key = env[0].unpack_string();
                            let value = env[1].unpack_string();
                            std::env::set_var(key.as_ref(), value.as_ref());
                            Value::Unit
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(env_setvar_io),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::ExitSuccess => {
                fn exit_success_io(_: &mut Interpreter, _: Rc<[Value]>) -> Value {
                    std::process::exit(0)
                }
                let closure = Object::IO {
                    env: Rc::from([]),
                    body: IOBody(exit_success_io),
                };
                self.alloc(closure)
            }
            Builtin::ExitFailure => {
                fn exit_failure_io(_: &mut Interpreter, _: Rc<[Value]>) -> Value {
                    std::process::exit(1)
                }
                let closure = Object::IO {
                    env: Rc::from([]),
                    body: IOBody(exit_failure_io),
                };
                self.alloc(closure)
            }
            Builtin::ExitWith => {
                function1!(
                    exit_with,
                    self,
                    |interpreter: &mut Interpreter, env: Rc<[Value]>, arg: Value| {
                        fn exit_with_io(_: &mut Interpreter, env: Rc<[Value]>) -> Value {
                            let code: i32 = env[0].unpack_int();
                            std::process::exit(code)
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(exit_with_io),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::FileRead => function1!(
                file_read,
                self,
                |interpreter: &mut Interpreter<'_>, _: Rc<[Value]>, arg: Value| {
                    fn file_read_io(interpreter: &mut Interpreter<'_>, env: Rc<[Value]>) -> Value {
                        let path = env[0].unpack_string();
                        let contents = std::fs::read_to_string(path.as_ref())
                            .unwrap_or_else(|err| panic!("{}", err));
                        interpreter.reflect(contents)
                    }

                    let env = interpreter.alloc_values([arg]);
                    interpreter.alloc(Object::IO {
                        env,
                        body: IOBody(file_read_io),
                    })
                }
            ),
            Builtin::FileWrite => {
                function2!(
                    file_write,
                    self,
                    |interpreter: &mut Interpreter, env: Rc<[Value]>, arg: Value| {
                        fn file_write_io(_: &mut Interpreter, env: Rc<[Value]>) -> Value {
                            let path = env[0].unpack_string();
                            let content = env[1].unpack_string();
                            std::fs::write(path.as_ref(), content.as_ref())
                                .unwrap_or_else(|err| panic!("{}", err));
                            Value::Unit
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(file_write_io),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::FileAppend => {
                function2!(
                    file_append,
                    self,
                    |interpreter: &mut Interpreter, env: Rc<[Value]>, arg: Value| {
                        fn file_append_io(_: &mut Interpreter, env: Rc<[Value]>) -> Value {
                            let path = env[0].unpack_string();
                            let content = env[1].unpack_string();
                            let mut file = std::fs::OpenOptions::new()
                                .create(true)
                                .append(true)
                                .open(path.as_ref())
                                .unwrap_or_else(|err| panic!("{}", err));
                            file.write_all(content.as_bytes())
                                .unwrap_or_else(|err| panic!("{}", err));
                            Value::Unit
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(file_append_io),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::ArrayEach_ => {
                function2!(
                    array_each_,
                    self,
                    |interpreter: &mut Interpreter, env: Rc<[Value]>, arg: Value| {
                        #[allow(non_snake_case)]
                        fn array_each__io(
                            interpreter: &mut Interpreter,
                            env: Rc<[Value]>,
                        ) -> Value {
                            let array = env[0].unpack_array();
                            let func = &env[1];
                            for item in array.iter() {
                                func.apply(interpreter, item.clone())
                                    .perform_io(interpreter);
                            }
                            Value::Unit
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(array_each__io),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::CmdEachline_ => {
                function2!(
                    cmd_eachline_,
                    self,
                    |interpreter: &mut Interpreter, env: Rc<[Value]>, arg: Value| {
                        #[allow(non_snake_case)]
                        fn cmd_eachline__io(
                            interpreter: &mut Interpreter,
                            env: Rc<[Value]>,
                        ) -> Value {
                            let cmd = env[0].unpack_cmd();
                            let func = &env[1];
                            if let Some(command) = cmd.get(0) {
                                let mut process = Command::new(command.as_ref())
                                    .args(
                                        cmd[1..].iter().map(|arg| arg.as_ref()).collect::<Vec<_>>(),
                                    )
                                    .stdin(Stdio::inherit())
                                    .stdout(Stdio::piped())
                                    .stderr(Stdio::inherit())
                                    .spawn()
                                    .unwrap_or_else(|err| {
                                        panic!("{} failed to start: {}", command, err)
                                    });

                                match &mut process.stdout {
                                    None => unreachable!("no handle for process stdout"),
                                    Some(stdout) => {
                                        let mut stdout = BufReader::new(stdout);
                                        let mut buffer = String::new();
                                        loop {
                                            buffer.clear();
                                            match stdout.read_line(&mut buffer) {
                                                Err(err) => panic!("failed to read line: {}", err),
                                                Ok(bytes_read) => {
                                                    if bytes_read == 0 {
                                                        break;
                                                    } else {
                                                        let stripped = buffer
                                                            .strip_suffix('\n')
                                                            .unwrap_or(&buffer);
                                                        let line = interpreter.reflect(stripped);
                                                        func.apply(interpreter, line)
                                                            .perform_io(interpreter);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }

                                match process.wait() {
                                    Err(err) => {
                                        panic!("waiting on child failed: {}", err)
                                    }
                                    Ok(status) => {
                                        if !status.success() {
                                            match status.code() {
                                                None => {
                                                    panic!("{} failed due to a signal", command)
                                                }
                                                Some(n) => {
                                                    panic!("{} failed with status {}", command, n)
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            Value::Unit
                        }
                        let env = interpreter.alloc_values({
                            let mut env = Vec::from(env.as_ref());
                            env.push(arg);
                            env
                        });
                        let closure = Object::IO {
                            env,
                            body: IOBody(cmd_eachline__io),
                        };
                        interpreter.alloc(closure)
                    }
                )
            }
            Builtin::StringToChars => {
                function1!(
                    string_to_chars,
                    self,
                    |interpreter: &mut Interpreter, _env: Rc<[Value]>, arg: Value| {
                        let string = arg.unpack_string();
                        let chars: Vec<char> = string.chars().collect::<Vec<_>>();
                        interpreter.reflect(chars)
                    }
                )
            }
            Builtin::StringFromChars => {
                function1!(
                    string_from_chars,
                    self,
                    |interpreter: &mut Interpreter, _env: Rc<[Value]>, arg: Value| {
                        let chars = arg.unpack_array();
                        let string = chars.iter().map(|c| c.unpack_char()).collect::<String>();
                        interpreter.reflect(string)
                    }
                )
            }
            Builtin::StringStripPrefix => {
                function2!(
                    string_strip_prefix,
                    self,
                    |interpreter: &mut Interpreter, env: Rc<[Value]>, arg: Value| {
                        let prefix = env[0].unpack_string();
                        let string = arg.unpack_string();
                        interpreter.reflect(string.strip_prefix(prefix.as_ref()))
                    }
                )
            }
            Builtin::StringStartsWith => {
                function2!(
                    string_starts_with,
                    self,
                    |interpreter: &mut Interpreter, env: Rc<[Value]>, arg: Value| {
                        let prefix = env[0].unpack_string();
                        let string = arg.unpack_string();
                        interpreter.reflect(string.starts_with(prefix.as_ref()))
                    }
                )
            }
            Builtin::ArrayFilter => {
                function2!(
                    array_filter,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let predicate = &env[0];
                        let array = arg.unpack_array();
                        let new_array: Vec<Value> = array
                            .iter()
                            .cloned()
                            .filter(|item| predicate.apply(interpreter, item.clone()).unpack_bool())
                            .collect::<Vec<_>>();
                        interpreter.reflect(new_array)
                    }
                )
            }
            Builtin::ArrayFilterMap => {
                function2!(
                    array_filter_map,
                    self,
                    |interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value| {
                        let predicate = &env[0];
                        let array = arg.unpack_array();
                        let new_array: Vec<Value> = array
                            .iter()
                            .cloned()
                            .filter_map(|item| {
                                match predicate.apply(interpreter, item).unpack_variant() {
                                    // None () : (| None : (), Some : b |)
                                    (0, value) => {
                                        value.unpack_unit();
                                        None
                                    }
                                    // Some value : (| None : (), Some : b |)
                                    (1, value) => Some(value.clone()),
                                    value => panic!("expected tag 0 or 1, got: {:?}", value),
                                }
                            })
                            .collect::<Vec<_>>();
                        interpreter.reflect(new_array)
                    }
                )
            }
        }
    }

    fn current_bindings(&mut self) -> &mut Bindings {
        match self.context.modules.last() {
            Some(module_path) => lookup_path(
                &mut self.modules.get_mut(&module_path.id).unwrap().bindings,
                &module_path.path,
            ),
            None => &mut self.context.base,
        }
    }

    pub fn eval_from_module(
        &mut self,
        env: &mut Env,
        id: &ModuleRef,
        path: &[String],
        item: &Name,
    ) -> Value {
        let bindings = match id {
            ModuleRef::This => self.current_bindings(),
            ModuleRef::Id(id) => match self.modules.get_mut(id) {
                None => panic!("{:?} not found", id),
                Some(module) => &mut module.bindings,
            },
        };

        let bindings = lookup_path(bindings, path);

        let expr = match bindings.get(item) {
            None => panic!("{:?} not found in {:?}", item, id),
            Some(binding) => match binding {
                Binding::Expr(expr) => expr,
                Binding::Module(module) => panic!("unexpected module {:?}", module),
            },
        };

        if let ModuleRef::Id(id) = id {
            self.context.modules.push(ModulePath {
                id: *id,
                path: Vec::from(path),
            });
        }

        let result = self.eval(env, &expr);

        if let ModuleRef::Id(_) = id {
            self.context.modules.pop();
        }

        result
    }

    fn lookup_name(&mut self, name: &Name) -> Rc<Expr> {
        let binding = match self.context.modules.last() {
            None => match self.context.base.get(name) {
                None => panic!("{:?} not in base scope", name),
                Some(binding) => binding,
            },
            Some(module_path) => match lookup_path(
                &mut self.modules.get_mut(&module_path.id).unwrap().bindings,
                &module_path.path,
            )
            .get(name)
            {
                None => panic!(
                    "{:?} not in scope of {:?}{:?}",
                    name, module_path.id, module_path.path
                ),
                Some(binding) => binding,
            },
        };

        match binding {
            Binding::Expr(body) => body,
            Binding::Module(module) => panic!("unexpected module {:?}", module),
        }
    }

    fn current_module(&self) -> Option<&ModulePath> {
        self.context.modules.last()
    }

    fn eval_string_parts(&mut self, env: &mut Env, parts: &[StringPart<Expr>]) -> String {
        let mut value = String::new();

        for part in parts {
            match part {
                StringPart::Expr(expr) => {
                    let s = self.eval(env, expr);
                    let s = s.unpack_string();
                    value.push_str(s.as_ref());
                }
                StringPart::String(s) => value.push_str(s.as_str()),
            }
        }

        value
    }

    pub fn eval(&mut self, env: &mut Env, expr: &Expr) -> Value {
        fn lookup_index(env: &Env, ix: usize) -> Value {
            let env_len = env.len();
            if ix < env_len {
                env[env_len - 1 - ix].clone()
            } else {
                panic!(
                    "index {} out of bounds. env(len = {}): {:?}",
                    ix, env_len, env
                )
            }
        }

        fn lookup_indices(env: &Env, ixs: &[usize]) -> Rc<[Value]> {
            ixs.iter()
                .copied()
                .map(|ix| lookup_index(env, ix))
                .collect()
        }

        let out = match expr {
            Expr::Var(ix) => lookup_index(env, *ix),
            Expr::Name(name) => {
                let body = self.lookup_name(name);
                self.eval(env, body.as_ref())
            }
            Expr::Module { id, path, item } => self.eval_from_module(env, id, path, item),
            Expr::Builtin(name) => self.eval_builtin(name),

            Expr::App(a, b) => {
                let a = self.eval(env, a);
                let b = self.eval(env, b);
                a.apply(self, b)
            }
            Expr::Lam {
                env: new_env,
                arg,
                body,
            } => self.alloc(Object::Closure {
                env: lookup_indices(env, new_env),
                arg: *arg,
                module: self.current_module().cloned(),
                body: body.clone(),
            }),

            Expr::Let { value, rest } => {
                let value = self.eval(env, value);
                env.push(value);
                let result = self.eval(env, rest);
                env.pop();
                result
            }

            Expr::True => Value::True,
            Expr::False => Value::False,
            Expr::IfThenElse(cond, t, e) => {
                let cond = self.eval(env, cond);
                match cond {
                    Value::True => self.eval(env, t),
                    Value::False => self.eval(env, e),
                    cond => panic!("expected bool, got {:?}", cond),
                }
            }

            Expr::Int(n) => Value::Int(*n),

            Expr::Binop(op, a, b) => match op {
                Binop::Add => {
                    let a = self.eval(env, a).unpack_int();
                    let b = self.eval(env, b).unpack_int();
                    Value::Int(a + b)
                }
                Binop::Multiply => {
                    let a = self.eval(env, a).unpack_int();
                    let b = self.eval(env, b).unpack_int();
                    Value::Int(a * b)
                }
                Binop::Subtract => {
                    let a = self.eval(env, a).unpack_int();
                    let b = self.eval(env, b).unpack_int();
                    Value::Int(a - b)
                }
                Binop::Divide => {
                    let a = self.eval(env, a).unpack_int();
                    let b = self.eval(env, b).unpack_int();
                    Value::Int(a / b)
                }
                Binop::Append => {
                    let a = self.eval(env, a).unpack_array();
                    let b = self.eval(env, b).unpack_array();
                    let c = {
                        let mut c = Vec::with_capacity(a.len() + b.len());
                        c.extend(a.as_ref().iter().cloned());
                        c.extend(b.as_ref().iter().cloned());
                        c
                    };
                    self.reflect(c)
                }
                Binop::Or => {
                    if self.eval(env, a).unpack_bool() {
                        Value::True
                    } else {
                        self.eval(env, b)
                    }
                }
                Binop::And => {
                    if self.eval(env, a).unpack_bool() {
                        self.eval(env, b)
                    } else {
                        Value::False
                    }
                }
                Binop::RApply => {
                    let left = self.eval(env, a);
                    let right = self.eval(env, b);
                    right.apply(self, left)
                }
                Binop::LApply => {
                    let left = self.eval(env, a);
                    let right = self.eval(env, b);
                    left.apply(self, right)
                }
            },

            Expr::Char(c) => Value::Char(*c),

            Expr::String(parts) => {
                let value = self.eval_string_parts(env, parts);
                self.reflect(value)
            }

            Expr::Array(items) => {
                let items: Vec<Value> = items.iter().map(|item| self.eval(env, item)).collect();
                self.reflect(items)
            }

            Expr::Extend(ev, value, rest) => {
                let ix = self.eval(env, ev).unpack_int();
                let value = self.eval(env, value);
                let rest = self.eval(env, rest);
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
                let mut record: Vec<Value> = Vec::with_capacity(fields.len());
                let mut fields: Vec<(i32, Value)> = fields
                    .iter()
                    .map(|(ev, field)| (self.eval(env, ev).unpack_int(), self.eval(env, field)))
                    .collect();
                fields.sort_by_key(|x| x.0);
                for (_index, field) in fields.into_iter() {
                    record.push(field);
                }

                let record = self.alloc_values(record);
                self.alloc(Object::Record(record))
            }
            Expr::Project(expr, index) => {
                let index = self.eval(env, index).unpack_int();
                let expr = self.eval(env, expr);
                match expr.unpack_object() {
                    Object::Record(fields) => fields[index as usize].clone(),
                    expr => panic!("expected record, got {:?}", expr),
                }
            }

            Expr::Variant(tag) => {
                let tag = self.eval(env, tag);
                let env = self.alloc_values(vec![tag]);
                fn code(interpreter: &mut Interpreter<'_>, env: Rc<[Value]>, arg: Value) -> Value {
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
                let tag = self.eval(env, tag).unpack_int() as usize;
                let rest = self.eval(env, rest);
                let (&old_tag, arg) = rest.unpack_variant();
                self.alloc(Object::Variant(
                    if tag <= old_tag { old_tag + 1 } else { old_tag },
                    arg.clone(),
                ))
            }
            Expr::Case(expr, branches) => {
                let expr = self.eval(env, expr);

                struct MatchedBranch<'a> {
                    body: &'a Expr,
                    bound_vars: usize,
                }
                let mut target: Option<MatchedBranch> = None;

                match expr {
                    Value::Object(object) if matches!(object.as_ref(), Object::Variant(_, _)) => {
                        let (tag, value) = match object.as_ref() {
                            Object::Variant(tag, value) => (tag, value),
                            _ => unreachable!(),
                        };

                        /*
                        Because of the way constructors are peeled from variants during
                        pattern matching (see [note: peeling constructors when matching on variants]),
                        the expected tag must change as each branch is checked.
                        */
                        let mut expected_tag: usize = *tag;

                        for branch in branches {
                            match &branch.pattern {
                                Pattern::Variant { tag: branch_tag } => {
                                    let branch_tag =
                                        self.eval(env, branch_tag).unpack_int() as usize;

                                    match expected_tag.cmp(&branch_tag) {
                                        /*
                                        When the expected tag is less than the branch's tag, it
                                        means that the expected constructor comes *before* the
                                        branch's constructor in the lexicographically ordered row
                                        type.

                                        The branch's contructor has no impact on the expected constructor's
                                        position in the row type, so the expected tag doesn't need to be adjusted.

                                        An illustration with an array:

                                        ```
                                        [a, b, c, d]
                                        ```

                                        `b` is at position 1 in the array. If `c` or `d` is removed, `b`'s position
                                        doesn't change, because it preceds `c` and `d`.
                                        */
                                        std::cmp::Ordering::Less => {}

                                        std::cmp::Ordering::Equal => {
                                            env.push(value.clone());
                                            target = Some(MatchedBranch {
                                                body: &branch.body,
                                                bound_vars: 1,
                                            });
                                            break;
                                        }

                                        /*
                                        When the expected tag is less than the branch's tag, it
                                        means that the expected constructor comes *after* the
                                        branch's constructor in the lexicographically ordered row
                                        type.

                                        The branch's contructor influences the expected constructor's
                                        position in the row type, so the expected tag *does* need to be adjusted.

                                        Another array illustration:

                                        ```
                                        [a, b, c, d]
                                        ```

                                        `c` is at position 2 in the array. If `a` or `b` is removed then `c`'s position
                                        changes.
                                        */
                                        std::cmp::Ordering::Greater => {
                                            expected_tag -= 1;
                                        }
                                    }
                                }
                                Pattern::Name => {
                                    env.push(
                                        self.alloc(Object::Variant(expected_tag, value.clone())),
                                    );

                                    target = Some(MatchedBranch {
                                        body: &branch.body,
                                        bound_vars: 1,
                                    });
                                    break;
                                }
                                Pattern::Wildcard => {
                                    target = Some(MatchedBranch {
                                        body: &branch.body,
                                        bound_vars: 0,
                                    });
                                    break;
                                }
                                Pattern::String(_)
                                | Pattern::Unit
                                | Pattern::Int(_)
                                | Pattern::Record { .. }
                                | Pattern::Char(_)
                                | Pattern::Array { .. } => {
                                    panic!("expected variant pattern, got: {:?}", branch.pattern);
                                }
                            }
                        }
                    }
                    _ => {
                        for branch in branches {
                            match &branch.pattern {
                                Pattern::Name => {
                                    env.push(expr);
                                    target = Some(MatchedBranch {
                                        body: &branch.body,
                                        bound_vars: 1,
                                    });
                                    break;
                                }
                                Pattern::Record { names, rest } => {
                                    let fields = expr.unpack_record();
                                    let mut extracted: Vec<usize> = names
                                        .iter()
                                        .map(|name| self.eval(env, name).unpack_int() as usize)
                                        .collect();

                                    let leftover_record = if *rest {
                                        let mut leftover_fields = Rope::from_vec(fields.as_ref());
                                        extracted.sort_unstable();
                                        for ix in extracted.iter().rev() {
                                            leftover_fields = leftover_fields.delete(*ix).unwrap();
                                        }
                                        let leftover_fields =
                                            self.alloc_values(leftover_fields.iter().cloned());

                                        Some(self.alloc(Object::Record(leftover_fields)))
                                    } else {
                                        None
                                    };

                                    let mut bound_vars = 0;
                                    for ix in extracted.iter() {
                                        env.push(fields[*ix].clone());
                                        bound_vars += 1;
                                    }
                                    if let Some(leftover_record) = leftover_record {
                                        env.push(leftover_record);
                                        bound_vars += 1;
                                    };
                                    target = Some(MatchedBranch {
                                        body: &branch.body,
                                        bound_vars,
                                    });
                                    break;
                                }
                                Pattern::Variant { tag: branch_tag } => {
                                    let (tag, value) = expr.unpack_variant();
                                    let branch_tag =
                                        self.eval(env, branch_tag).unpack_int() as usize;
                                    if *tag == branch_tag {
                                        env.push(value.clone());
                                        target = Some(MatchedBranch {
                                            body: &branch.body,
                                            bound_vars: 1,
                                        });
                                        break;
                                    }
                                }
                                Pattern::Array { names } => {
                                    let array = expr.unpack_array();
                                    if array.len() == *names {
                                        let mut bound_vars = 0;
                                        array.iter().for_each(|item| {
                                            env.push(item.clone());
                                            bound_vars += 1;
                                        });
                                        target = Some(MatchedBranch {
                                            body: &branch.body,
                                            bound_vars,
                                        });
                                        break;
                                    }
                                }
                                Pattern::Char(actual_char) => {
                                    let expected_char = expr.unpack_char();
                                    if expected_char == *actual_char {
                                        target = Some(MatchedBranch {
                                            body: &branch.body,
                                            bound_vars: 0,
                                        });
                                        break;
                                    }
                                }
                                Pattern::Int(actual_int) => {
                                    let expected_int = expr.unpack_int();
                                    if expected_int == *actual_int {
                                        target = Some(MatchedBranch {
                                            body: &branch.body,
                                            bound_vars: 0,
                                        });
                                        break;
                                    }
                                }
                                Pattern::String(actual_string) => {
                                    let expected_string = expr.unpack_string();
                                    if expected_string.as_ref() == actual_string.as_ref() {
                                        target = Some(MatchedBranch {
                                            body: &branch.body,
                                            bound_vars: 0,
                                        });
                                        break;
                                    }
                                }
                                Pattern::Unit => {
                                    expr.unpack_unit();
                                    target = Some(MatchedBranch {
                                        body: &branch.body,
                                        bound_vars: 0,
                                    });
                                    break;
                                }
                                Pattern::Wildcard => {
                                    target = Some(MatchedBranch {
                                        body: &branch.body,
                                        bound_vars: 0,
                                    });
                                    break;
                                }
                            }
                        }
                    }
                }

                match target {
                    Some(matched_branch) => {
                        let result = self.eval(env, matched_branch.body);
                        for _ in 0..matched_branch.bound_vars {
                            env.pop();
                        }
                        result
                    }
                    None => panic!("incomplete pattern match"),
                }
            }
            Expr::Unit => Value::Unit,
            Expr::Cmd(cmd_parts) => {
                let mut new_cmd_parts: Vec<Rc<str>> = Vec::with_capacity(cmd_parts.len());
                for cmd_part in cmd_parts {
                    match cmd_part {
                        core::CmdPart::Arg(string_parts) => {
                            let new_part = self.eval_string_parts(env, string_parts);
                            new_cmd_parts.push(Rc::from(new_part.as_str()));
                        }
                        core::CmdPart::Args(expr) => {
                            let args = self.eval(env, expr).unpack_array();
                            new_cmd_parts.extend(args.iter().map(|arg| arg.unpack_string()))
                        }
                    }
                }
                self.alloc(Object::Cmd(new_cmd_parts))
            }
        };
        out
    }
}
