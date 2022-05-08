use std::{
    io::{self, BufReader, BufWriter},
    rc::Rc,
};

use ipso_core::CommonKinds;
use ipso_diagnostic::Source;
use ipso_eval::{Env, Interpreter};
use ipso_syntax::Spanned;
use ipso_typecheck::type_inference::{self, infer};
use typed_arena::Arena;

#[derive(Debug)]
pub enum Error {
    TypeError(type_inference::Error),
}

impl From<type_inference::Error> for Error {
    fn from(err: type_inference::Error) -> Self {
        Error::TypeError(err)
    }
}

pub struct Repl {
    common_kinds: CommonKinds,
    source: Source,
}

impl Repl {
    pub fn new() -> Self {
        Repl {
            common_kinds: Default::default(),
            source: Source::Interactive {
                label: String::from("repl"),
            },
        }
    }

    pub fn type_of(&self, expr: Spanned<ipso_syntax::Expr>) -> Result<ipso_core::Type, Error> {
        let env = type_inference::Env {
            common_kinds: &self.common_kinds,
            modules: &Default::default(),
            types: &Default::default(),
            type_variables: &Default::default(),
            type_signatures: &Default::default(),
            source: &self.source,
        };
        let mut state = type_inference::State::new();
        let (_, ty) = infer(env, &mut state, &expr)?;
        Ok(ty)
    }

    pub fn eval(&self, expr: Spanned<ipso_syntax::Expr>) -> Result<Value, Error> {
        let env = type_inference::Env {
            common_kinds: &self.common_kinds,
            modules: &Default::default(),
            types: &Default::default(),
            type_variables: &Default::default(),
            type_signatures: &Default::default(),
            source: &self.source,
        };
        let mut state = type_inference::State::new();
        let (expr, _) = infer(env, &mut state, &expr)?;

        let stdin = io::stdin();
        let mut stdin = BufReader::new(stdin);

        let stdout = io::stdout();
        let mut stdout = BufWriter::new(stdout);

        let modules = Default::default();
        let context = Default::default();

        let bytes = Arena::new();
        let values = Arena::new();
        let objects = Arena::new();

        let mut interpreter = Interpreter::new(
            &mut stdin,
            &mut stdout,
            &self.common_kinds,
            &modules,
            &context,
            &bytes,
            &values,
            &objects,
        );

        let value = interpreter.eval(&mut Env::new(), &expr);

        Ok(Value::from(value))
    }
}

impl Default for Repl {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    True,
    False,
    Int(u32),
    Char(char),
    Unit,
    String(String),
    Bytes(Vec<u8>),
    Variant(usize, Box<Value>),
    Array(Vec<Value>),
    Record(Vec<Value>),
    Closure,
    IO,
    Cmd(Vec<Rc<str>>),
}

impl<'a> From<ipso_eval::Value<'a>> for Value {
    fn from(value: ipso_eval::Value) -> Self {
        match value {
            ipso_eval::Value::True => Value::True,
            ipso_eval::Value::False => Value::False,
            ipso_eval::Value::Int(i) => Value::Int(i),
            ipso_eval::Value::Char(c) => Value::Char(c),
            ipso_eval::Value::Unit => Value::Unit,
            ipso_eval::Value::Object(object) => match object {
                ipso_eval::Object::String(s) => Value::String(String::from(*s)),
                ipso_eval::Object::Bytes(b) => Value::Bytes(Vec::from(*b)),
                ipso_eval::Object::Variant(tag, arg) => {
                    Value::Variant(*tag, Box::new(Value::from(*arg)))
                }
                ipso_eval::Object::Array(items) => {
                    Value::Array(items.iter().map(|item| Value::from(*item)).collect())
                }
                ipso_eval::Object::Record(items) => {
                    Value::Record(items.iter().map(|item| Value::from(*item)).collect())
                }
                ipso_eval::Object::Closure { .. } | ipso_eval::Object::StaticClosure { .. } => {
                    Value::Closure
                }
                ipso_eval::Object::IO { .. } => Value::IO,
                ipso_eval::Object::Cmd(parts) => Value::Cmd(parts.clone()),
            },
        }
    }
}
