use std::{
    collections::HashMap,
    io::{self, BufReader, BufWriter},
    rc::Rc,
};

use ipso_core::{ClassDeclaration, CommonKinds, Signature, Type};
use ipso_diagnostic::Source;
use ipso_eval::{Env, Interpreter, Object};
use ipso_import::{resolve_from_import_all, rewrite_module_accessors_expr, ImportedItemInfo};
use ipso_syntax::{
    desugar::{self, desugar_expr},
    kind::Kind,
    ModuleId, ModuleKey, Modules, Names, Spanned,
};
use ipso_typecheck::{
    abstract_evidence,
    constraint_solving::Implication,
    module::register_from_import,
    type_inference::{self, infer},
};
use typed_arena::Arena;

#[derive(Debug)]
pub enum Error {
    TypeError(type_inference::Error),
    CheckError(ipso_typecheck::Error),
    DesugarError(desugar::Error),
}

impl From<ipso_typecheck::Error> for Error {
    fn from(err: ipso_typecheck::Error) -> Self {
        Error::CheckError(err)
    }
}

impl From<type_inference::Error> for Error {
    fn from(err: type_inference::Error) -> Self {
        Error::TypeError(err)
    }
}

impl From<desugar::Error> for Error {
    fn from(err: desugar::Error) -> Self {
        Error::DesugarError(err)
    }
}

impl Error {
    pub fn position(&self) -> usize {
        match self {
            Error::TypeError(err) => err.position.unwrap_or(0),
            Error::CheckError(err) => err.position(),
            Error::DesugarError(err) => err.position(),
        }
    }

    pub fn message(&self) -> String {
        match self {
            Error::TypeError(err) => err.message(),
            Error::CheckError(err) => err.message(),
            Error::DesugarError(err) => err.message(),
        }
    }
}

pub struct Repl {
    common_kinds: CommonKinds,
    source: Source,
    module_context: HashMap<ModuleId, HashMap<String, ipso_core::Signature>>,
    imported_items: HashMap<String, ImportedItemInfo>,
    implications: Vec<Implication>,
    type_context: HashMap<Rc<str>, Kind>,

    #[allow(dead_code)] // We'll need this to implement imports
    modules: Modules<ipso_core::Module>,

    #[allow(dead_code)] // We'll need this to implement bindings
    context: HashMap<String, Signature>,

    #[allow(dead_code)] // We'll need this to implement class declarations
    class_context: HashMap<Rc<str>, ClassDeclaration>,
}

impl Repl {
    pub fn new(source: Source) -> Self {
        let common_kinds = CommonKinds::default();

        let mut modules = Modules::new();

        let builtins_module_id = {
            let builtins = ipso_builtins::builtins(&common_kinds);
            modules.insert(ModuleKey::from("builtins"), builtins)
        };
        let builtins = modules.lookup(builtins_module_id);

        let mut imported_items = HashMap::new();
        resolve_from_import_all(
            &common_kinds,
            &mut imported_items,
            builtins_module_id,
            builtins,
        );

        let mut implications = Vec::new();
        let mut type_context = HashMap::new();
        let mut context = HashMap::new();
        let mut class_context = HashMap::new();
        register_from_import(
            &common_kinds,
            &mut implications,
            &mut type_context,
            &mut context,
            &mut class_context,
            &modules,
            builtins_module_id,
            &Names::All,
        );

        let module_context =
            HashMap::from([(builtins_module_id, builtins.get_signatures(&common_kinds))]);
        Repl {
            common_kinds,
            source,
            modules,
            module_context,
            imported_items,
            implications,
            type_context,
            context,
            class_context,
        }
    }

    pub fn type_of(&self, expr: Spanned<ipso_syntax::Expr>) -> Result<Type, Error> {
        let mut expr = desugar_expr(&self.source, expr)?;
        rewrite_module_accessors_expr(&mut Default::default(), &self.imported_items, &mut expr);

        let env = type_inference::Env {
            common_kinds: &self.common_kinds,
            modules: &self.module_context,
            types: &Default::default(),
            type_variables: &Default::default(),
            type_signatures: &Default::default(),
            source: &self.source,
        };
        let mut state = type_inference::State::new();
        let (_, ty) = infer(env, &mut state, &expr)?;
        Ok(ty)
    }

    pub fn eval_show(&self, expr: Spanned<ipso_syntax::Expr>) -> Result<Option<String>, Error> {
        let mut expr = desugar_expr(&self.source, expr)?;
        rewrite_module_accessors_expr(&mut Default::default(), &self.imported_items, &mut expr);

        let (expr, show_final_value) = {
            let env = type_inference::Env {
                common_kinds: &self.common_kinds,
                modules: &self.module_context,
                types: &Default::default(),
                type_variables: &Default::default(),
                type_signatures: &Default::default(),
                source: &self.source,
            };

            let mut state = type_inference::State::new();
            let (_, ty) = infer(env, &mut state, &expr)?;

            let ty = state.zonk_type(ty);
            let (mut expr, show_final_value) = match ty {
                Type::Bool
                | Type::Int
                | Type::Char
                | Type::String
                | Type::Bytes
                | Type::Unit
                | Type::Arrow(_)
                | Type::FatArrow(_)
                | Type::Record(_)
                | Type::Variant(_)
                | Type::Array(_)
                | Type::Name(_, _)
                | Type::Var(_, _)
                | Type::IO(_)
                | Type::Cmd => (
                    ipso_syntax::Expr::mk_app(
                        Spanned {
                            pos: 0,
                            item: ipso_syntax::Expr::Var(String::from("debug")),
                        },
                        expr,
                    ),
                    true,
                ),
                Type::App(_, a, b) => {
                    if let Type::IO(_) = a.as_ref() {
                        if let Type::Unit = b.as_ref() {
                            (expr, false)
                        } else {
                            (
                                ipso_syntax::Expr::mk_app(
                                    ipso_syntax::Expr::mk_app(
                                        Spanned {
                                            pos: 0,
                                            item: ipso_syntax::Expr::mk_project(
                                                Spanned {
                                                    pos: 0,
                                                    item: ipso_syntax::Expr::mk_var("io"),
                                                },
                                                Spanned {
                                                    pos: 0,
                                                    item: String::from("map"),
                                                },
                                            ),
                                        },
                                        Spanned {
                                            pos: 0,
                                            item: ipso_syntax::Expr::Var(String::from("debug")),
                                        },
                                    ),
                                    expr,
                                ),
                                true,
                            )
                        }
                    } else {
                        (
                            ipso_syntax::Expr::mk_app(
                                Spanned {
                                    pos: 0,
                                    item: ipso_syntax::Expr::Var(String::from("debug")),
                                },
                                expr,
                            ),
                            true,
                        )
                    }
                }
                Type::Meta(_, _) => unreachable!(),
                Type::Constraints(_)
                | Type::RowNil
                | Type::RowCons(_, _, _)
                | Type::HasField(_, _) => unreachable!(),
            };

            rewrite_module_accessors_expr(&mut Default::default(), &self.imported_items, &mut expr);
            let (expr, _) = infer(env, &mut state, &expr)?;

            let (expr, unsolved) = abstract_evidence(
                &self.common_kinds,
                &self.implications,
                &self.type_context,
                &Default::default(),
                &mut state,
                &self.source,
                expr,
            )?;

            if !unsolved.is_empty() {
                todo!("unsolved: {:?}", unsolved)
            }

            Ok::<_, Error>((expr, show_final_value))
        }?;

        let stdin = io::stdin();
        let mut stdin = BufReader::new(stdin);

        let stdout = io::stdout();
        let mut stdout = BufWriter::new(stdout);

        let context = Default::default();

        let bytes = Arena::new();
        let values = Arena::new();
        let objects = Arena::new();

        let mut interpreter = Interpreter::new(
            &mut stdin,
            &mut stdout,
            &self.common_kinds,
            &self.modules,
            &context,
            &bytes,
            &values,
            &objects,
        );

        let value = interpreter.eval(&mut Env::new(), &expr);

        match value {
            ipso_eval::Value::Object(object) => match object {
                Object::Bytes(_)
                | Object::Variant(_, _)
                | Object::Array(_)
                | Object::Record(_)
                | Object::Closure { .. }
                | Object::StaticClosure { .. }
                | Object::Cmd(_) => todo!(),
                Object::String(s) => Ok(Some(String::from(*s))),
                Object::IO { env, body } => {
                    let result = body.run(&mut interpreter, env);
                    Ok(if show_final_value {
                        Some(String::from(result.unpack_string()))
                    } else {
                        // `show_final_value` should only be false for `IO ()`
                        debug_assert!(result == ipso_eval::Value::Unit);
                        None
                    })
                }
            },
            ipso_eval::Value::True
            | ipso_eval::Value::False
            | ipso_eval::Value::Int(_)
            | ipso_eval::Value::Char(_)
            | ipso_eval::Value::Unit => unreachable!(),
        }
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
