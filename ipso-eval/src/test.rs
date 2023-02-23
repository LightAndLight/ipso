use super::{Interpreter, Value};
use crate::{closure_conversion::Expr, Env, Object, IO};
use ipso_core::{
    self as core, Builtin, CommonKinds, Declaration, Module, Name, StringPart, Type, TypeSig,
};
use ipso_syntax::{ModuleKey, ModuleRef, Modules};
use std::{collections::HashMap, rc::Rc};

#[test]
fn eval_1() {
    let mut stdin = std::io::empty();
    let mut stdout = Vec::new();
    let mut stderr = Vec::new();
    let term = Expr::App(
        Rc::new(Expr::App(
            Rc::new(Expr::Builtin(Builtin::Trace)),
            Rc::new(Expr::Int(0)),
        )),
        Rc::new(Expr::Int(1)),
    );
    let common_kinds = CommonKinds::default();
    let context = HashMap::new();
    let modules = Modules::new();
    let args = vec![];
    let mut interpreter = Interpreter::new(
        Rc::from("test"),
        &args,
        IO {
            stdin: &mut stdin,
            stdout: &mut stdout,
            stderr: &mut stderr,
        },
        &common_kinds,
        &modules,
        &context,
    );
    let mut env = Env::new();

    let expected_value = Value::Int(1);
    let actual_value = interpreter.eval(&mut env, &term);
    assert_eq!(expected_value, actual_value);

    let actual_stdout = String::from_utf8(stdout).unwrap();
    let expected_stdout = String::from("trace: 0\n");
    assert_eq!(expected_stdout, actual_stdout);
}

#[test]
fn eval_2() {
    let mut stdin = std::io::empty();
    let mut stdout = Vec::new();
    let mut stderr = Vec::new();
    let str = String::from("hello");
    let term = Expr::App(
        Rc::new(Expr::Builtin(Builtin::ToUtf8)),
        Rc::new(Expr::String(vec![StringPart::String(str.clone())])),
    );
    let common_kinds = CommonKinds::default();
    let context = HashMap::new();
    let modules = Modules::new();
    let args = vec![];
    let mut interpreter = Interpreter::new(
        Rc::from("test"),
        &args,
        IO {
            stdin: &mut stdin,
            stdout: &mut stdout,
            stderr: &mut stderr,
        },
        &common_kinds,
        &modules,
        &context,
    );
    let mut env = Env::new();

    let expected_value = interpreter.alloc(Object::Bytes(Rc::from(str.as_bytes())));
    let actual_value = interpreter.eval(&mut env, &term);
    assert_eq!(expected_value, actual_value);
}

#[test]
fn submodule_self_reference() {
    let mut stdin = std::io::empty();
    let mut stdout = Vec::new();
    let mut stderr = Vec::new();
    let common_kinds = CommonKinds::default();
    let context = HashMap::new();

    let mut modules = Modules::new();
    let my_module_ref = modules.insert(
        ModuleKey::from("myModule"),
        Module {
            decls: vec![Declaration::Module {
                name: String::from("mySubmodule"),
                decls: vec![
                    Rc::new(Declaration::Definition {
                        name: Rc::from("a"),
                        sig: TypeSig {
                            ty_vars: vec![],
                            body: Type::Int,
                        },
                        body: Rc::new(core::Expr::Int(1)),
                    }),
                    Rc::new(Declaration::Definition {
                        name: Rc::from("b"),
                        sig: TypeSig {
                            ty_vars: vec![],
                            body: Type::Int,
                        },
                        body: Rc::new(core::Expr::Module {
                            id: ModuleRef::This,
                            path: vec![],
                            item: Name::definition("a"),
                        }),
                    }),
                ],
            }],
        },
    );

    let term = Expr::Module {
        id: ModuleRef::Id(my_module_ref),
        path: vec![String::from("mySubmodule")],
        item: Name::definition("b"),
    };

    let args = vec![];
    let mut interpreter = Interpreter::new(
        Rc::from("test"),
        &args,
        IO {
            stdin: &mut stdin,
            stdout: &mut stdout,
            stderr: &mut stderr,
        },
        &common_kinds,
        &modules,
        &context,
    );
    let mut env = Env::new();

    let expected_value = Value::Int(1);
    let actual_value = interpreter.eval(&mut env, &term);
    assert_eq!(expected_value, actual_value);
}
