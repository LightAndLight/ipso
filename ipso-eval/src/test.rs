use super::{Interpreter, Value};
use crate::{closure_conversion::Expr, Env, Object};
use ipso_core::{Builtin, CommonKinds, StringPart};
use ipso_syntax::Modules;
use std::{collections::HashMap, rc::Rc};

#[test]
fn eval_1() {
    let mut stdin = std::io::empty();
    let mut stdout = Vec::new();
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
        &args,
        &mut stdin,
        &mut stdout,
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
        &args,
        &mut stdin,
        &mut stdout,
        &common_kinds,
        &modules,
        &context,
    );
    let mut env = Env::new();

    let expected_value = interpreter.alloc(Object::Bytes(Rc::from(str.as_bytes())));
    let actual_value = interpreter.eval(&mut env, &term);
    assert_eq!(expected_value, actual_value);
}
