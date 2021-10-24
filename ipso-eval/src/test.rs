#[cfg(test)]
use super::{Interpreter, Value};
#[cfg(test)]
use crate::Object;
#[cfg(test)]
use ipso_core::{Builtin, Expr, StringPart};
#[cfg(test)]
use std::collections::HashMap;
#[cfg(test)]
use typed_arena::Arena;

#[test]
fn eval_1() {
    let mut stdin = std::io::empty();
    let mut stdout = Vec::new();
    let bytes = Arena::new();
    let values = Arena::new();
    let objects = Arena::new();
    let term = Expr::mk_app(
        Expr::mk_app(Expr::Builtin(Builtin::Trace), Expr::Int(0)),
        Expr::Int(1),
    );
    let context = HashMap::new();
    let mut interpreter = Interpreter::new(
        &mut stdin,
        &mut stdout,
        &context,
        HashMap::new(),
        &bytes,
        &values,
        &objects,
    );
    let env = interpreter.alloc_values(vec![]);

    let expected_value = Value::Int(1);
    let actual_value = interpreter.eval(env, &term);
    assert_eq!(expected_value, actual_value);

    let actual_stdout = String::from_utf8(stdout).unwrap();
    let expected_stdout = String::from("trace: 0\n");
    assert_eq!(expected_stdout, actual_stdout);
}

#[test]
fn eval_2() {
    let mut stdin = std::io::empty();
    let mut stdout = Vec::new();
    let bytes = Arena::new();
    let values = Arena::new();
    let objects = Arena::new();
    let str = String::from("hello");
    let term = Expr::mk_app(
        Expr::Builtin(Builtin::ToUtf8),
        Expr::String(vec![StringPart::String(str.clone())]),
    );
    let context = HashMap::new();
    let mut interpreter = Interpreter::new(
        &mut stdin,
        &mut stdout,
        &context,
        HashMap::new(),
        &bytes,
        &values,
        &objects,
    );
    let env = interpreter.alloc_values(vec![]);

    let expected_value = interpreter.alloc(Object::Bytes(str.as_bytes()));
    let actual_value = interpreter.eval(env, &term);
    assert_eq!(expected_value, actual_value);
}
