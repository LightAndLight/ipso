#[cfg(test)]
use super::{Interpreter, Value};
#[cfg(test)]
use crate::core::{Builtin, Expr};
#[cfg(test)]
use std::collections::HashMap;
#[cfg(test)]
use typed_arena::Arena;

#[test]
fn eval_1() {
    let mut stdout: Vec<u8> = Vec::new();
    let heap = Arena::new();
    let term = Expr::mk_app(
        Expr::mk_app(Expr::Builtin(Builtin::Trace), Expr::Int(0)),
        Expr::Int(1),
    );
    let mut interpreter = Interpreter::new(&mut stdout, HashMap::new(), &heap);

    let expected_value = heap.alloc(Value::Int(1));
    let actual_value = interpreter.eval(term);
    assert_eq!(expected_value, actual_value);

    let actual_stdout = String::from_utf8(stdout).unwrap();
    let expected_stdout = String::from("trace: 0\n");
    assert_eq!(expected_stdout, actual_stdout);
}
