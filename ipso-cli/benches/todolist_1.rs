use std::{
    fs,
    io::{BufReader, Cursor},
    rc::Rc,
};

use criterion::{criterion_group, criterion_main, BatchSize, Criterion};

fn one(c: &mut Criterion) {
    let contents: Rc<str> =
        Rc::from(fs::read_to_string("benches/todolist_1_input.txt").expect("failed to read file"));
    c.bench_function("one", |b| {
        b.iter_batched(
            || {
                let output = Vec::new();
                ipso_cli::run::Config {
                    filename: String::from("../examples/todolist.ipso"),
                    entrypoint: None,
                    args: vec![],
                    stdin: Some(Box::new(BufReader::new(Cursor::new(String::from(
                        contents.as_ref(),
                    ))))),
                    stdout: Some(Box::new(output)),
                }
            },
            ipso_cli::run::run_interpreter,
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, one);
criterion_main!(benches);
