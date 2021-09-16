use std::{fs::File, io::BufReader};

use criterion::{criterion_group, criterion_main, BatchSize, Criterion};

fn one(c: &mut Criterion) {
    c.bench_function("one", |b| {
        b.iter_batched(
            || {
                let input =
                    File::open("benches/todolist_1_input.txt").expect("File::open() failed");
                let output = Vec::new();
                ipso::run::Config {
                    filename: String::from("examples/todolist.ipso"),
                    entrypoint: None,
                    stdin: Some(Box::new(BufReader::new(input))),
                    stdout: Some(Box::new(output)),
                }
            },
            ipso::run::run_interpreter,
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, one);
criterion_main!(benches);
