#[derive(Debug)]
pub enum Step<'a, S, A> {
    Yield(A),
    Skip,
    Continue1(&'a S),
    Continue2(&'a S, &'a S),
    Continue3(&'a S, &'a S, &'a S),
    Continue(Vec<&'a S>),
}
