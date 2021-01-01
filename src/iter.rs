#[derive(Debug)]
pub enum Step<'a, S, A> {
    Yield(A),
    Skip,
    Continue(Vec<&'a S>),
}
