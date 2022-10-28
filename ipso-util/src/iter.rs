#[derive(Debug)]
pub enum Step<'a, S, A> {
    Yield(A),
    Skip,
    Continue1(&'a S),
    Continue2(&'a S, &'a S),
    Continue3(&'a S, &'a S, &'a S),
    Continue(Vec<&'a S>),
}

pub enum Stack<'a, T> {
    Zero,
    Many { stack: Vec<&'a T>, current: &'a T },
}

impl<'a, T> Stack<'a, T> {
    pub fn one(value: &'a T) -> Self {
        Self::Many {
            stack: Vec::new(),
            current: value,
        }
    }

    pub fn push(&mut self, value: &'a T) {
        match self {
            Stack::Zero => {
                *self = Stack::Many {
                    stack: Vec::new(),
                    current: value,
                };
            }
            Stack::Many { stack, current } => {
                stack.push(current);
                *current = value
            }
        }
    }

    pub fn pop(&mut self) -> Option<&'a T> {
        let state = std::mem::replace(self, Stack::Zero);
        match state {
            Stack::Zero => None,
            Stack::Many { mut stack, current } => {
                match stack.pop() {
                    Some(next) => {
                        *self = Stack::Many {
                            stack,
                            current: next,
                        }
                    }
                    None => {}
                }
                Some(current)
            }
        }
    }
}

impl<'a, T> Extend<&'a T> for Stack<'a, T> {
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
        iter.into_iter().for_each(|value| self.push(value));
    }
}
