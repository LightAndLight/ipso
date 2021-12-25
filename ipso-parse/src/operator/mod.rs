#[cfg(test)]
mod test;

use crate::ParseResult;
use ipso_syntax::{Binop, Expr, Spanned};
use std::cmp::Ordering;

struct State<'a> {
    values: Vec<Spanned<Expr>>,
    operators: Vec<Binop>,
    rest: &'a mut dyn ExactSizeIterator<Item = (Binop, Spanned<Expr>)>,
    current_pair: Option<(Binop, Spanned<Expr>)>,
}

impl<'a> State<'a> {
    fn new(
        first: Spanned<Expr>,
        rest: &'a mut dyn ExactSizeIterator<Item = (Binop, Spanned<Expr>)>,
    ) -> Self {
        let values = {
            let mut values = Vec::with_capacity(rest.len() + 1);
            values.push(first);
            values
        };
        let operators = Vec::with_capacity(rest.len());
        let current_pair: Option<(Binop, Spanned<Expr>)> = rest.next();

        State {
            values,
            operators,
            rest,
            current_pair,
        }
    }

    fn shift(&mut self) {
        match &self.current_pair {
            None => {
                panic!("shift called on empty input")
            }
            Some((binop, rhs)) => {
                self.values.push(rhs.clone());
                self.operators.push(*binop);
                self.current_pair = self.rest.next();
            }
        }
    }

    fn reduce(&mut self) {
        debug_assert!(self.values.len() >= 2);

        let rhs = self.values.pop().unwrap();
        let lhs = self.values.pop().unwrap();
        let binop = self.operators.pop().unwrap();
        self.values.push(Expr::mk_binop(binop, lhs, rhs));
    }

    fn peek_operator(&self) -> Option<&Binop> {
        self.operators.last()
    }

    fn peek_current(&self) -> Option<(&Binop, &Spanned<Expr>)> {
        self.current_pair.as_ref().map(|(a, b)| (a, b))
    }
}

pub fn operator(
    first: Spanned<Expr>,
    rest: Vec<(Binop, Spanned<Expr>)>,
) -> ParseResult<Spanned<Expr>> {
    let mut rest = rest.into_iter();
    let mut state = State::new(first, &mut rest);

    loop {
        match state.peek_current() {
            None => match state.peek_operator() {
                None => {
                    break;
                }
                Some(_prev_binop) => {
                    state.reduce();
                }
            },
            Some((binop, _)) => match state.peek_operator() {
                None => {
                    state.shift();
                }
                Some(prev_binop) => match binop.compare_precedence(prev_binop) {
                    Ordering::Less => {
                        state.reduce();
                    }
                    Ordering::Equal => match (binop.assoc(), prev_binop.assoc()) {
                        (ipso_syntax::Assoc::Left, ipso_syntax::Assoc::Left) => {
                            state.reduce();
                        }
                        (ipso_syntax::Assoc::Right, ipso_syntax::Assoc::Right) => {
                            state.shift();
                        }
                        _ => {
                            panic!("ambiguous use of operator {:?}", binop)
                        }
                    },
                    Ordering::Greater => {
                        state.shift();
                    }
                },
            },
        }
    }

    debug_assert!(state.values.len() == 1);
    debug_assert!(state.operators.is_empty());
    ParseResult::pure(state.values.pop().unwrap())
}
