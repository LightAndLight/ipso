//! Operator precedence parsing

#[cfg(test)]
mod test;

use crate::ParseResult;
use ipso_syntax::{Binop, Expr, Spanned};
use std::cmp::Ordering;

struct State<'a> {
    values: Vec<Spanned<Expr>>,
    operators: Vec<Binop>,
    rest: &'a mut dyn Iterator<Item = ParseResult<(Binop, Spanned<Expr>)>>,
    current_pair: Option<(Binop, Spanned<Expr>)>,
}

impl<'a> State<'a> {
    fn new(
        first: Spanned<Expr>,
        rest: &'a mut dyn Iterator<Item = ParseResult<(Binop, Spanned<Expr>)>>,
    ) -> ParseResult<Self> {
        let values = vec![first];
        let operators = Vec::with_capacity(1);
        match rest.next() {
            None => ParseResult::pure(State {
                values,
                operators,
                rest,
                current_pair: None,
            }),
            Some(result) => result.map(move |current_pair| State {
                values,
                operators,
                rest,
                current_pair: Some(current_pair),
            }),
        }
    }

    fn shift(&mut self) -> ParseResult<()> {
        match &self.current_pair {
            None => {
                panic!("shift called on empty input")
            }
            Some((binop, rhs)) => {
                self.values.push(rhs.clone());
                self.operators.push(*binop);
                match self.rest.next() {
                    None => {
                        self.current_pair = None;
                        ParseResult::pure(())
                    }
                    Some(result) => {
                        result.map(|current_pair| self.current_pair = Some(current_pair))
                    }
                }
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

/// Parse a sequence of binary operators into a correctly associated expression tree.
pub fn operator(
    first: Spanned<Expr>,
    rest: &mut dyn Iterator<Item = ParseResult<(Binop, Spanned<Expr>)>>,
) -> ParseResult<Spanned<Expr>> {
    State::new(first, rest).and_then(|mut state| {
        let mut loop_result = ParseResult::pure(());
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
                        loop_result = loop_result.and_then(|()| state.shift());
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
                                loop_result = loop_result.and_then(|()| state.shift());
                            }
                            _ => {
                                loop_result =
                                    loop_result.and_then(|()| ParseResult::ambiguous_use_of(binop))
                            }
                        },
                        Ordering::Greater => {
                            loop_result = loop_result.and_then(|()| state.shift());
                        }
                    },
                },
            }
        }

        debug_assert!(state.values.len() == 1);
        debug_assert!(state.operators.is_empty());
        loop_result.map(|()| state.values.pop().unwrap())
    })
}
