//! Operator precedence parsing
//!
//! This module implements a [shift-reduce](https://en.wikipedia.org/wiki/Shift-reduce_parser)
//! style operator precedence parser via [`operator`].
//!
//! The rest of the module's contents are exposed to provide a good internal documentation
//! experience.

#[cfg(test)]
mod test;

use crate::Parsed;
use ipso_syntax::{Binop, Expr, Spanned};
use std::cmp::Ordering;

/// The operator precedence parser's state
pub struct State<'a> {
    /// The value stack
    pub exprs: Vec<Spanned<Expr>>,
    /// The operator stack
    pub operators: Vec<Spanned<Binop>>,
    /// The current (operator, expression) pair
    pub current_pair: Option<(Spanned<Binop>, Spanned<Expr>)>,
    /// The (operator, expression) pair parser
    pub rest: &'a mut dyn Iterator<Item = Parsed<(Spanned<Binop>, Spanned<Expr>)>>,
}

impl<'a> State<'a> {
    /// # Arguments
    ///
    /// * `first` - The leftmost expression, already parsed
    /// * `rest` - An iterator that parses subsequent (operator, expression) pairs
    pub fn new(
        first: Spanned<Expr>,
        rest: &'a mut dyn Iterator<Item = Parsed<(Spanned<Binop>, Spanned<Expr>)>>,
    ) -> Parsed<Self> {
        let exprs = vec![first];
        let operators = Vec::with_capacity(1);
        match rest.next() {
            None => Parsed::pure(State {
                exprs,
                operators,
                rest,
                current_pair: None,
            }),
            Some(result) => result.map(move |current_pair| State {
                exprs,
                operators,
                rest,
                current_pair: Some(current_pair),
            }),
        }
    }

    /// Get the top of the operator stack.
    pub fn peek_operator(&self) -> Option<&Spanned<Binop>> {
        self.operators.last()
    }

    /// Get the current (operator, expression) pair.
    pub fn peek_current(&self) -> Option<(&Spanned<Binop>, &Spanned<Expr>)> {
        self.current_pair.as_ref().map(|(a, b)| (a, b))
    }

    /// Push the current (operator, expression) pair onto their respective stacks
    /// and parse a new pair.
    ///
    /// [`State::shift`] delays the construction of a binary operator node for the operator
    /// on top of the stack. It's called when the current operator has a higher precedence
    /// than the operator on top of the stack, and when parsing a sequence of
    /// right-associative operators of the same precedence.
    pub fn shift(&mut self) -> Parsed<()> {
        match &self.current_pair {
            None => {
                panic!("shift called on empty input")
            }
            Some((binop, rhs)) => {
                self.exprs.push(rhs.clone());
                self.operators.push(*binop);
                match self.rest.next() {
                    None => {
                        self.current_pair = None;
                        Parsed::pure(())
                    }
                    Some(result) => {
                        result.map(|current_pair| self.current_pair = Some(current_pair))
                    }
                }
            }
        }
    }

    /// Apply the most recent operator to the two most recent expressions.
    ///
    /// [`State::reduce`] forces the construction of a binary operator node. It's
    /// used when the current operator has a lower precedence than the operator on
    /// top of the stack, to ensure that the operator on top of the stack binds
    /// first. [`State::reduce`] is also used when parsing a sequence of left-associative
    /// operators of the same precedence.
    pub fn reduce(&mut self) {
        debug_assert!(self.exprs.len() >= 2);

        let rhs = self.exprs.pop().unwrap();
        let lhs = self.exprs.pop().unwrap();
        let binop = self.operators.pop().unwrap();
        self.exprs.push(Expr::mk_binop(binop, lhs, rhs));
    }
}

/// Parse a sequence of binary operators into a correctly associated expression tree.
///
/// # Arguments
///
/// * `first` - The leftmost expression, already parsed
/// * `rest` - An iterator that parses subsequent (operator, expression) pairs
pub fn operator(
    first: Spanned<Expr>,
    rest: &mut dyn Iterator<Item = Parsed<(Spanned<Binop>, Spanned<Expr>)>>,
) -> Parsed<Spanned<Expr>> {
    State::new(first, rest).and_then(|mut state| {
        let mut loop_result = Parsed::pure(());
        while loop_result.result.is_ok() {
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
                    Some(prev_binop) => match binop.item.compare_precedence(&prev_binop.item) {
                        Ordering::Less => {
                            state.reduce();
                        }
                        Ordering::Equal => match (binop.item.assoc(), prev_binop.item.assoc()) {
                            (ipso_syntax::Assoc::Left, ipso_syntax::Assoc::Left) => {
                                state.reduce();
                            }
                            (ipso_syntax::Assoc::Right, ipso_syntax::Assoc::Right) => {
                                loop_result = loop_result.and_then(|()| state.shift());
                            }
                            _ => {
                                loop_result =
                                    loop_result.and_then(|()| Parsed::ambiguous_use_of(*binop))
                            }
                        },
                        Ordering::Greater => {
                            loop_result = loop_result.and_then(|()| state.shift());
                        }
                    },
                },
            }
        }

        loop_result.map(|()| {
            debug_assert!(
                state.exprs.len() == 1,
                "state.exprs contains more than one value: {:?}",
                state.exprs
            );
            debug_assert!(state.operators.is_empty());
            state.exprs.pop().unwrap()
        })
    })
}
