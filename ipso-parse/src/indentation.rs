//! # Indentation parsing
//!
//! This module provides primitives for parsing indentation in the style of
//! "Principled parsing for indentation-sensitive languages: revisiting landin's offside rule"[^principled].
//!
//! Every [Token] is annotated with its column. [indent_scope!] pushes the
//! current token's column onto the indentation stack, evaluates its body, and
//! pops the stack. [indent!] asserts that the current token has some
//! [Relation]ship with the column at the top of the indentation stack.
//!
//! These primitives are combined to implement scoped indentation parsing,
//! where each token's indentation is required to have some particular
//! relationship with a previously identified token. See `ipso_parse/src/lib.rs` for
//! examples.
//!
//! ## Datatypes
//!
//! * [Relation]
//!
//! ## Macros
//!
//! * [indent_scope!]
//! * [indent!]
//!
//! [^principled]: Adams, M. D. (2013). Principled parsing for indentation-sensitive languages: revisiting landin's offside rule. ACM SIGPLAN Notices, 48(1), 511-522.

#[allow(unused_imports)] // Used by docs
use ipso_lex::token::{Relation, Token};

/// Evaluate an expression within a new indentation scope.
///
/// All [indent]s within `$body` are parsed relative to the indentation of
/// the current token.
#[macro_export]
macro_rules! indent_scope {
    ($parser:expr, $body:expr) => {{
        $parser.indentation.push($parser.column);
        let b = $body;
        $parser.indentation.pop().unwrap();
        b
    }};
}

/// Parse an indent relative to the current indentation scope.
///
/// If this macro is used outside of an [indent_scope], then the current
/// indentation is considered to be 'negative infinity'. In this situation,
/// [Relation::Gt] and [Relation::Gte] will always succeed, and the use of
/// any other [Relation] will fail.
#[macro_export]
macro_rules! indent {
    ($parser:expr, $relation:expr, $body:expr) => {{
        use ipso_lex::token::{self, Relation};

        let current_indentation: Option<usize> = $parser.indentation.last().copied();
        $parser.expecting.insert(match current_indentation {
            None => token::Name::Indent(Relation::Gte, 0),
            Some(current_indentation) => token::Name::Indent($relation, current_indentation),
        });
        match &$parser.current {
            None => $parser.unexpected(false),
            Some(token) => {
                let current_indentation_matches = match current_indentation {
                    None => match $relation {
                        Relation::Gt => true,
                        Relation::Gte => true,
                        Relation::Eq => false,
                    },
                    Some(current_indentation) => match $relation {
                        Relation::Gt => token.column > current_indentation,
                        Relation::Gte => token.column >= current_indentation,
                        Relation::Eq => token.column == current_indentation,
                    },
                };
                if current_indentation_matches {
                    $parser.expecting.clear_indents();
                    $body
                } else {
                    $parser.unexpected(false)
                }
            }
        }
    }};
}
