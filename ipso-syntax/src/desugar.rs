use ipso_diagnostic::{Diagnostic, Location, Message, Source};

use crate::{Branch, CmdPart, CompLine, Declaration, Expr, Module, Pattern, Spanned, StringPart};
use std::rc::Rc;

/// An invalid ending for a computation expression.
#[derive(PartialEq, Eq, Debug)]
pub enum CompExprEnd {
    None,
    Let,
    Bind,
}

/// Syntax error information.
#[derive(PartialEq, Eq, Debug)]
pub enum Error {
    CompExprEndsWith {
        source: Source,
        pos: usize,
        end: CompExprEnd,
    },
}

impl Error {
    pub fn source(&self) -> &Source {
        match self {
            Error::CompExprEndsWith { source, .. } => source,
        }
    }

    pub fn position(&self) -> usize {
        match self {
            Error::CompExprEndsWith { pos, .. } => *pos,
        }
    }

    pub fn message(&self) -> String {
        match self {
            Error::CompExprEndsWith { end, .. } => String::from(match end {
                CompExprEnd::None => "empty computation expression",
                CompExprEnd::Bind => "computation expression ends with a bind",
                CompExprEnd::Let => "computation expression ends with a let",
            }),
        }
    }

    pub fn report(&self, diagnostic: &mut Diagnostic) {
        diagnostic.item(
            Some(Location {
                source: self.source().clone(),
                offset: Some(self.position()),
            }),
            Message {
                content: self.message(),
                addendum: None,
            },
        )
    }
}

pub fn desugar_module(source: &Source, mut module: Module) -> Result<Module, Error> {
    desugar_module_mut(source, &mut module)?;
    Ok(module)
}

fn desugar_module_mut(source: &Source, module: &mut Module) -> Result<(), Error> {
    module
        .decls
        .iter_mut()
        .try_for_each(|decl| desugar_decl_mut(source, &mut decl.item))
}

fn desugar_decl_mut(source: &Source, decl: &mut Declaration) -> Result<(), Error> {
    match decl {
        Declaration::Definition { body, .. } => desugar_expr_mut(source, body),
        Declaration::Instance { members, .. } => members
            .iter_mut()
            .try_for_each(|(_, _, expr)| desugar_expr_mut(source, expr)),
        Declaration::Class { .. }
        | Declaration::TypeAlias { .. }
        | Declaration::Import { .. }
        | Declaration::FromImport { .. } => Ok(()),
    }
}

fn desugar_string_part_mut(source: &Source, string_part: &mut StringPart) -> Result<(), Error> {
    match string_part {
        StringPart::String(_) => Ok(()),
        StringPart::Expr(expr) => desugar_expr_mut(source, expr),
    }
}

fn desugar_branch_mut(source: &Source, branch: &mut Branch) -> Result<(), Error> {
    desugar_expr_mut(source, &mut branch.body)
}

fn desugar_cmd_part_mut(source: &Source, cmd_part: &mut CmdPart) -> Result<(), Error> {
    match cmd_part {
        CmdPart::Literal(_) => Ok(()),
        CmdPart::Expr(expr) => desugar_expr_mut(source, expr),
    }
}

pub fn desugar_expr(source: &Source, mut expr: Spanned<Expr>) -> Result<Spanned<Expr>, Error> {
    desugar_expr_mut(source, &mut expr)?;
    Ok(expr)
}

/*
[note: private mutable functions]

This module defines private functions of type `fn(&mut T) -> Result<(), Error>` with public
counterparts of type `fn(T) -> Result<T, Error>`.

The private `&mut` functions can fail part way through desugaring a value, leaving the
value in an invalid state (see [note: std::mem::take]). By only exposing functions that
take ownership, we ensure that desugaring never returns partly-desugared value to the
caller.
*/
fn desugar_expr_mut(source: &Source, expr: &mut Spanned<Expr>) -> Result<(), Error> {
    match &mut expr.item {
        Expr::App(func, arg) => {
            desugar_expr_mut(source, &mut Rc::make_mut(func))?;
            desugar_expr_mut(source, &mut Rc::make_mut(arg))
        }
        Expr::Lam { body, .. } => desugar_expr_mut(source, &mut Rc::make_mut(body)),
        Expr::Let { value, rest, .. } => {
            desugar_expr_mut(source, &mut Rc::make_mut(value))?;
            desugar_expr_mut(source, &mut Rc::make_mut(rest))
        }
        Expr::IfThenElse(cond, a, b) => {
            desugar_expr_mut(source, &mut Rc::make_mut(cond))?;
            desugar_expr_mut(source, &mut Rc::make_mut(a))?;
            desugar_expr_mut(source, &mut Rc::make_mut(b))
        }
        Expr::Binop(_, left, right) => {
            desugar_expr_mut(source, &mut Rc::make_mut(left))?;
            desugar_expr_mut(source, &mut Rc::make_mut(right))
        }
        Expr::String(parts) => parts
            .iter_mut()
            .try_for_each(|string_part| desugar_string_part_mut(source, string_part)),
        Expr::Array(items) => items
            .iter_mut()
            .try_for_each(|expr| desugar_expr_mut(source, expr)),
        Expr::Record { fields, rest } => {
            fields
                .iter_mut()
                .try_for_each(|(_, expr)| desugar_expr_mut(source, expr))?;
            rest.iter_mut()
                .try_for_each(|expr| desugar_expr_mut(source, &mut Rc::make_mut(expr)))
        }
        Expr::Project(value, _) => desugar_expr_mut(source, &mut Rc::make_mut(value)),
        Expr::Embed(_, value) => desugar_expr_mut(source, &mut Rc::make_mut(value)),
        Expr::Case(expr, branches) => {
            desugar_expr_mut(source, &mut Rc::make_mut(expr))?;
            branches
                .iter_mut()
                .try_for_each(|branch| desugar_branch_mut(source, branch))
        }
        Expr::Cmd(parts) => parts
            .iter_mut()
            .try_for_each(|cmd_part| desugar_cmd_part_mut(source, cmd_part)),
        Expr::Comp(comp_lines) => {
            // [note: std::mem::take]
            let mut comp_lines = std::mem::take(comp_lines);

            comp_lines
                .iter_mut()
                .try_for_each(|comp_line| match &mut comp_line.item {
                    CompLine::Expr(expr) => desugar_expr_mut(source, expr),
                    CompLine::Bind(_, expr) => desugar_expr_mut(source, expr),
                    CompLine::Let(_, expr) => desugar_expr_mut(source, expr),
                })?;

            match comp_lines.pop() {
                None => Err(Error::CompExprEndsWith {
                    source: source.clone(),
                    pos: expr.pos,
                    end: CompExprEnd::None,
                }),
                Some(last_comp_line) => match last_comp_line.item {
                    CompLine::Bind(_, _) => Err(Error::CompExprEndsWith {
                        source: source.clone(),
                        pos: last_comp_line.pos,
                        end: CompExprEnd::Bind,
                    }),
                    CompLine::Let(_, _) => Err(Error::CompExprEndsWith {
                        source: source.clone(),
                        pos: last_comp_line.pos,
                        end: CompExprEnd::Let,
                    }),
                    CompLine::Expr(mut last_expr) => {
                        desugar_expr_mut(source, &mut last_expr)?;

                        *expr = comp_lines
                            .into_iter()
                            .rev()
                            .fold(last_expr, |expr, comp_line| {
                                let comp_line_pos = comp_line.pos;
                                match comp_line.item {
                                    CompLine::Expr(comp_line_expr) => {
                                        // bindIO <comp_line_expr> (\_ -> <expr>)
                                        Expr::mk_app(
                                            Expr::mk_app(
                                                Spanned {
                                                    pos: comp_line_pos,
                                                    item: Expr::mk_var("bindIO"),
                                                },
                                                comp_line_expr,
                                            ),
                                            Spanned {
                                                pos: expr.pos,
                                                item: Expr::mk_lam(
                                                    vec![Spanned {
                                                        pos: expr.pos,
                                                        item: Pattern::Wildcard,
                                                    }],
                                                    expr,
                                                ),
                                            },
                                        )
                                    }
                                    CompLine::Bind(name, value) => {
                                        // bindIO <value> (\<name> -> <expr>)
                                        Expr::mk_app(
                                            Expr::mk_app(
                                                Spanned {
                                                    pos: comp_line_pos,
                                                    item: Expr::mk_var("bindIO"),
                                                },
                                                value,
                                            ),
                                            Spanned {
                                                pos: expr.pos,
                                                item: Expr::mk_lam(
                                                    vec![Spanned {
                                                        pos: expr.pos,
                                                        item: Pattern::Name(name),
                                                    }],
                                                    expr,
                                                ),
                                            },
                                        )
                                    }
                                    CompLine::Let(name, value) => {
                                        // let <name> = <value> in <expr>
                                        Spanned {
                                            pos: comp_line_pos,
                                            item: Expr::Let {
                                                name: name.item,
                                                value: Rc::new(value),
                                                rest: Rc::new(expr),
                                            },
                                        }
                                    }
                                }
                            });

                        Ok(())
                    }
                },
            }
        }
        Expr::Variant(_)
        | Expr::Module { .. }
        | Expr::Int(_)
        | Expr::Char(_)
        | Expr::True
        | Expr::False
        | Expr::Unit
        | Expr::Var(_) => Ok(()),
    }
}
