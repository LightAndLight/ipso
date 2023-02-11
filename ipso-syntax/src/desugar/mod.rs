#[cfg(test)]
mod test;

use crate::{
    Binop, Branch, CmdPart, CompLine, Declaration, Expr, Module, Pattern, Spanned, StringPart,
};
use ipso_diagnostic::{Diagnostic, Location, Message, Source};
use std::{collections::HashMap, rc::Rc};

pub struct VarGen {
    next: usize,
}

impl VarGen {
    pub fn new() -> Self {
        Self { next: 0 }
    }

    pub fn gen(&mut self) -> String {
        let index = self.next;
        self.next += 1;
        format!("desugar#{}", index)
    }
}

impl Default for VarGen {
    fn default() -> Self {
        Self::new()
    }
}

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
    let mut var_gen = VarGen::new();
    desugar_module_mut(source, &mut var_gen, &mut module)?;
    Ok(module)
}

fn desugar_module_mut(
    source: &Source,
    var_gen: &mut VarGen,
    module: &mut Module,
) -> Result<(), Error> {
    module
        .decls
        .iter_mut()
        .try_for_each(|decl| desugar_decl_mut(source, var_gen, &mut decl.item))
}

fn desugar_decl_mut(
    source: &Source,
    var_gen: &mut VarGen,
    decl: &mut Declaration,
) -> Result<(), Error> {
    match decl {
        Declaration::Definition { body, .. } => desugar_expr_mut(source, var_gen, body),
        Declaration::Instance { members, .. } => members
            .iter_mut()
            .try_for_each(|member| desugar_expr_mut(source, var_gen, &mut member.body)),
        Declaration::Class { .. }
        | Declaration::TypeAlias { .. }
        | Declaration::Import { .. }
        | Declaration::FromImport { .. } => Ok(()),
    }
}

fn desugar_string_part_mut(
    source: &Source,
    var_gen: &mut VarGen,
    string_part: &mut StringPart,
) -> Result<(), Error> {
    match string_part {
        StringPart::String(_) => Ok(()),
        StringPart::Expr(expr) => desugar_expr_mut(source, var_gen, expr),
    }
}

fn desugar_cmd_part_mut(
    source: &Source,
    var_gen: &mut VarGen,
    cmd_part: &mut CmdPart,
) -> Result<(), Error> {
    match cmd_part {
        CmdPart::Arg(string_parts) => string_parts
            .iter_mut()
            .try_for_each(|string_part| desugar_string_part_mut(source, var_gen, string_part)),
        CmdPart::Args(expr) => desugar_expr_mut(source, var_gen, expr),
    }
}

fn desugar_branches_mut(
    source: &Source,
    var_gen: &mut VarGen,
    branches: &mut Vec<Branch>,
) -> Result<(), Error> {
    /*
    Group branches by constructor while preserving their relative order.

    This ensures there are no duplicate "outer" constructors after desugaring nested patterns.
    The type checker expects that there are no duplicate variant constructors in a `case`
    expression.

    Example:

    ```
    case x of
      A 1 -> "A 1"
      B _ -> "B _"
      A _ -> "A _"
    ```

    Without reordering, the above code desugars to:

    ```
    f : (| A : Int, B : Int |) -> String
    f x =
      case x of
        A fresh_1 ->
          case fresh_1 of
            1 -> "A 1"
        B fresh_2 ->
          case fresh_2 of
            _ -> "B _"
        A fresh_3 ->
          case fresh_3 of
            _ -> "A _"
    ```

    The type checker considers the `A fresh_3` pattern redundant.

    Instead, we first reorder the `A` constructors to reside next to each other:

    ```
    case x of
      A 1 -> "A 1"
      A _ -> "A _"
      B _ -> "B _"
    ```

    And then desugar:

    ```
    f : (| A : Int, B : Int |) -> String
    f x =
      case x of
        A fresh_1 ->
          case fresh_1 of
            1 -> "A 1"
            _ -> "A _"
        B fresh_2 ->
            case fresh_2 of
            _ -> "B _"
    ```

    Resulting in a `case` expression with no redundant matches.
    */
    struct BranchGroup {
        first: Branch,
        rest: Vec<Branch>,
    }

    fn group_branches(branches: Vec<Branch>) -> Vec<BranchGroup> {
        let mut constructor_indices: HashMap<Rc<str>, usize> = HashMap::new();
        let mut grouped_branches: Vec<BranchGroup> = Vec::new();
        for branch in branches {
            match &branch.pattern.item {
                Pattern::Variant { name, .. } => {
                    let name = name.clone();
                    match constructor_indices.get(&name) {
                        Some(index) => {
                            grouped_branches.get_mut(*index).unwrap().rest.push(branch);
                        }
                        None => {
                            let index = grouped_branches.len();
                            grouped_branches.push(BranchGroup {
                                first: branch,
                                rest: Vec::new(),
                            });
                            constructor_indices.insert(name, index);
                        }
                    };
                }
                Pattern::Name(_)
                | Pattern::Record { .. }
                | Pattern::Char(_)
                | Pattern::Int(_)
                | Pattern::String(_)
                | Pattern::Unit
                | Pattern::Wildcard => grouped_branches.push(BranchGroup {
                    first: branch,
                    rest: Vec::new(),
                }),
            }
        }
        grouped_branches
    }

    #[cfg(debug_assertions)]
    let expected_branches_count = branches.len();

    let grouped_branches = group_branches(std::mem::take(branches));

    #[cfg(debug_assertions)]
    let mut actual_branches_count = 0;

    fn desugar_non_nested(
        #[cfg(debug_assertions)] actual_branches_count: &mut usize,
        source: &Source,
        var_gen: &mut VarGen,
        branches: &mut Vec<Branch>,
        mut first: Branch,
        rest: Vec<Branch>,
    ) -> Result<(), Error> {
        debug_assert!(rest.is_empty());

        #[cfg(debug_assertions)]
        {
            *actual_branches_count += 1;
        }

        desugar_expr_mut(source, var_gen, &mut first.body)?;

        branches.push(first);

        Ok(())
    }

    grouped_branches
        .into_iter()
        .try_for_each(|grouped| match grouped.first.pattern.item {
            Pattern::Variant { name, arg } => match arg.item.as_ref() {
                Pattern::Name(_) if grouped.rest.is_empty() => desugar_non_nested(
                    #[cfg(debug_assertions)]
                    &mut actual_branches_count,
                    source,
                    var_gen,
                    branches,
                    Branch {
                        pattern: Spanned {
                            pos: grouped.first.pattern.pos,
                            item: Pattern::Variant { name, arg },
                        },
                        body: grouped.first.body,
                    },
                    grouped.rest,
                ),
                _ => {
                    #[cfg(debug_assertions)]
                    {
                        actual_branches_count += 1;
                    }
                    let mut inner_branches = vec![Branch {
                        pattern: Spanned {
                            pos: arg.pos,
                            item: *arg.item,
                        },
                        body: grouped.first.body,
                    }];

                    for branch in grouped.rest {
                        match branch.pattern.item {
                            Pattern::Variant {
                                name: current_name,
                                arg,
                            } => {
                                debug_assert!(
                                    name == current_name,
                                    "unexpected variant {:?} in group for {:?}",
                                    current_name,
                                    name
                                );

                                #[cfg(debug_assertions)]
                                {
                                    actual_branches_count += 1;
                                }
                                inner_branches.push(Branch {
                                    pattern: Spanned {
                                        pos: arg.pos,
                                        item: *arg.item,
                                    },
                                    body: branch.body,
                                });
                            }
                            Pattern::Name(_)
                            | Pattern::Record { .. }
                            | Pattern::Char(_)
                            | Pattern::Int(_)
                            | Pattern::String(_)
                            | Pattern::Unit
                            | Pattern::Wildcard => panic!(
                                "unexpected pattern {:?} in group for variant {:?}",
                                branch.pattern, name
                            ),
                        }
                    }

                    let fresh_var = var_gen.gen();

                    let mut body = Spanned {
                        pos: grouped.first.pattern.pos,
                        item: Expr::mk_case(
                            Spanned {
                                pos: grouped.first.pattern.pos,
                                item: Expr::mk_var(&fresh_var),
                            },
                            inner_branches,
                        ),
                    };

                    desugar_expr_mut(source, var_gen, &mut body)?;

                    branches.push(Branch {
                        pattern: Spanned {
                            pos: grouped.first.pattern.pos,
                            item: Pattern::Variant {
                                name,
                                arg: Spanned {
                                    pos: grouped.first.pattern.pos,
                                    item: Box::new(Pattern::Name(Spanned {
                                        pos: grouped.first.pattern.pos,
                                        item: Rc::from(fresh_var.as_str()),
                                    })),
                                },
                            },
                        },
                        body,
                    });

                    Ok(())
                }
            },
            Pattern::Name(_)
            | Pattern::Record { .. }
            | Pattern::Char(_)
            | Pattern::Int(_)
            | Pattern::String(_)
            | Pattern::Unit
            | Pattern::Wildcard => desugar_non_nested(
                #[cfg(debug_assertions)]
                &mut actual_branches_count,
                source,
                var_gen,
                branches,
                grouped.first,
                grouped.rest,
            ),
        })?;

    #[cfg(debug_assertions)]
    assert!(
        expected_branches_count == actual_branches_count,
        "branches processed ({}) != original branches count ({})",
        actual_branches_count,
        expected_branches_count
    );

    Ok(())
}

pub fn desugar_expr(source: &Source, mut expr: Spanned<Expr>) -> Result<Spanned<Expr>, Error> {
    let mut var_gen = VarGen::new();
    desugar_expr_mut(source, &mut var_gen, &mut expr)?;
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
fn desugar_expr_mut(
    source: &Source,
    var_gen: &mut VarGen,
    expr: &mut Spanned<Expr>,
) -> Result<(), Error> {
    match &mut expr.item {
        Expr::App(func, arg) => {
            desugar_expr_mut(source, var_gen, Rc::make_mut(func))?;
            desugar_expr_mut(source, var_gen, Rc::make_mut(arg))
        }
        Expr::Lam { body, .. } => desugar_expr_mut(source, var_gen, Rc::make_mut(body)),
        Expr::Let { value, rest, .. } => {
            desugar_expr_mut(source, var_gen, Rc::make_mut(value))?;
            desugar_expr_mut(source, var_gen, Rc::make_mut(rest))
        }
        Expr::IfThenElse(cond, a, b) => {
            desugar_expr_mut(source, var_gen, Rc::make_mut(cond))?;
            desugar_expr_mut(source, var_gen, Rc::make_mut(a))?;
            desugar_expr_mut(source, var_gen, Rc::make_mut(b))
        }
        Expr::Binop(op, left, right) => {
            fn mk_desugared_binop(
                pos: usize,
                op: &str,
                left: Rc<Spanned<Expr>>,
                right: Rc<Spanned<Expr>>,
            ) -> Expr {
                Expr::App(
                    Rc::new(Spanned {
                        pos,
                        item: Expr::App(
                            Rc::new(Spanned {
                                pos,
                                item: Expr::mk_var(op),
                            }),
                            left,
                        ),
                    }),
                    right,
                )
            }

            desugar_expr_mut(source, var_gen, Rc::make_mut(left))?;
            desugar_expr_mut(source, var_gen, Rc::make_mut(right))?;

            match &op.item {
                Binop::Add
                | Binop::Multiply
                | Binop::Subtract
                | Binop::Divide
                | Binop::Append
                | Binop::Or
                | Binop::And
                | Binop::LApply
                | Binop::RApply => {}
                Binop::Eq => {
                    expr.item = mk_desugared_binop(expr.pos, "eq", left.clone(), right.clone());
                }
                Binop::Neq => {
                    expr.item = mk_desugared_binop(expr.pos, "neq", left.clone(), right.clone());
                }
                Binop::Gt => {
                    expr.item = mk_desugared_binop(expr.pos, "gt", left.clone(), right.clone());
                }
                Binop::Gte => {
                    expr.item = mk_desugared_binop(expr.pos, "gte", left.clone(), right.clone());
                }
                Binop::Lt => {
                    expr.item = mk_desugared_binop(expr.pos, "lt", left.clone(), right.clone());
                }
                Binop::Lte => {
                    expr.item = mk_desugared_binop(expr.pos, "lte", left.clone(), right.clone());
                }
            }

            Ok(())
        }
        Expr::String(parts) => parts
            .iter_mut()
            .try_for_each(|string_part| desugar_string_part_mut(source, var_gen, string_part)),
        Expr::Array(items) => items
            .iter_mut()
            .try_for_each(|expr| desugar_expr_mut(source, var_gen, expr)),
        Expr::Record { fields, rest } => {
            fields
                .iter_mut()
                .try_for_each(|(_, expr)| desugar_expr_mut(source, var_gen, expr))?;
            rest.iter_mut()
                .try_for_each(|expr| desugar_expr_mut(source, var_gen, Rc::make_mut(expr)))
        }
        Expr::Project(value, _) => desugar_expr_mut(source, var_gen, Rc::make_mut(value)),
        Expr::Embed(_, value) => desugar_expr_mut(source, var_gen, Rc::make_mut(value)),
        Expr::Case(expr, branches) => {
            desugar_expr_mut(source, var_gen, Rc::make_mut(expr))?;
            desugar_branches_mut(source, var_gen, branches)
        }
        Expr::Cmd(parts) => parts
            .iter_mut()
            .try_for_each(|cmd_part| desugar_cmd_part_mut(source, var_gen, cmd_part)),
        Expr::Comp(comp_lines) => {
            // [note: std::mem::take]
            let mut comp_lines = std::mem::take(comp_lines);

            comp_lines
                .iter_mut()
                .try_for_each(|comp_line| match &mut comp_line.item {
                    CompLine::Expr(expr) => desugar_expr_mut(source, var_gen, expr),
                    CompLine::Bind(_, expr) => desugar_expr_mut(source, var_gen, expr),
                    CompLine::Let(_, expr) => desugar_expr_mut(source, var_gen, expr),
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
                        desugar_expr_mut(source, var_gen, &mut last_expr)?;

                        *expr = comp_lines
                            .into_iter()
                            .rev()
                            .fold(last_expr, |expr, comp_line| {
                                let comp_line_pos = comp_line.pos;
                                match comp_line.item {
                                    CompLine::Expr(comp_line_expr) => {
                                        // io.andThen <comp_line_expr> (\_ -> <expr>)
                                        Expr::mk_app(
                                            Expr::mk_app(
                                                Spanned {
                                                    pos: comp_line_pos,
                                                    item: Expr::mk_project(
                                                        Spanned {
                                                            pos: comp_line.pos,
                                                            item: Expr::mk_var("io"),
                                                        },
                                                        Spanned {
                                                            pos: comp_line.pos,
                                                            item: String::from("andThen"),
                                                        },
                                                    ),
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
                                        // io.andThen <value> (\<name> -> <expr>)
                                        Expr::mk_app(
                                            Expr::mk_app(
                                                Spanned {
                                                    pos: comp_line_pos,
                                                    item: Expr::mk_project(
                                                        Spanned {
                                                            pos: comp_line.pos,
                                                            item: Expr::mk_var("io"),
                                                        },
                                                        Spanned {
                                                            pos: comp_line.pos,
                                                            item: String::from("andThen"),
                                                        },
                                                    ),
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
