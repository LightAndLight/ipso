#[cfg(test)]
mod test;

use fnv::FnvHashSet;
use ipso_core::{Binop, Branch, Builtin, CmdPart, Name, Pattern, StringPart};
use ipso_syntax::ModuleRef;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Var(usize),
    Name(Name),
    Module {
        id: ModuleRef,
        path: Vec<String>,
        item: Name,
    },
    Builtin(Builtin),

    App(Rc<Expr>, Rc<Expr>),
    Lam {
        env: Vec<usize>,
        arg: bool,
        body: Rc<Expr>,
    },

    Let {
        value: Rc<Expr>,
        rest: Rc<Expr>,
    },

    True,
    False,
    IfThenElse(Rc<Expr>, Rc<Expr>, Rc<Expr>),

    Int(i32),

    Binop(Binop, Rc<Expr>, Rc<Expr>),

    Char(char),

    String(Vec<StringPart<Expr>>),

    Array(Vec<Expr>),

    Extend(Rc<Expr>, Rc<Expr>, Rc<Expr>),
    Record(Vec<(Expr, Expr)>),
    Project(Rc<Expr>, Rc<Expr>),

    Variant(Rc<Expr>),
    Embed(Rc<Expr>, Rc<Expr>),
    Case(Rc<Expr>, Vec<Branch<Expr>>),
    Unit,

    Cmd(Vec<CmdPart<Expr>>),
}

pub struct ConvertResult<A> {
    pub required_vars: FnvHashSet<usize>,
    pub build: Box<dyn FnOnce(&[usize]) -> A>,
}

impl<A: 'static> ConvertResult<A> {
    pub fn closed(value: A) -> ConvertResult<A> {
        Self {
            required_vars: FnvHashSet::default(),
            build: Box::new(move |_| value),
        }
    }

    pub fn map<B: 'static, F: 'static + FnOnce(A) -> B>(self, f: F) -> ConvertResult<B> {
        let ConvertResult {
            required_vars,
            build,
        } = self;

        ConvertResult {
            required_vars,
            build: Box::new(move |env| f(build(env))),
        }
    }

    pub fn and<B: 'static>(self, other: ConvertResult<B>) -> ConvertResult<(A, B)> {
        let build_left = self.build;
        let build_right = other.build;

        let required_by_both = {
            let mut vars = FnvHashSet::default();
            vars.extend(self.required_vars);
            vars.extend(other.required_vars);
            vars
        };
        ConvertResult {
            required_vars: required_by_both,
            build: Box::new(move |env| {
                let left = build_left(env);
                let right = build_right(env);
                (left, right)
            }),
        }
    }
}

pub fn convert_many<A: 'static, B: 'static, F: 'static + Fn(&A) -> ConvertResult<B>>(
    convert_item: F,
    items: &[A],
) -> ConvertResult<Vec<B>> {
    let items: Vec<ConvertResult<_>> = items.iter().map(convert_item).collect();

    let required_by_items = {
        let mut vars = FnvHashSet::default();
        items.iter().for_each(|item| {
            vars.extend(&item.required_vars);
        });
        vars
    };

    ConvertResult {
        required_vars: required_by_items,
        build: Box::new(move |env| items.into_iter().map(|item| (item.build)(env)).collect()),
    }
}

pub fn convert(expr: &ipso_core::Expr) -> Expr {
    let result = convert_expr(expr);
    (result.build)(&[])
}

pub fn convert_expr(expr: &ipso_core::Expr) -> ConvertResult<Expr> {
    fn get_position_of_var(env: &[usize], ix: usize) -> usize {
        match env
            .iter()
            .copied()
            // Search from the end, because we treat the last item as having de bruijn index 0
            .rev()
            .enumerate()
            .find_map(|(position, var)| if var == ix { Some(position) } else { None })
        {
            Some(position) => position,
            None => panic!("variable {} missing from env {:?}", ix, env),
        }
    }

    match expr {
        ipso_core::Expr::Var(ix) => {
            let ix = *ix;

            let required_vars = {
                let mut vars = FnvHashSet::default();
                vars.insert(ix);
                vars
            };
            ConvertResult {
                required_vars,
                build: Box::new(move |env| Expr::Var(get_position_of_var(env, ix))),
            }
        }

        ipso_core::Expr::Lam { arg, body } => {
            let arg = *arg;

            let body = convert_expr(body);
            let build_body = body.build;

            let required_by_lam = if arg {
                body.required_vars
                    .iter()
                    .copied()
                    .filter_map(|var| if var == 0 { None } else { Some(var - 1) })
                    .collect()
            } else {
                body.required_vars
            };
            ConvertResult {
                required_vars: required_by_lam.clone(),
                build: Box::new(move |env| {
                    let new_env: Vec<usize> = if arg {
                        let env: Vec<usize> = env
                            .iter()
                            .copied()
                            .filter(|var| required_by_lam.contains(var))
                            .collect();

                        env.iter()
                            .copied()
                            .map(|var| var + 1)
                            .chain(std::iter::once(0))
                            .collect()
                    } else {
                        env.iter()
                            .copied()
                            .filter(|var| required_by_lam.contains(var))
                            .collect()
                    };

                    let body = build_body(&new_env);

                    let env: Vec<usize> = if arg {
                        new_env
                            .into_iter()
                            .filter_map(|var| if var == 0 { None } else { Some(var - 1) })
                            .map(|ix| get_position_of_var(env, ix))
                            .collect()
                    } else {
                        new_env
                            .into_iter()
                            .map(|ix| get_position_of_var(env, ix))
                            .collect()
                    };
                    Expr::Lam {
                        env,
                        arg,
                        body: Rc::new(body),
                    }
                }),
            }
        }

        // Compound expressions.
        ipso_core::Expr::App(f, x) => convert_expr(f)
            .and(convert_expr(x))
            .map(move |(f, x)| Expr::App(Rc::new(f), Rc::new(x))),
        ipso_core::Expr::Let { value, rest } => {
            let value = convert_expr(value);
            let build_value = value.build;

            let ConvertResult {
                build: build_rest,
                required_vars: rest_required_vars,
            } = convert_expr(rest);

            let required_by_let = {
                let mut vars = FnvHashSet::default();
                vars.extend(value.required_vars);
                vars.extend(rest_required_vars.iter().copied().filter_map(|var| {
                    if var == 0 {
                        None
                    } else {
                        Some(var - 1)
                    }
                }));
                vars
            };
            ConvertResult {
                required_vars: required_by_let,
                build: Box::new(move |env| {
                    let value = build_value(env);

                    let rest = {
                        let env: Vec<usize> = env
                            .iter()
                            .copied()
                            .map(|var| var + 1)
                            .chain(std::iter::once(0))
                            .collect();
                        build_rest(&env)
                    };

                    Expr::Let {
                        value: Rc::new(value),
                        rest: Rc::new(rest),
                    }
                }),
            }
        }
        ipso_core::Expr::IfThenElse(cond, then_expr, else_expr) => convert_expr(cond)
            .and(convert_expr(then_expr))
            .and(convert_expr(else_expr))
            .map(move |((cond, then_expr), else_expr)| {
                Expr::IfThenElse(Rc::new(cond), Rc::new(then_expr), Rc::new(else_expr))
            }),
        ipso_core::Expr::Binop(op, left, right) => {
            let op = *op;
            convert_expr(left)
                .and(convert_expr(right))
                .map(move |(left, right)| Expr::Binop(op, Rc::new(left), Rc::new(right)))
        }
        ipso_core::Expr::String(parts) => {
            convert_many(convert_string_part, parts).map(Expr::String)
        }
        ipso_core::Expr::Array(items) => convert_many(convert_expr, items).map(Expr::Array),
        ipso_core::Expr::Extend(index, value, record) => convert_expr(index)
            .and(convert_expr(value))
            .and(convert_expr(record))
            .map(move |((index, value), record)| {
                Expr::Extend(Rc::new(index), Rc::new(value), Rc::new(record))
            }),
        ipso_core::Expr::Record(fields) => convert_many(
            |field: &(ipso_core::Expr, ipso_core::Expr)| {
                convert_expr(&field.0).and(convert_expr(&field.1))
            },
            fields,
        )
        .map(Expr::Record),

        ipso_core::Expr::Project(record, index) => convert_expr(record)
            .and(convert_expr(index))
            .map(move |(record, index)| Expr::Project(Rc::new(record), Rc::new(index))),
        ipso_core::Expr::Variant(tag) => {
            convert_expr(tag).map(move |tag| Expr::Variant(Rc::new(tag)))
        }
        ipso_core::Expr::Embed(tag, variant) => convert_expr(tag)
            .and(convert_expr(variant))
            .map(move |(tag, variant)| Expr::Embed(Rc::new(tag), Rc::new(variant))),
        ipso_core::Expr::Case(expr, branches) => {
            let expr = convert_expr(expr);
            let build_expr = expr.build;

            let branches = convert_many(convert_branch, branches);
            let build_branches = branches.build;

            let required_by_case = {
                let mut vars = FnvHashSet::default();
                vars.extend(expr.required_vars);
                vars.extend(branches.required_vars);
                vars
            };
            ConvertResult {
                required_vars: required_by_case,
                build: Box::new(move |env| {
                    let expr = build_expr(env);
                    let branches = build_branches(env);

                    Expr::Case(Rc::new(expr), branches)
                }),
            }
        }
        ipso_core::Expr::Cmd(parts) => convert_many(convert_cmd_part, parts).map(Expr::Cmd),

        // Simple expressions.
        ipso_core::Expr::Name(name) => ConvertResult::closed(Expr::Name(name.clone())),
        ipso_core::Expr::True => ConvertResult::closed(Expr::True),
        ipso_core::Expr::False => ConvertResult::closed(Expr::False),
        ipso_core::Expr::Int(i) => ConvertResult::closed(Expr::Int(*i)),
        ipso_core::Expr::Char(c) => ConvertResult::closed(Expr::Char(*c)),
        ipso_core::Expr::Unit => ConvertResult::closed(Expr::Unit),
        ipso_core::Expr::Builtin(builtin) => ConvertResult::closed(Expr::Builtin(*builtin)),
        ipso_core::Expr::Module { id, path, item } => ConvertResult::closed(Expr::Module {
            id: *id,
            path: path.clone(),
            item: item.clone(),
        }),

        // Variants that should have been eleminated by this stage.
        ipso_core::Expr::EVar(evar) => panic!("found evar: {:?}", evar),
        ipso_core::Expr::Placeholder(placeholder) => {
            panic!("found placeholder: {:?}", placeholder)
        }
    }
}

pub fn convert_string_part(part: &StringPart<ipso_core::Expr>) -> ConvertResult<StringPart<Expr>> {
    match part {
        StringPart::String(string) => {
            let string = string.clone();
            ConvertResult::closed(StringPart::String(string))
        }
        StringPart::Expr(expr) => convert_expr(expr).map(StringPart::Expr),
    }
}

pub fn convert_cmd_part(part: &CmdPart<ipso_core::Expr>) -> ConvertResult<CmdPart<Expr>> {
    match part {
        CmdPart::Literal(string) => {
            let string = string.clone();
            ConvertResult::closed(CmdPart::Literal(string))
        }
        CmdPart::Expr(expr) => convert_expr(expr).map(CmdPart::Expr),
    }
}

pub fn convert_branch(branch: &Branch<ipso_core::Expr>) -> ConvertResult<Branch<Expr>> {
    let Branch { pattern, body } = branch;
    let bound_vars = pattern.bound_vars();

    let pattern = convert_pattern(pattern);
    let build_pattern = pattern.build;

    let body = convert_expr(body);

    let required_by_branch = {
        let mut vars = FnvHashSet::default();

        vars.extend(pattern.required_vars);

        let required_by_body = body.required_vars.iter().filter_map(|var| {
            if *var < bound_vars {
                None
            } else {
                Some(var - bound_vars)
            }
        });
        vars.extend(required_by_body);

        vars
    };
    ConvertResult {
        required_vars: required_by_branch,
        build: Box::new(move |env| {
            let pattern = build_pattern(env);

            let body = {
                let env: Vec<usize> = env
                    .iter()
                    .copied()
                    .map(|var| var + bound_vars)
                    .chain((0..bound_vars).rev())
                    .collect();
                (body.build)(&env)
            };
            Branch { pattern, body }
        }),
    }
}

pub fn convert_pattern(pattern: &Pattern<ipso_core::Expr>) -> ConvertResult<Pattern<Expr>> {
    match pattern {
        ipso_core::Pattern::Record { names, rest } => {
            let rest = *rest;
            convert_many(convert_expr, names).map(move |names| Pattern::Record { names, rest })
        }
        ipso_core::Pattern::Variant { tag } => {
            convert_expr(tag).map(move |tag| Pattern::Variant { tag: Rc::new(tag) })
        }
        ipso_core::Pattern::Name => ConvertResult::closed(Pattern::Name),
        ipso_core::Pattern::Char(c) => ConvertResult::closed(Pattern::Char(*c)),
        ipso_core::Pattern::Int(i) => ConvertResult::closed(Pattern::Int(*i)),
        ipso_core::Pattern::String(s) => ConvertResult::closed(Pattern::String(s.clone())),
        ipso_core::Pattern::Wildcard => ConvertResult::closed(Pattern::Wildcard),
    }
}
