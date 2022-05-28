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

    Int(u32),

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

macro_rules! convert_one {
    ($target:expr, $build:expr) => {{
        let ConvertResult {
            required_vars,
            build,
        } = convert_expr($target);

        ConvertResult {
            required_vars,
            build: Box::new(move |env| {
                let target = build(env);
                $build(Rc::new(target))
            }),
        }
    }};
}

macro_rules! convert_two {
    ($left:expr, $right:expr, $build:expr) => {{
        let left = convert_expr($left);
        let build_left = left.build;

        let right = convert_expr($right);
        let build_right = right.build;

        let required_by_both = {
            let mut vars = FnvHashSet::default();
            vars.extend(left.required_vars);
            vars.extend(right.required_vars);
            vars
        };
        ConvertResult {
            required_vars: required_by_both,
            build: Box::new(move |env| {
                let left = build_left(env);
                let right = build_right(env);
                $build(Rc::new(left), Rc::new(right))
            }),
        }
    }};
}

macro_rules! convert_three {
    ($left:expr, $middle:expr, $right:expr, $build:expr) => {{
        let left = convert_expr($left);
        let build_left = left.build;

        let middle = convert_expr($middle);
        let build_middle = middle.build;

        let right = convert_expr($right);
        let build_right = right.build;

        let required_by_all = {
            let mut vars = FnvHashSet::default();
            vars.extend(left.required_vars);
            vars.extend(middle.required_vars);
            vars.extend(right.required_vars);
            vars
        };
        ConvertResult {
            required_vars: required_by_all,
            build: Box::new(move |env| {
                let left = build_left(env);
                let middle = build_middle(env);
                let right = build_right(env);
                $build(Rc::new(left), Rc::new(middle), Rc::new(right))
            }),
        }
    }};
}

pub fn convert(expr: &ipso_core::Expr) -> Expr {
    let result = convert_expr(expr);
    (result.build)(&[])
}

pub struct ConvertResult<A> {
    pub required_vars: FnvHashSet<usize>,
    pub build: Box<dyn FnOnce(&[usize]) -> A>,
}

impl<A> ConvertResult<A> {
    pub fn closed(value: A) -> ConvertResult<A>
    where
        A: 'static,
    {
        Self {
            required_vars: FnvHashSet::default(),
            build: Box::new(move |_| value),
        }
    }
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
        ipso_core::Expr::App(f, x) => {
            convert_two!(f, x, Expr::App)
        }
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
                            .filter(|var| rest_required_vars.contains(var))
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
        ipso_core::Expr::IfThenElse(cond, then_expr, else_expr) => {
            convert_three!(cond, then_expr, else_expr, Expr::IfThenElse)
        }
        ipso_core::Expr::Binop(op, left, right) => {
            let op = *op;

            convert_two!(left, right, |left, right| Expr::Binop(op, left, right))
        }
        ipso_core::Expr::String(parts) => {
            let parts: Vec<ConvertResult<StringPart<Expr>>> =
                parts.iter().map(|part| convert_string_part(part)).collect();

            let required_by_parts = {
                let mut vars = FnvHashSet::default();
                parts.iter().for_each(|part| {
                    vars.extend(&part.required_vars);
                });
                vars
            };
            ConvertResult {
                required_vars: required_by_parts,
                build: Box::new(move |env| {
                    let parts = parts.into_iter().map(|part| (part.build)(env)).collect();
                    Expr::String(parts)
                }),
            }
        }
        ipso_core::Expr::Array(items) => {
            let items: Vec<ConvertResult<Expr>> =
                items.iter().map(|item| convert_expr(item)).collect();

            let required_by_items = {
                let mut vars = FnvHashSet::default();
                items.iter().for_each(|item| {
                    vars.extend(&item.required_vars);
                });
                vars
            };
            ConvertResult {
                required_vars: required_by_items,
                build: Box::new(move |env| {
                    let items = items.into_iter().map(|item| (item.build)(env)).collect();
                    Expr::Array(items)
                }),
            }
        }
        ipso_core::Expr::Extend(index, value, record) => {
            convert_three!(index, value, record, Expr::Extend)
        }
        ipso_core::Expr::Record(fields) => {
            let fields: Vec<(ConvertResult<Expr>, ConvertResult<Expr>)> = fields
                .iter()
                .map(|(index, value)| (convert_expr(index), convert_expr(value)))
                .collect();

            let required_by_fields = {
                let mut vars = FnvHashSet::default();
                fields.iter().for_each(|(index, value)| {
                    vars.extend(&index.required_vars);
                    vars.extend(&value.required_vars);
                });
                vars
            };
            ConvertResult {
                required_vars: required_by_fields,
                build: Box::new(move |env| {
                    let fields = fields
                        .into_iter()
                        .map(|(index, value)| {
                            let index = (index.build)(env);
                            let value = (value.build)(env);

                            (index, value)
                        })
                        .collect();
                    Expr::Record(fields)
                }),
            }
        }
        ipso_core::Expr::Project(record, index) => {
            convert_two!(record, index, Expr::Project)
        }
        ipso_core::Expr::Variant(tag) => convert_one!(tag, Expr::Variant),
        ipso_core::Expr::Embed(tag, variant) => convert_two!(tag, variant, Expr::Embed),
        ipso_core::Expr::Case(expr, branches) => {
            let expr = convert_expr(expr);
            let build_expr = expr.build;

            let branches: Vec<ConvertResult<Branch<Expr>>> = branches
                .iter()
                .map(|branch| convert_branch(branch))
                .collect();

            let required_by_case = {
                let mut vars = FnvHashSet::default();
                vars.extend(expr.required_vars);
                branches.iter().for_each(|branch| {
                    vars.extend(&branch.required_vars);
                });
                vars
            };
            ConvertResult {
                required_vars: required_by_case,
                build: Box::new(move |env| {
                    let expr = build_expr(env);
                    let branches = branches
                        .into_iter()
                        .map(|branch| (branch.build)(env))
                        .collect();

                    Expr::Case(Rc::new(expr), branches)
                }),
            }
        }
        ipso_core::Expr::Cmd(parts) => {
            let parts: Vec<ConvertResult<CmdPart<Expr>>> =
                parts.iter().map(|part| convert_cmd_part(part)).collect();

            let required_by_parts = {
                let mut vars = FnvHashSet::default();
                parts.iter().for_each(|item| {
                    vars.extend(&item.required_vars);
                });
                vars
            };
            ConvertResult {
                required_vars: required_by_parts,
                build: Box::new(move |env| {
                    let parts = parts.into_iter().map(|part| (part.build)(env)).collect();
                    Expr::Cmd(parts)
                }),
            }
        }

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
        StringPart::Expr(expr) => {
            let expr = convert_expr(expr);
            let build_expr = expr.build;

            ConvertResult {
                required_vars: expr.required_vars,
                build: Box::new(move |env| {
                    let expr = build_expr(env);
                    StringPart::Expr(expr)
                }),
            }
        }
    }
}

pub fn convert_cmd_part(part: &CmdPart<ipso_core::Expr>) -> ConvertResult<CmdPart<Expr>> {
    match part {
        CmdPart::Literal(string) => {
            let string = string.clone();
            ConvertResult::closed(CmdPart::Literal(string))
        }
        CmdPart::Expr(expr) => {
            let expr = convert_expr(expr);
            let build_expr = expr.build;

            ConvertResult {
                required_vars: expr.required_vars,
                build: Box::new(move |env| CmdPart::Expr(build_expr(env))),
            }
        }
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

            let names: Vec<ConvertResult<Expr>> =
                names.iter().map(|name| convert_expr(name)).collect();

            let required_by_names = {
                let mut vars = FnvHashSet::default();
                names
                    .iter()
                    .for_each(|name| vars.extend(&name.required_vars));
                vars
            };
            ConvertResult {
                required_vars: required_by_names,
                build: Box::new(move |env| {
                    let names: Vec<Expr> =
                        names.into_iter().map(|name| (name.build)(env)).collect();
                    Pattern::Record { names, rest }
                }),
            }
        }
        ipso_core::Pattern::Variant { tag } => {
            convert_one!(tag, |tag| Pattern::Variant { tag })
        }
        ipso_core::Pattern::Name => ConvertResult::closed(Pattern::Name),
        ipso_core::Pattern::Char(c) => ConvertResult::closed(Pattern::Char(*c)),
        ipso_core::Pattern::Int(i) => ConvertResult::closed(Pattern::Int(*i)),
        ipso_core::Pattern::String(s) => ConvertResult::closed(Pattern::String(s.clone())),
        ipso_core::Pattern::Wildcard => ConvertResult::closed(Pattern::Wildcard),
    }
}
