//! Parsing expressions

mod test;

use crate::{
    between, choices, grammar::pattern::pattern, indent, indent_scope, keep_left, keep_right, many,
    map0, map2, operator::operator, optional, sep_by, spanned, ParseResult, Parser,
};
use ipso_lex::token;
use ipso_syntax::{Binop, Branch, CmdPart, CompLine, Expr, Keyword, Spanned, StringPart};
use std::rc::Rc;

/**
```text
comp_line ::=
  'bind' ident '<-' expr
  'let' ident '=' expr
  'return' expr
  expr
```
*/
pub fn comp_line(parser: &mut Parser) -> ParseResult<CompLine> {
    choices!(
        // 'bind' ident '<-' expr
        {
            keep_right!(
                parser.keyword(&Keyword::Bind),
                indent!(parser, Relation::Gt, spanned!(parser, parser.ident()))
            )
            .and_then(|name| {
                keep_right!(
                    indent!(parser, Relation::Gt, parser.token(&token::Data::LeftArrow)),
                    indent!(parser, Relation::Gt, expr(parser))
                )
                .map(|value| CompLine::Bind(name, value))
            })
        },
        // 'let' ident '=' expr
        {
            keep_right!(
                parser.keyword(&Keyword::Let),
                indent!(parser, Relation::Gt, spanned!(parser, parser.ident()))
            )
            .and_then(|name| {
                keep_right!(
                    indent!(parser, Relation::Gt, parser.token(&token::Data::Equals)),
                    indent!(parser, Relation::Gt, expr(parser))
                )
                .map(|value| CompLine::Let(name, value))
            })
        },
        expr(parser).map(CompLine::Expr)
    )
}

/**
```text
expr_comp ::=
  'comp' comp_line*
```
 */
pub fn expr_comp(parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
    spanned!(
        parser,
        indent_scope!(
            parser,
            keep_right!(
                indent!(parser, Relation::Eq, parser.keyword(&Keyword::Comp)),
                indent!(
                    parser,
                    Relation::Gt,
                    indent_scope!(
                        parser,
                        many!(indent!(parser, Relation::Eq, comp_line(parser))).map(Expr::Comp)
                    )
                )
            )
        )
    )
}

/**
```text
string_part_expr ::=
  '${' expr '}'
  '$' ident
```
*/
pub fn string_part_expr(parser: &mut Parser) -> ParseResult<StringPart> {
    choices!(
        between!(
            indent!(
                parser,
                Relation::Gte,
                parser.token(&token::Data::DollarLBrace)
            ),
            indent!(parser, Relation::Gte, parser.token(&token::Data::RBrace)),
            expr(parser).map(StringPart::Expr)
        ),
        keep_right!(
            indent!(parser, Relation::Gte, parser.token(&token::Data::Dollar)),
            spanned!(parser, parser.ident_owned().map(Expr::Var)).map(StringPart::Expr)
        )
    )
}

pub fn string_part_string(parser: &mut Parser) -> ParseResult<StringPart> {
    indent!(
        parser,
        Relation::Gte,
        parser.string().map(StringPart::String)
    )
}

/**
```text
string_part ::=
  string_part_expr
  string_part_string

string ::=
  '"' string_part* '"'
```
*/
pub fn string(parser: &mut Parser) -> ParseResult<Vec<StringPart>> {
    indent_scope!(parser, {
        between!(
            indent!(
                parser,
                Relation::Eq,
                parser.token(&token::Data::DoubleQuote)
            ),
            indent!(
                parser,
                Relation::Gte,
                parser.token(&token::Data::DoubleQuote)
            ),
            many!(choices!(
                string_part_expr(parser),
                string_part_string(parser)
            ))
        )
    })
}

/**
```text
expr_record_fields ::=
  ident '=' expr [',' expr_record_fields]
  '..' expr_atom
```
*/
pub fn expr_record_fields(
    parser: &mut Parser,
    fields: &mut Vec<(String, Spanned<Expr>)>,
) -> ParseResult<Option<Spanned<Expr>>> {
    choices!(
        keep_left!(
            indent!(parser, Relation::Gte, parser.ident_owned()),
            indent!(parser, Relation::Gte, parser.token(&token::Data::Equals))
        )
        .and_then(|name| {
            expr(parser).and_then(|expr| {
                fields.push((name, expr));
                optional!(keep_right!(
                    indent!(parser, Relation::Gte, parser.token(&token::Data::Comma)),
                    expr_record_fields(parser, fields)
                ))
                .map(|m_rest| match m_rest {
                    None => None,
                    Some(rest) => rest,
                })
            })
        }),
        keep_right!(
            indent!(parser, Relation::Gte, parser.token(&token::Data::DotDot)),
            indent!(parser, Relation::Gte, expr_atom(parser)).map(Some)
        )
    )
}

/**
```text
expr_record ::=
  '{' [expr_record_fields] '}'
```
*/
pub fn expr_record(parser: &mut Parser) -> ParseResult<Expr> {
    indent_scope!(parser, {
        between!(
            indent!(parser, Relation::Eq, parser.token(&token::Data::LBrace)),
            indent!(parser, Relation::Gte, parser.token(&token::Data::RBrace)),
            {
                let mut fields = Vec::new();

                expr_record_fields(parser, &mut fields).map(|rest| Expr::mk_record(fields, rest))
            }
        )
    })
}

/**
```text
expr_embed ::=
  '(|' ctor ',' '..' expr_atom '|)'
```
*/
pub fn expr_embed(parser: &mut Parser) -> ParseResult<Expr> {
    indent_scope!(parser, {
        between!(
            indent!(parser, Relation::Eq, parser.token(&token::Data::LParenPipe)),
            indent!(
                parser,
                Relation::Gte,
                parser.token(&token::Data::PipeRParen)
            ),
            indent!(parser, Relation::Gte, spanned!(parser, parser.ctor_owned())).and_then(
                |ctor| indent!(parser, Relation::Gte, parser.token(&token::Data::Comma)).and_then(
                    |_| keep_right!(
                        indent!(parser, Relation::Gte, parser.token(&token::Data::DotDot)),
                        expr_atom(parser).map(|rest| Expr::mk_embed(ctor, rest))
                    )
                )
            )
        )
    })
}

/**
```text
expr_array ::=
  '[' expr [',' expr] ']'
```
*/
pub fn expr_array(parser: &mut Parser) -> ParseResult<Expr> {
    indent_scope!(parser, {
        between!(
            indent!(parser, Relation::Eq, parser.token(&token::Data::LBracket)),
            indent!(parser, Relation::Gte, parser.token(&token::Data::RBracket)),
            sep_by!(
                expr(parser),
                indent!(parser, Relation::Gte, parser.token(&token::Data::Comma))
            )
        )
        .map(Expr::Array)
    })
}

/**
```text
cmd_part ::=
  cmd_char*
  '"' string_char* '"'
  '$' ident
```

```text
cmd_char ::=
  (ASCII characters excluding '\', '`', '"', '$')
  '\' '\'
  '\' '`'
  '\' '"'
  '\' '$'
```
*/
pub fn cmd_part(parser: &mut Parser) -> ParseResult<CmdPart> {
    choices!(
        {
            parser.expecting.insert(token::Name::Cmd);
            match &parser.current {
                None => ParseResult::unexpected(false),
                Some(token) => match &token.data {
                    token::Data::Cmd(value) => {
                        let value = value.clone();
                        map0!(CmdPart::Literal(value), parser.consume())
                    }
                    _ => ParseResult::unexpected(false),
                },
            }
        },
        between!(
            parser.token(&token::Data::DoubleQuote),
            parser.token(&token::Data::DoubleQuote),
            parser.string().map(Rc::from)
        )
        .map(CmdPart::Literal),
        keep_right!(
            parser.token(&token::Data::Dollar),
            spanned!(parser, parser.ident_owned())
        )
        .map(|value| CmdPart::Expr(Spanned {
            pos: value.pos,
            item: Expr::Var(value.item)
        }))
    )
}

/**
```text
expr_cmd ::=
  '`' cmd_part* '`'
```
*/
pub fn expr_cmd(parser: &mut Parser) -> ParseResult<Vec<CmdPart>> {
    between!(
        parser.token(&token::Data::Backtick),
        parser.token(&token::Data::Backtick),
        many!(cmd_part(parser))
    )
}

/**
```text
expr_atom ::=
  int
  char
  'false'
  'true'
  ident
  ctor
  expr_record
  expr_embed
  expr_array
  expr_cmd
  '(' expr ')'
  string
```
*/
pub fn expr_atom(parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
    spanned!(
        parser,
        choices!(
            parser.int().map(Expr::Int),
            parser.char().map(Expr::Char),
            parser.keyword(&Keyword::False).map(|_| Expr::False),
            parser.keyword(&Keyword::True).map(|_| Expr::True),
            parser.ident_owned().map(Expr::Var),
            spanned!(parser, parser.ctor_owned()).map(Expr::Variant),
            expr_record(parser),
            expr_embed(parser),
            expr_array(parser),
            expr_cmd(parser).map(Expr::Cmd),
            between!(
                parser.token(&token::Data::LParen),
                indent!(parser, Relation::Gt, parser.token(&token::Data::RParen)),
                optional!(indent!(parser, Relation::Gt, expr(parser))).map(|m_ty| {
                    match m_ty {
                        None => Expr::Unit,
                        Some(ty) => ty.item,
                    }
                })
            ),
            string(parser).map(Expr::String)
        )
    )
}

/**
```text
expr_project ::=
  expr_atom ('.' ident)*
```
*/
pub fn expr_project(parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
    expr_atom(parser).and_then(|val| {
        many!(keep_right!(
            indent!(parser, Relation::Gt, parser.token(&token::Data::Dot)),
            indent!(parser, Relation::Gt, parser.ident_owned())
        ))
        .map(|fields| {
            let mut expr = val;
            for field in fields {
                expr = Spanned {
                    pos: expr.pos,
                    item: Expr::mk_project(expr, field),
                };
            }
            expr
        })
    })
}

/**
```text
case_branch ::=
  pattern '->' expr
```
*/
pub fn case_branch(parser: &mut Parser) -> ParseResult<Branch> {
    spanned!(parser, indent!(parser, Relation::Eq, pattern(parser))).and_then(|pattern| {
        map2!(
            |_, body| Branch { pattern, body },
            indent!(parser, Relation::Gt, parser.token(&token::Data::Arrow)),
            expr(parser)
        )
    })
}

/**
```text
expr_case ::=
  'case' expr 'of' case_branch*
```
*/
pub fn expr_case(parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
    spanned!(
        parser,
        keep_right!(parser.keyword(&Keyword::Case), expr(parser)).and_then(|cond| {
            keep_right!(
                indent!(parser, Relation::Gt, parser.keyword(&Keyword::Of)),
                indent!(
                    parser,
                    Relation::Gt,
                    indent_scope!(parser, { many!(case_branch(parser)) })
                )
            )
            .map(|branches| Expr::mk_case(cond, branches))
        })
    )
}

/**
```text
expr_app ::=
  expr_project+
```
*/
pub fn expr_app(parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
    expr_project(parser).and_then(|first| {
        many!(indent!(parser, Relation::Gt, expr_project(parser)))
            .map(|rest| rest.into_iter().fold(first, Expr::mk_app))
    })
}

/**
```text
binop ::=
  '+'
  '*'
  '-'
  '/'
  '||'
  '&&'
  '=='
  '!='
  '<'
  '<='
  '>'
  '>='
```
*/
pub fn binop(parser: &mut Parser) -> ParseResult<Binop> {
    choices!(
        // +
        map0!(Binop::Add, parser.token(&token::Data::Plus)),
        // *
        map0!(Binop::Multiply, parser.token(&token::Data::Asterisk)),
        // -
        map0!(Binop::Subtract, parser.token(&token::Data::Hyphen)),
        // /
        map0!(Binop::Divide, parser.token(&token::Data::Slash)),
        keep_right!(
            parser.token(&token::Data::Pipe),
            choices!(
                // ||
                map0!(Binop::Or, parser.token(&token::Data::Pipe)),
                // |>
                map0!(Binop::RApply, parser.token(&token::Data::RAngle))
            )
        ),
        keep_right!(
            parser.token(&token::Data::Ampersand),
            // &&
            map0!(Binop::And, parser.token(&token::Data::Ampersand))
        ),
        keep_right!(
            parser.token(&token::Data::Equals),
            // ==
            map0!(Binop::Eq, parser.token(&token::Data::Equals))
        ),
        keep_right!(
            parser.token(&token::Data::Bang),
            // !=
            map0!(Binop::Neq, parser.token(&token::Data::Equals))
        ),
        keep_right!(
            parser.token(&token::Data::LAngle),
            choices!(
                // <=
                map0!(Binop::Lte, parser.token(&token::Data::Equals)),
                // <|
                map0!(Binop::LApply, parser.token(&token::Data::Pipe)),
                // <
                ParseResult::pure(Binop::Lt)
            )
        ),
        keep_right!(
            parser.token(&token::Data::RAngle),
            choices!(
                // >=
                map0!(Binop::Gte, parser.token(&token::Data::Equals)),
                // >
                ParseResult::pure(Binop::Gt)
            )
        )
    )
}

/**
```text
expr_op ::=
  expr_app (binop expr_app)*
```
*/
pub fn expr_op(parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
    struct Operators<'a, 'input> {
        parser: &'a mut Parser<'input>,
    }

    impl<'a, 'input> Iterator for Operators<'a, 'input> {
        type Item = ParseResult<(Spanned<Binop>, Spanned<Expr>)>;

        fn next(&mut self) -> Option<Self::Item> {
            let result = indent!(
                self.parser,
                Relation::Gt,
                spanned!(self.parser, binop(self.parser))
                    .and_then(|op| expr_app(self.parser).map(|expr| (op, expr)))
            );
            match result.result {
                Err(_) => {
                    if result.consumed {
                        Some(result)
                    } else {
                        None
                    }
                }
                Ok(_) => Some(result),
            }
        }
    }

    expr_app(parser).and_then(|first| {
        let mut operators = Operators { parser };
        operator(first, &mut operators)
    })
}

/**
```text
expr_lam ::=
  '\' pattern '->' expr
```
*/
pub fn expr_lam(parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
    spanned!(
        parser,
        keep_right!(
            parser.token(&token::Data::Backslash),
            many!(indent!(
                parser,
                Relation::Gt,
                spanned!(parser, pattern(parser))
            ))
            .and_then(|args| keep_right!(
                indent!(parser, Relation::Gt, parser.token(&token::Data::Arrow)),
                expr(parser).map(|body| syntax::Expr::mk_lam(args, body))
            ))
        )
    )
}

/**
```text
expr_ifthenelse ::=
  'if' expr 'then' expr 'else' expr
```
*/
pub fn expr_ifthenelse(parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
    spanned!(
        parser,
        keep_right!(
            parser.keyword(&Keyword::If),
            expr(parser).and_then(|cond| keep_right!(
                indent!(parser, Relation::Gt, parser.keyword(&Keyword::Then)),
                expr(parser).and_then(|then| keep_right!(
                    indent!(parser, Relation::Gt, parser.keyword(&Keyword::Else)),
                    expr(parser).map(|else_| syntax::Expr::mk_ifthenelse(cond, then, else_))
                ))
            ))
        )
    )
}

/**
```text
expr_let ::=
  'let' ident '=' expr 'in' expr
```
*/
pub fn expr_let(parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
    spanned!(
        parser,
        keep_right!(
            parser.keyword(&Keyword::Let),
            indent!(parser, Relation::Gt, parser.ident()).and_then(|name| {
                keep_right!(
                    indent!(parser, Relation::Gt, parser.token(&token::Data::Equals)),
                    expr(parser).and_then(|value| {
                        keep_right!(
                            indent!(parser, Relation::Gte, parser.keyword(&Keyword::In)),
                            expr(parser).map(|rest| {
                                syntax::Expr::Let {
                                    name,
                                    value: Rc::new(value),
                                    rest: Rc::new(rest),
                                }
                            })
                        )
                    })
                )
            })
        )
    )
}

/**
```text
expr ::=
  expr_case
  expr_lam
  expr_ifthenelse
  expr_let
  expr_comp
  expr_op
```
*/
pub fn expr(parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
    choices!(
        expr_case(parser),
        expr_lam(parser),
        expr_ifthenelse(parser),
        expr_let(parser),
        expr_comp(parser),
        expr_op(parser)
    )
}
