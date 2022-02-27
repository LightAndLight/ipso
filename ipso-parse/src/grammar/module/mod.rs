//! Parsing modules

mod test;

use crate::{
    between, choices,
    grammar::{expr::expr, pattern::pattern, r#type::type_},
    indent, indent_scope, keep_left, keep_right, many, many_, map0, optional, sep_by, spanned,
    ParseResult, Parser,
};
use ipso_syntax::{Declaration, Expr, Keyword, Module, Names, Pattern, Spanned, Type};
use std::rc::Rc;

/**
```text
definition ::=
  ident ':' type ident pattern* '=' expr
```
*/
pub fn definition(parser: &mut Parser) -> ParseResult<Declaration> {
    indent_scope!(parser, {
        keep_left!(
            indent!(parser, Relation::Eq, parser.ident_owned()),
            indent!(parser, Relation::Gt, parser.token(&token::Data::Colon))
        )
        .and_then(|name| {
            type_(parser).and_then(|ty| {
                keep_right!(
                    indent!(
                        parser,
                        Relation::Eq,
                        parser.token(&token::Data::Ident(Rc::from(name.as_ref())))
                    ),
                    many!(indent!(
                        parser,
                        Relation::Gt,
                        spanned!(parser, pattern(parser))
                    ))
                )
                .and_then(|args| {
                    keep_right!(
                        indent!(parser, Relation::Gt, parser.token(&token::Data::Equals)),
                        indent!(parser, Relation::Gt, expr(parser)).map(|body| {
                            Declaration::Definition {
                                name,
                                ty,
                                args,
                                body,
                            }
                        })
                    )
                })
            })
        })
    })
}

/**
```text
type_alias ::=
  'type' ctor '=' type
```
*/
pub fn type_alias(parser: &mut Parser) -> ParseResult<Declaration> {
    indent_scope!(parser, {
        keep_right!(
            indent!(parser, Relation::Eq, parser.keyword(&Keyword::Type)),
            indent!(parser, Relation::Gt, parser.ctor_owned()).and_then(|name| many!(
                parser.ident_owned()
            )
            .and_then(|args| keep_right!(
                indent!(parser, Relation::Gt, parser.token(&token::Data::Equals)),
                indent!(parser, Relation::Gt, type_(parser)).map(|body| Declaration::TypeAlias {
                    name,
                    args,
                    body
                })
            )))
        )
    })
}

/**
```text
import ::=
  'import' ident ['as' ident]
```
*/
pub fn import(parser: &mut Parser) -> ParseResult<Declaration> {
    indent_scope!(parser, {
        keep_right!(
            indent!(parser, Relation::Eq, parser.keyword(&Keyword::Import)),
            spanned!(parser, indent!(parser, Relation::Gt, parser.ident_owned())).and_then(
                |module| {
                    optional!(keep_right!(
                        indent!(parser, Relation::Gt, parser.keyword(&Keyword::As)),
                        spanned!(parser, indent!(parser, Relation::Gt, parser.ident_owned()))
                    ))
                    .map(|name| Declaration::Import { module, name })
                }
            )
        )
    })
}

/**
```text
from_import_choices ::=
  '*'
  ident [',' ident]

from_import ::=
  'from' ident 'import'
```
*/
pub fn from_import(parser: &mut Parser) -> ParseResult<Declaration> {
    indent_scope!(parser, {
        keep_right!(
            parser.keyword(&Keyword::From),
            spanned!(parser, indent!(parser, Relation::Gt, parser.ident_owned())).and_then(
                |module| keep_right!(
                    indent!(parser, Relation::Gt, parser.keyword(&Keyword::Import)),
                    choices!(
                        map0!(
                            Names::All,
                            indent!(parser, Relation::Gt, parser.token(&token::Data::Asterisk))
                        ),
                        sep_by!(
                            indent!(parser, Relation::Gt, parser.ident_owned()),
                            indent!(parser, Relation::Gt, parser.token(&token::Data::Comma))
                        )
                        .map(Names::Names)
                    )
                    .map(|names| Declaration::FromImport { module, names })
                )
            )
        )
    })
}

/**
```text
assumptions ::=
  epsilon
  '(' type* ')' '=>'
```
*/
pub fn assumptions(parser: &mut Parser) -> ParseResult<Vec<Spanned<Type<Rc<str>>>>> {
    indent_scope!(parser, {
        optional!(between!(
            indent!(parser, Relation::Eq, parser.token(&token::Data::LParen)),
            indent!(parser, Relation::Gte, parser.token(&token::Data::RParen)),
            many!(spanned!(
                parser,
                indent!(parser, Relation::Gte, type_(parser))
            ))
        ))
        .and_then(|m_tys| match m_tys {
            None => ParseResult::pure(Vec::new()),
            Some(tys) => {
                indent!(parser, Relation::Gte, parser.token(&token::Data::FatArrow)).map(|_| tys)
            }
        })
    })
}

/**
```text
class_member ::=
  ident ':' type
```
*/
pub fn class_member(parser: &mut Parser) -> ParseResult<(String, Type<Rc<str>>)> {
    parser.ident_owned().and_then(|name| {
        keep_right!(
            indent!(parser, Relation::Gt, parser.token(&token::Data::Colon)),
            indent!(parser, Relation::Gt, type_(parser)).map(|type_| (name, type_))
        )
    })
}

/**
```text
class ::=
  'class' assumptions ctor ident* 'where' class_member*
```
*/
pub fn class(parser: &mut Parser) -> ParseResult<Declaration> {
    indent_scope!(parser, {
        keep_right!(
            indent!(parser, Relation::Eq, parser.keyword(&Keyword::Class)),
            indent!(parser, Relation::Gt, assumptions(parser)).and_then(|supers| {
                indent!(parser, Relation::Gt, parser.ctor()).and_then(|name| {
                    many!(spanned!(
                        parser,
                        indent!(parser, Relation::Gt, parser.ident())
                    ))
                    .and_then(|args| {
                        keep_right!(
                            indent!(parser, Relation::Gt, parser.keyword(&Keyword::Where)),
                            indent!(
                                parser,
                                Relation::Gt,
                                indent_scope!(
                                    parser,
                                    many!(indent!(parser, Relation::Eq, class_member(parser))).map(
                                        |members| {
                                            Declaration::Class {
                                                supers,
                                                name,
                                                args,
                                                members,
                                            }
                                        }
                                    )
                                )
                            )
                        )
                    })
                })
            })
        )
    })
}

/*
```text
instance_member ::=
  ident pattern* '=' expr
```
*/
pub fn instance_member(
    parser: &mut Parser,
) -> ParseResult<(Spanned<String>, Vec<Spanned<Pattern>>, Spanned<Expr>)> {
    spanned!(parser, parser.ident_owned()).and_then(|name| {
        many!(indent!(
            parser,
            Relation::Gt,
            spanned!(parser, pattern(parser))
        ))
        .and_then(|args| {
            keep_right!(
                indent!(parser, Relation::Gt, parser.token(&token::Data::Equals)),
                expr(parser).map(|body| (name, args, body))
            )
        })
    })
}

/**
```text
instance ::=
  'instance' assumptions ctor type* 'where' instance_member*
```
*/
pub fn instance(parser: &mut Parser) -> ParseResult<Declaration> {
    indent_scope!(parser, {
        keep_right!(
            indent!(parser, Relation::Eq, parser.keyword(&Keyword::Instance)),
            indent!(parser, Relation::Gt, assumptions(parser)).and_then(|assumes| {
                spanned!(parser, indent!(parser, Relation::Gt, parser.ctor())).and_then(|name| {
                    many!(indent!(
                        parser,
                        Relation::Gt,
                        spanned!(parser, type_(parser))
                    ))
                    .and_then(|args| {
                        keep_right!(
                            indent!(parser, Relation::Gt, parser.keyword(&Keyword::Where)),
                            indent!(
                                parser,
                                Relation::Gt,
                                indent_scope!(
                                    parser,
                                    many!(indent!(parser, Relation::Eq, instance_member(parser)))
                                        .map(|members| {
                                            Declaration::Instance {
                                                assumes,
                                                name,
                                                args,
                                                members,
                                            }
                                        })
                                )
                            )
                        )
                    })
                })
            })
        )
    })
}

/**
```text
declaration ::=
  definition
  type_alias
  import
  from_import
  class
  instance
```
*/
pub fn declaration(parser: &mut Parser) -> ParseResult<Declaration> {
    keep_right!(
        many_!(parser.comment()),
        choices!(
            definition(parser),
            type_alias(parser),
            import(parser),
            from_import(parser),
            class(parser),
            instance(parser)
        )
    )
}

/**
```text
module ::=
  declaration*
```
*/
pub fn module(parser: &mut Parser) -> ParseResult<Module> {
    indent_scope!(
        parser,
        many!(spanned!(
            parser,
            indent!(parser, Relation::Eq, declaration(parser))
        ))
    )
    .map(|decls| Module { decls })
}
