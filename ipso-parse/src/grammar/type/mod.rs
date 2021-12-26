//! Parsing types

mod test;

use crate::{
    between, choices, indent, indent_scope, keep_right, many, map0, map2, optional, ParseResult,
    Parser,
};
use ipso_syntax::Type;
use std::rc::Rc;

/// ```text
/// type_record_fields ::=
///  [ident [':' type [',' type_record_fields]]
/// ```
pub fn type_record_fields(
    parser: &mut Parser,
    fields: &mut Vec<(Rc<str>, Type<Rc<str>>)>,
) -> ParseResult<Option<Type<Rc<str>>>> {
    /*
    `indent_gte` allows the record type's contents to be in any column
    greater than or equal to the record type's opening brace.
    */
    optional!(indent!(parser, Relation::Gte, parser.ident())).and_then(|m_ident| match m_ident {
        None => ParseResult::pure(None),
        Some(ident) => optional!(keep_right!(
            indent!(parser, Relation::Gte, parser.token(&token::Data::Colon)),
            indent!(parser, Relation::Gte, type_(parser))
        ))
        .and_then(|m_ty| match m_ty {
            None => ParseResult::pure(Some(Type::Var(ident))),
            Some(ty) => {
                fields.push((ident, ty));
                optional!(keep_right!(
                    indent!(parser, Relation::Gte, parser.token(&token::Data::Comma)),
                    indent!(parser, Relation::Gte, type_record_fields(parser, fields))
                ))
                .map(|m_rest| match m_rest {
                    None => None,
                    Some(rest) => rest,
                })
            }
        }),
    })
}

/**
```text
type_record ::=
  '{' type_record_fields '}'
```
 */
pub fn type_record(parser: &mut Parser) -> ParseResult<Type<Rc<str>>> {
    indent_scope!(parser, {
        between!(
            indent!(parser, Relation::Eq, parser.token(&token::Data::LBrace)),
            indent!(parser, Relation::Gte, parser.token(&token::Data::RBrace)),
            {
                let mut fields = Vec::new();
                type_record_fields(parser, &mut fields).map(|rest| Type::mk_record(fields, rest))
            }
        )
    })
}

/**
```text
type_variant_ctors ::=
  ident
  ctor ':' type ['|' type_variant_ctors]
```
*/
pub fn type_variant_ctors(
    parser: &mut Parser,
    ctors: &mut Vec<(Rc<str>, Type<Rc<str>>)>,
) -> ParseResult<Option<Type<Rc<str>>>> {
    choices!(
        indent!(parser, Relation::Gte, parser.ident()).map(|x| Some(Type::Var(x))),
        indent!(parser, Relation::Gte, parser.ctor())
            .and_then(|ctor| map0!(
                ctor,
                indent!(parser, Relation::Gte, parser.token(&token::Data::Colon))
            ))
            .and_then(|ctor| type_(parser).map(|ty| (ctor, ty)))
            .and_then(|(ctor, ty)| {
                ctors.push((ctor, ty));
                optional!(
                    indent!(parser, Relation::Gte, parser.token(&token::Data::Pipe))
                        .and_then(|_| type_variant_ctors(parser, ctors))
                )
                .map(|m_rest| match m_rest {
                    None => None,
                    Some(rest) => rest,
                })
            })
    )
}

/**
```text
type_variant ::=
  '<' [type_variant_ctors] '>
```
 */
pub fn type_variant(parser: &mut Parser) -> ParseResult<Type<Rc<str>>> {
    indent_scope!(parser, {
        between!(
            indent!(parser, Relation::Eq, parser.token(&token::Data::LAngle)),
            indent!(parser, Relation::Gte, parser.token(&token::Data::RAngle)),
            {
                let mut ctors = Vec::new();
                optional!(type_variant_ctors(parser, &mut ctors)).map(|m_rest| match m_rest {
                    None => Type::mk_variant(Vec::new(), None),
                    Some(rest) => Type::mk_variant(ctors, rest),
                })
            }
        )
    })
}

/**
```text
type_atom ::=
  'Bool'
  'Int'
  'Char'
  'String'
  'Array'
  'IO'
  'Cmd'
  type_record
  type_variant
  ident
  ctor
  '(' type ')'
```
*/
pub fn type_atom(parser: &mut Parser) -> ParseResult<Type<Rc<str>>> {
    indent!(
        parser,
        Relation::Gt,
        choices!(
            map0!(
                Type::Bool,
                parser.token(&token::Data::Ident(Rc::from("Bool")))
            ),
            map0!(
                Type::Int,
                parser.token(&token::Data::Ident(Rc::from("Int")))
            ),
            map0!(
                Type::Char,
                parser.token(&token::Data::Ident(Rc::from("Char")))
            ),
            map0!(
                Type::String,
                parser.token(&token::Data::Ident(Rc::from("String")))
            ),
            map0!(
                Type::Array,
                parser.token(&token::Data::Ident(Rc::from("Array")))
            ),
            map0!(Type::IO, parser.token(&token::Data::Ident(Rc::from("IO")))),
            map0!(
                Type::Cmd,
                parser.token(&token::Data::Ident(Rc::from("Cmd")))
            ),
            type_record(parser),
            type_variant(parser),
            parser.ctor().map(Type::Name),
            parser.ident().map(Type::Var),
            between!(
                indent!(parser, Relation::Gt, parser.token(&token::Data::LParen)),
                indent!(parser, Relation::Gt, parser.token(&token::Data::RParen)),
                optional!(indent!(parser, Relation::Gt, type_(parser)))
                    .map(|m_ty| m_ty.unwrap_or(Type::Unit))
            )
        )
    )
}

/**
```text
type_app ::=
  type_atom+
```
 */
pub fn type_app(parser: &mut Parser) -> ParseResult<Type<Rc<str>>> {
    type_atom(parser).and_then(|first| {
        many!(type_atom(parser)).map(|rest| rest.into_iter().fold(first, Type::mk_app))
    })
}

/**
```text
type_arrow ::=
  type_app ['->' type_arrow]
```
 */
pub fn type_arrow(parser: &mut Parser) -> ParseResult<Type<Rc<str>>> {
    type_app(parser).and_then(|a| {
        optional!(map2!(
            |_, ty| ty,
            indent!(parser, Relation::Gt, parser.token(&token::Data::Arrow)),
            type_arrow(parser)
        ))
        .map(|m_b| match m_b {
            None => a,
            Some(b) => Type::mk_arrow(a, b),
        })
    })
}

/**
```text
type_fatarrow ::=
  type_arrow ['=>' type_fatarrow]
```
 */
pub fn type_fatarrow(parser: &mut Parser) -> ParseResult<Type<Rc<str>>> {
    type_arrow(parser).and_then(|a| {
        optional!(map2!(
            |_, ty| ty,
            indent!(parser, Relation::Gt, parser.token(&token::Data::FatArrow)),
            type_fatarrow(parser)
        ))
        .map(|m_b| match m_b {
            None => a,
            Some(b) => Type::mk_fatarrow(a, b),
        })
    })
}

/**
```text
type ::=
  type_fatarrow
```
*/
pub fn type_(parser: &mut Parser) -> ParseResult<Type<Rc<str>>> {
    type_fatarrow(parser)
}
