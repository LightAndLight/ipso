//! Parsing patterns

mod test;

use std::rc::Rc;

use crate::{
    between, choices, indent, indent_scope, keep_right, map0, optional, spanned, ParseResult,
    Parser,
};
use ipso_lex::token;
use ipso_syntax::{Pattern, Spanned};

/**
```text
pattern_record_fields ::=
  ident [',' pattern_record_fields]
  '..' ident
```
*/
pub fn pattern_record_fields(
    parser: &mut Parser,
    names: &mut Vec<Spanned<String>>,
) -> ParseResult<Option<Spanned<String>>> {
    choices!(
        // ident [',' pattern_record_fields]
        spanned!(parser, indent!(parser, Relation::Gte, parser.ident_owned())).and_then(|name| {
            names.push(name);
            optional!(keep_right!(
                indent!(parser, Relation::Gte, parser.token(&token::Data::Comma)),
                pattern_record_fields(parser, names)
            ))
            .map(|m_rest| match m_rest {
                None => None,
                Some(rest) => rest,
            })
        }),
        // '..' ident
        keep_right!(
            indent!(parser, Relation::Gte, parser.token(&token::Data::DotDot)),
            spanned!(parser, indent!(parser, Relation::Gte, parser.ident_owned())).map(Some)
        )
    )
}

/**
```text
pattern_record ::=
  '{' [pattern_record_fields] '}'
```
*/
pub fn pattern_record(parser: &mut Parser) -> ParseResult<Pattern> {
    indent_scope!(parser, {
        between!(
            indent!(parser, Relation::Eq, parser.token(&token::Data::LBrace)),
            indent!(parser, Relation::Gte, parser.token(&token::Data::RBrace)),
            {
                let mut names = Vec::new();

                pattern_record_fields(parser, &mut names)
                    .map(|rest| Pattern::Record { names, rest })
            }
        )
    })
}

/**
```text
pattern_variant ::=
  ctor ident
```
*/
pub fn pattern_variant(parser: &mut Parser) -> ParseResult<Pattern> {
    parser.ctor_owned().and_then(|name| {
        spanned!(parser, indent!(parser, Relation::Gt, parser.ident_owned()))
            .map(|arg| Pattern::Variant { name, arg })
    })
}

/**
```text
pattern ::=
  ident
  pattern_record
  pattern_variant
  char
  int
  '"' string '"'
  '_'
```
*/
pub fn pattern(parser: &mut Parser) -> ParseResult<Pattern> {
    choices!(
        spanned!(parser, parser.ident_owned()).map(Pattern::Name),
        pattern_record(parser),
        pattern_variant(parser),
        spanned!(parser, parser.char()).map(Pattern::Char),
        spanned!(parser, parser.int()).map(Pattern::Int),
        spanned!(
            parser,
            between!(
                parser.token(&token::Data::DoubleQuote),
                parser.token(&token::Data::DoubleQuote),
                parser.string()
            )
        )
        .map(|s| Pattern::String(Spanned {
            pos: s.pos,
            item: Rc::from(s.item)
        })),
        map0!(Pattern::Wildcard, parser.token(&token::Data::Underscore))
    )
}
