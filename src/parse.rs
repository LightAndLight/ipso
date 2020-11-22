use std::str::Chars;

#[derive(Debug)]
pub enum ParseError {
    ParseError,
}

pub fn parse_file(filename: &String) -> Result<(), ParseError> {
    let content: String = todo!("{:?}", filename);
    let parser: Parser = Parser::new(&content);
    parser.module()
}

pub enum Token {}

pub struct Parser<'input> {
    line: u32,
    col: u32,
    token: Option<char>,
    input: Chars<'input>,
}

impl<'input> Parser<'input> {
    fn new(input: &'input String) -> Self {
        let mut input: Chars = input.chars();
        Parser {
            line: 1,
            col: 1,
            token: input.next(),
            input,
        }
    }

    fn module(&mut self) -> Result<(), ParseError> {
        todo!()
    }
}
