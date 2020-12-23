use std::io::{BufRead, BufReader, Write};
use std::iter::repeat;
use std::path::Path;
use std::str::from_utf8;
use std::{fs::File, io};

mod test;

#[derive(Debug)]
pub struct Item {
    pub pos: usize,
    pub message: String,
}

pub struct Diagnostic {
    items: Vec<Item>,
}

impl Diagnostic {
    pub fn new() -> Self {
        Diagnostic { items: Vec::new() }
    }

    pub fn item(&mut self, item: Item) {
        match self.items.binary_search_by_key(&item.pos, |i| i.pos) {
            Err(ix) => self.items.insert(ix, item),
            Ok(ix) => self.items.insert(ix + 1, item),
        }
    }

    pub fn report_string(
        line: usize,
        col: usize,
        path: &str,
        line_str: &str,
        message: &String,
    ) -> String {
        let mut result = String::new();
        let caret: String = {
            let mut caret: String = repeat(' ').take(col - 1).collect();
            caret.push('^');
            caret
        };
        let line1 = format!("{}:{}:{}: error: {}", path, line, col, message);
        let pad_amount = ((line as f32).log(10.0).floor() as usize) + 1;
        let padding: String = repeat(' ').take(pad_amount).collect();

        let line2 = format!("{} |", padding);
        let line3 = format!("{} | {}", line, line_str);
        let line4 = format!("{} | {}", padding, caret);

        result.push_str(&line1);
        result.push('\n');
        result.push_str(&line2);
        result.push('\n');
        result.push_str(&line3);
        result.push('\n');
        result.push_str(&line4);
        result
    }

    pub fn report_all(self, path: &Path) -> io::Result<()> {
        let file = File::open(path)?;
        let mut reader = BufReader::new(file);
        let mut line_str = String::new();
        let mut line: usize = 0;
        let mut offset: usize = 0;
        for item in self.items.into_iter() {
            let mut pos = item.pos;
            while item.pos >= offset {
                pos -= line_str.len();
                line_str.clear();
                match reader.read_line(&mut line_str) {
                    Err(err) => return Err(err),
                    Ok(bytes_read) => {
                        if bytes_read == 0 {
                            return Ok(());
                        } else {
                            offset = offset + bytes_read;
                            line = line + 1;
                        }
                    }
                }
            }
            let col: usize = {
                let item_bytes = &(line_str.as_bytes())[0..pos];
                from_utf8(item_bytes).unwrap().chars().count() + 1
            };
            let result = Diagnostic::report_string(
                line,
                col,
                path.to_str().unwrap(),
                line_str.trim_end_matches('\n'),
                &item.message,
            );
            match io::stderr().write(result.as_bytes()) {
                Ok(_) => {}
                Err(err) => return Err(err),
            };
            match io::stderr().write(b"\n") {
                Ok(_) => {}
                Err(err) => return Err(err),
            };
        }
        Ok(())
    }
}
