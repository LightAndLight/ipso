use std::str::from_utf8;
use std::{
    collections::HashMap,
    io::{BufRead, BufReader, Write},
    path::PathBuf,
};
use std::{fs::File, io};

mod test;

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum InputLocation {
    File { path: PathBuf },
    Interactive { label: String },
}

impl InputLocation {
    pub fn to_str(&self) -> &str {
        match self {
            InputLocation::File { path } => path.to_str().unwrap(),
            InputLocation::Interactive { label } => label,
        }
    }
}

#[derive(Debug)]
pub struct Item {
    pub location: InputLocation,
    pub offset: usize,
    pub message: String,
    pub addendum: Option<String>,
}

pub struct Diagnostic {
    items: Vec<Item>,
}

impl Diagnostic {
    pub fn new() -> Self {
        Diagnostic { items: Vec::new() }
    }

    pub fn item(&mut self, item: Item) {
        match self.items.binary_search_by_key(&item.offset, |i| i.offset) {
            Err(ix) => self.items.insert(ix, item),
            Ok(ix) => self.items.insert(ix + 1, item),
        }
    }

    pub fn report_error_heading(path: &str, line: usize, col: usize, message: &str) -> String {
        format!("{}:{}:{}: error: {}", path, line, col, message)
    }

    pub fn report_string(
        line: usize,
        col: usize,
        path: &str,
        line_str: &str,
        message: &str,
        addendum: &Option<String>,
    ) -> String {
        let mut result = String::new();
        let caret: String = {
            let mut caret: String = " ".repeat(col - 1);
            caret.push('^');
            caret
        };
        let line1 = Self::report_error_heading(path, line, col, message);
        let pad_amount = ((line as f32).log(10.0).floor() as usize) + 1;
        let padding: String = " ".repeat(pad_amount);

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
        match addendum {
            None => {}
            Some(addendum) => {
                result.push('\n');
                result.push_str(addendum.as_str());
            }
        }
        result
    }

    pub fn report_all(self) -> io::Result<()> {
        struct FileEntry {
            file: BufReader<File>,
            line_str: String,
            line: usize,
            offset: usize,
        }
        enum LocationEntry {
            InteractiveEntry { label: String },
            FileEntry(FileEntry),
        }

        fn get_entry(
            files: &mut HashMap<InputLocation, LocationEntry>,
            location: InputLocation,
        ) -> io::Result<&mut LocationEntry> {
            Ok(files
                .entry(location)
                .or_insert_with_key(|location| match location {
                    InputLocation::Interactive { label } => LocationEntry::InteractiveEntry {
                        label: label.clone(),
                    },
                    InputLocation::File { path } => {
                        let file = File::open(path).unwrap();
                        let file = BufReader::new(file);
                        let line_str = String::new();
                        let line: usize = 0;
                        let offset: usize = 0;
                        LocationEntry::FileEntry(FileEntry {
                            file,
                            line_str,
                            line,
                            offset,
                        })
                    }
                }))
        }
        let mut locations: HashMap<InputLocation, LocationEntry> = HashMap::new();
        for item in self.items.into_iter() {
            let result = match get_entry(&mut locations, item.location.clone()) {
                Err(err) => return Err(err),
                Ok(location_entry) => match location_entry {
                    LocationEntry::InteractiveEntry { label } => {
                        Self::report_error_heading(label, 1, item.offset, &item.message)
                    }
                    LocationEntry::FileEntry(file_entry) => {
                        let mut pos = item.offset;
                        while item.offset >= file_entry.offset {
                            pos -= file_entry.line_str.len();
                            file_entry.line_str.clear();
                            match file_entry.file.read_line(&mut file_entry.line_str) {
                                Err(err) => return Err(err),
                                Ok(bytes_read) => {
                                    if bytes_read == 0 {
                                        return Ok(());
                                    } else {
                                        file_entry.offset += bytes_read;
                                        file_entry.line += 1;
                                    }
                                }
                            }
                        }
                        let col: usize = {
                            let item_bytes = &(file_entry.line_str.as_bytes())[0..pos];
                            from_utf8(item_bytes).unwrap().chars().count() + 1
                        };
                        Diagnostic::report_string(
                            file_entry.line,
                            col,
                            item.location.to_str(),
                            file_entry.line_str.trim_end_matches('\n'),
                            &item.message,
                            &item.addendum,
                        )
                    }
                },
            };
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
