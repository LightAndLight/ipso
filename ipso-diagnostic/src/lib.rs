#[cfg(test)]
mod test;

use std::{
    collections::HashMap,
    fmt::Write as FmtWrite,
    fs::File,
    io::{self, BufRead, BufReader, Write as IoWrite},
    path::PathBuf,
    str::from_utf8,
};

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Source {
    File { path: PathBuf },
    Interactive { label: String },
}

impl Source {
    pub fn to_str(&self) -> &str {
        match self {
            Source::File { path } => path.to_str().unwrap(),
            Source::Interactive { label } => label,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Location {
    pub source: Source,
    pub offset: Option<usize>,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Message {
    pub content: String,
    pub addendum: Option<String>,
}

#[derive(Default)]
pub struct Diagnostic {
    items: Vec<Message>,
    located_items: Vec<(Location, Message)>,
}

pub struct Position {
    line: usize,
    column: usize,
}

impl Diagnostic {
    pub fn new() -> Self {
        Diagnostic {
            items: Vec::new(),
            located_items: Vec::new(),
        }
    }

    pub fn item(&mut self, location: Option<Location>, message: Message) {
        match location {
            None => self.items.push(message),
            Some(location) => {
                match self
                    .located_items
                    .binary_search_by_key(&location.offset, |i| i.0.offset)
                {
                    Err(ix) => self.located_items.insert(ix, (location, message)),
                    Ok(ix) => self.located_items.insert(ix + 1, (location, message)),
                }
            }
        }
    }

    pub fn report_error_heading(path: &str, position: Option<Position>, message: &str) -> String {
        let mut str = String::from(path);
        str.write_char(':').unwrap();
        if let Some(position) = position {
            write!(str, "{}:{}:", position.line, position.column).unwrap();
        }
        str.write_char(' ').unwrap();
        str.write_str("error: ").unwrap();
        str.write_str(message).unwrap();
        str
    }

    pub fn report_located_message(
        line: usize,
        column: usize,
        path: &str,
        line_str: &str,
        message: &Message,
    ) -> String {
        let mut result = String::new();
        let caret: String = {
            let mut caret: String = " ".repeat(column - 1);
            caret.push('^');
            caret
        };
        let line1 =
            Self::report_error_heading(path, Some(Position { line, column }), &message.content);
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
        match &message.addendum {
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
            files: &mut HashMap<Source, LocationEntry>,
            source: Source,
        ) -> io::Result<&mut LocationEntry> {
            Ok(files
                .entry(source)
                .or_insert_with_key(|location| match location {
                    Source::Interactive { label } => LocationEntry::InteractiveEntry {
                        label: label.clone(),
                    },
                    Source::File { path } => {
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

        for message in self.items {
            let result = format!("error: {}", message.content);

            match io::stderr().write(result.as_bytes()) {
                Ok(_) => {}
                Err(err) => return Err(err),
            };
            match io::stderr().write(b"\n") {
                Ok(_) => {}
                Err(err) => return Err(err),
            };
        }

        let mut source_map: HashMap<Source, LocationEntry> = HashMap::new();
        for (location, message) in self.located_items.into_iter() {
            let result = match get_entry(&mut source_map, location.source.clone()) {
                Err(err) => return Err(err),
                Ok(location_entry) => match location_entry {
                    LocationEntry::InteractiveEntry { label } => Self::report_error_heading(
                        label,
                        location.offset.map(|column| Position { line: 1, column }),
                        &message.content,
                    ),
                    LocationEntry::FileEntry(file_entry) => match location.offset {
                        None => match location.source {
                            Source::File { path } => Self::report_error_heading(
                                path.to_str().unwrap(),
                                None,
                                &message.content,
                            ),
                            Source::Interactive { label } => {
                                Self::report_error_heading(&label, None, &message.content)
                            }
                        },
                        Some(offset) => {
                            while !(file_entry.offset
                                ..file_entry.offset + file_entry.line_str.len())
                                .contains(&offset)
                            {
                                let line_size = file_entry.line_str.len();
                                file_entry.line_str.clear();
                                match file_entry.file.read_line(&mut file_entry.line_str) {
                                    Err(err) => return Err(err),
                                    Ok(bytes_read) => {
                                        file_entry.offset += line_size;
                                        file_entry.line += 1;

                                        if bytes_read == 0 {
                                            break;
                                        }
                                    }
                                }
                            }
                            let col: usize = {
                                let offset_in_line_str = offset - file_entry.offset;
                                let bytes_before_col =
                                    &(file_entry.line_str.as_bytes())[0..offset_in_line_str];
                                from_utf8(bytes_before_col).unwrap().chars().count() + 1
                            };
                            Diagnostic::report_located_message(
                                file_entry.line,
                                col,
                                location.source.to_str(),
                                file_entry.line_str.trim_end_matches('\n'),
                                &message,
                            )
                        }
                    },
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
