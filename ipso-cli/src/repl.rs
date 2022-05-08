use ipso_diagnostic::Source;
use std::io::{self, Write};
use termion::{
    cursor::DetectCursorPos,
    event::{Event, Key},
    input::TermRead,
    raw::IntoRawMode,
};

struct InputState {
    terminal_size: (u16, u16),
    cursor_row: u16,
    buffer_index: usize,
    buffer: String,
}

impl InputState {
    fn new(terminal_size: (u16, u16), cursor_row: u16) -> Self {
        InputState {
            terminal_size,
            cursor_row,
            buffer_index: 0,
            buffer: String::new(),
        }
    }

    fn reset(&mut self) {
        self.buffer_index = 0;
        self.buffer.clear();
    }

    fn cursor_left(&mut self, stdout: &mut dyn Write, count: u16) -> io::Result<()> {
        if count > 0 && self.buffer_index >= count as usize {
            write!(stdout, "{}", termion::cursor::Left(count))?;
            stdout.flush()?;
            self.buffer_index -= count as usize;
            Ok(())
        } else {
            Ok(())
        }
    }

    fn cursor_right(&mut self, stdout: &mut dyn Write, count: u16) -> io::Result<()> {
        if count > 0 && self.buffer_index + count as usize <= self.buffer.len() {
            write!(stdout, "{}", termion::cursor::Right(count))?;
            stdout.flush()?;
            self.buffer_index += count as usize;
            Ok(())
        } else {
            Ok(())
        }
    }

    fn insert(&mut self, c: char) -> io::Result<()> {
        self.buffer.push(c);
        self.buffer_index += 1;

        Ok(())
    }

    fn newline(&mut self, stdout: &mut dyn Write) -> io::Result<()> {
        let at_bottom_row = self.cursor_row == self.terminal_size.1;
        writeln!(stdout)?;
        if !at_bottom_row {
            self.cursor_row += 1;
        }
        write!(stdout, "{}", termion::cursor::Goto(1, self.cursor_row))?;
        stdout.flush()?;
        Ok(())
    }

    fn draw(&self, stdout: &mut dyn Write) -> io::Result<()> {
        let prompt = "> ";
        write!(
            stdout,
            "{}{}{}{}{}",
            termion::cursor::Goto(1, self.cursor_row),
            termion::clear::AfterCursor,
            prompt,
            self.buffer,
            termion::cursor::Goto(
                prompt.len() as u16 + self.buffer_index as u16 + 1,
                self.cursor_row
            )
        )?;
        stdout.flush()
    }
}

pub fn run() -> io::Result<()> {
    let mut stdout = io::stdout();
    let repl = ipso_repl::Repl::new();

    let ipso = r#" _                  
(_)                 
 _ _ __  ___  ___   
| | '_ \/ __|/ _ \  
| | |_) \__ \ (_) | 
|_| .__/|___/\___/
  | |               
  |_|"#;

    writeln!(
        stdout,
        "{}\n\nType :quit<ENTER> to quit.\nType :help<ENTER> to view all commands.\n",
        ipso
    )?;

    let mut stdout = stdout.into_raw_mode()?;
    let terminal_size = termion::terminal_size()?;
    let (_, cursor_row) = stdout.cursor_pos()?;
    let mut input_state = InputState::new(terminal_size, cursor_row);

    input_state.draw(&mut stdout)?;

    let stdin = io::stdin();
    for event in stdin.events() {
        if let Event::Key(key) = event? {
            match key {
                Key::Backspace => {
                    if input_state.buffer_index >= 1 {
                        input_state.buffer.remove(input_state.buffer_index - 1);
                        input_state.cursor_left(&mut stdout, 1)?;
                    }
                }
                Key::Delete => {
                    if input_state.buffer_index < input_state.buffer.len() {
                        input_state.buffer.remove(input_state.buffer_index);
                    }
                }
                Key::Left => {
                    input_state.cursor_left(&mut stdout, 1)?;
                }
                Key::Right => {
                    input_state.cursor_right(&mut stdout, 1)?;
                }
                Key::Char(c) => {
                    if c == '\n' {
                        if input_state.buffer == ":quit" {
                            break;
                        } else {
                            input_state.newline(&mut stdout)?;

                            let mut parser = ipso_parse::Parser::new(
                                Source::Interactive {
                                    label: String::from("repl"),
                                },
                                ipso_lex::Lexer::new(&input_state.buffer),
                            );
                            let parsed = ipso_parse::grammar::expr::expr(&mut parser);
                            match parser.into_parse_error(parsed.result) {
                                Err(err) => write!(stdout, "{:?}", err)?,
                                Ok(expr) => match repl.eval(expr) {
                                    Err(err) => write!(stdout, "{:?}", err)?,
                                    Ok(value) => write!(stdout, "{:?}", value)?,
                                },
                            }

                            input_state.newline(&mut stdout)?;
                            input_state.newline(&mut stdout)?;
                            input_state.reset();
                        }
                    } else {
                        input_state.insert(c)?;
                    }
                }
                _ => {}
            }
        }

        input_state.draw(&mut stdout)?;
    }

    Ok(())
}
